*Draft, November 2026.*

## Summary

Pharmacokinetic models are fitted in patients whose physiology is, for
the most part, normal. The clinician then gives the drug to a patient whose
liver, kidneys, circulation or temperature are not. This chapter sets out
how stanpumpR carries a model from the one to the other, and the evidence
on which each step rests. Clearance is treated as the product of blood flow
and an extraction ratio, so that a change in organ function and a change in
organ blood flow act through different terms of the well-stirred model.
Cardiac output changes hepatic and renal blood flow, distribution and the
central volume, and the rate of equilibration with the effect site.
Body temperature changes the capacity of every route of elimination, and
the solubility and potency of the inhaled anaesthetics. For temperature the
chapter draws on more than sixty papers read in full. Cytochrome P450
oxidation, glucuronidation, glomerular filtration, two esterases and Hofmann
elimination all slow by about 6 to 9% for each degree Celsius between 37
and 28 °C, which justifies one factor for every route, 6.4% per degree,
taken from the one human study that spans that range. The one direct
measurement of the hydrogen-abstraction step of cytochrome P450 oxidation
suggests that this step contributes only part of that sensitivity, and
quantum tunnelling does not alter it at body temperature. Where the
evidence is weak, or comes only from animals, the chapter says so.

## 1. Introduction

A population pharmacokinetic model describes the patients in whom it was
fitted. Its covariates (weight, age, sex, renal function) extend it to
other patients only along the axes the study happened to vary. Hepatic
failure, low cardiac output and hypothermia are rarely among them, yet
anaesthesia and intensive care bring them together in the same patient,
often within the same hour.

There is a long literature on each effect. The physiological approach to
hepatic clearance is fifty years old (Wilkinson and Shand 1975). The
effect of hypothermia on drug disposition has been reviewed repeatedly,
first by Ballard (1974) and since then, among others, by Tortorici and
colleagues (2007), van den Broek and colleagues (2010), Zhou and Poloyac
(2011), Anderson and colleagues (2016), Lutz and colleagues (2020), Smits
and colleagues (2020) and Leys and colleagues (2023). Those reviews
catalogue the changes drug by drug, and they agree that clearance falls
in the cold. They also agree that the effect of temperature is hard to
separate from the effects of the illness that brought the patient to be
cooled, and from the fall in cardiac output that accompanies cooling.

The purpose of this chapter is narrower and, we hope, complementary. It
asks what single, quantitative rule can carry a pharmacokinetic model from
normal physiology to abnormal physiology, which parts of the model each
physiological change should touch, and what the evidence says about the
size of each effect. The rules described here are the ones stanpumpR
applies when the Physiology sliders are moved
([Physiology sliders](help:models/physiology)), so each statement can be
examined in a running simulation.

Two principles run through the chapter. First, blood flow and metabolic
capacity are different things, and they act on different terms of the
model. A change in the one must not be counted as a change in the other.
Second, only clearance is a physiological quantity
([Comparing sources](help:models/overview)). Volumes and rate
constants are constructs that relate dose to concentration, so a
physiological change can be mapped onto them only by an explicit
assumption, which is stated wherever it is made.

## 2. Clearance as flow and capacity

### 2.1 The well-stirred model

The clearance of an organ is the blood flow through it, Q, times the
fraction of drug removed in one pass, the extraction ratio E. In the
well-stirred model the extraction ratio follows from the organ's intrinsic
clearance, CL<sub>int</sub>, its capacity to remove unbound drug in the
absence of any limit by flow:

E = CL<sub>int</sub> / (Q + CL<sub>int</sub>), and
CL = Q CL<sub>int</sub> / (Q + CL<sub>int</sub>)

(Wilkinson and Shand 1975). If blood flow changes by a factor *q* and
capacity by a factor *c*, the new clearance is

CL′ / CL = *q c* / (*q* (1 − E) + *c* E). (1)

Equation 1 needs only one property of the drug, its extraction ratio at
normal physiology. It contains the two limiting cases on which clinical
intuition is built. For a drug with a high extraction ratio, such as
propofol (E about 0.87), CL′/CL approaches *q*: clearance follows blood flow
and is nearly indifferent to capacity. For a drug with a low extraction
ratio, such as alfentanil (E about 0.28), CL′/CL approaches *c*: clearance
follows capacity, as in hepatic disease or enzyme induction, and is nearly
indifferent to flow.

### 2.2 Partitioning a drug's clearance

Most drugs are cleared by more than one route. stanpumpR divides each
drug's clearance into a renal share, a hepatic share, and the remainder,
which is elimination outside the liver and kidney (plasma and tissue
esterases, Hofmann elimination). Each share is taken from a published
source, and a drug for which a share or the hepatic extraction ratio could
not be found in a source read by us is left unchanged by the sliders. The
total change in clearance is the sum of the shares, each changed by its own
rule:

CL′ / CL = f<sub>R</sub> R + f<sub>H</sub> H + (1 − f<sub>R</sub> − f<sub>H</sub>) X, (2)

where f<sub>R</sub> and f<sub>H</sub> are the renal and hepatic shares,
H is Equation 1 applied to the liver, R is the change in renal clearance
and X the change in the remainder.

### 2.3 The kidney

Most renally cleared drugs are filtered, or filtered and secreted, without
being metabolised by the kidney. Their renal clearance is taken to change
in proportion to renal function, R = *r*, independent of renal blood flow
over the range in which glomerular filtration is autoregulated. A
measured serum creatinine, where a model uses one, already carries the
patient's renal function, and the slider acts on top of it.

A few drugs are metabolised by the kidney. Propofol is the important
example: its total clearance exceeds hepatic blood flow, and Takizawa and
colleagues (2005) measured a renal extraction ratio of 0.58, the kidney
accounting for about 27% of total clearance (see also Hiraoka 2005). For
such a drug the renal share follows Equation 1, with renal blood flow in
place of hepatic blood flow and renal function in place of hepatic
capacity.

### 2.4 Hepatic first pass

For a drug given by mouth whose bioavailability is limited by hepatic first
pass, a change in E changes the bioavailability. If F is the bioavailable
fraction at normal physiology and E′ the extraction ratio after a change,
F′ = F (1 − E′) / (1 − E), never more than 1. Morphine is the drug in the
library for which this matters most.

## 3. Cardiac output

Cardiac output acts on a pharmacokinetic model in three places: the blood
flow of the clearing organs, distribution, and the effect site.

### 3.1 Organ blood flow

Hepatic and renal blood flow are each taken as a fixed share of cardiac
output, so that the factor *q* in Equation 1 is the cardiac output relative
to normal. This is a simplification. In the hypovolaemic or vasoconstricted
patient the splanchnic circulation is reduced more than cardiac output is,
so that the assumption understates the fall in hepatic blood flow, and in
sepsis it may overstate it.

### 3.2 Distribution and the central volume

The intercompartmental clearances of a mammillary model describe the
transfer of drug between plasma and the tissues. If that transfer is
limited by delivery, the intercompartmental clearances follow cardiac
output, and stanpumpR scales them in proportion to it.

The central volume is a construct, not a space, and its relationship to
cardiac output is an assumption rather than a measurement. Its direction is
physiological. In a low output state a bolus is diluted in less blood
during its first passage and early concentrations are higher, which a
mammillary model represents as a smaller central volume. stanpumpR scales the central volume in proportion to cardiac
output. The exponent, 1, has not been fitted to data.

### 3.3 The effect site

The effect of cardiac output on the effect site is set out, with its
derivation and a simulation, on the
[Physiology sliders](help:models/physiology) page. The comparison that
settles it is between two circulations that deliver the same arterial
concentration curve. With less brain blood flow, less drug reaches the
brain each minute, equilibration takes longer, and ke0 falls. For a drug
whose entry into the brain is limited by delivery, ke0 falls in proportion
to brain blood flow; for a drug limited by the blood–brain barrier
(morphine), flow drops out and ke0 is unchanged.

Within autoregulation, brain blood flow follows cardiac output only in
part. Three studies give the exponent of that relation: 0.47 (Levine and
colleagues 1994, lower-body negative pressure), about 0.35 (Ogoh and
colleagues 2005, cardiac output lowered and raised at constant pressure
and arterial carbon dioxide) and 0.33 (Vlastra and colleagues 2021, cerebral
blood flow before and after aortic valve replacement). stanpumpR takes
ke0 as its normal value times (cardiac output)<sup>0.35</sup>. The evidence
is not strong: on cardiopulmonary bypass at constant pressure, halving
pump flow did not change cerebral blood flow (Cook and colleagues 1997), and
a systematic review judged the published studies too weak to establish
the coupling (Castle-Kirszbaum and colleagues 2022). Below the
autoregulatory range brain flow becomes pressure-passive and falls further.

## 4. Body temperature

### 4.1 The rule

stanpumpR multiplies the capacity of every route of elimination by

f(T) = exp(−θ (37 − T)), θ = −ln(1 − 0.0637) = 0.0658 per °C, (3)

so that each degree below 37 °C removes 6.37% of the capacity that
remains, leaving 55% at 28 °C. The factor acts on the hepatic capacity
*c* in Equation 1, on renal clearance, and on the remainder X in
Equation 2. It does not act on blood flow, which belongs to the cardiac
output term. Volumes, intercompartmental clearances and ke0 are left
unchanged.

The form and the slope are those of Michelsen and colleagues (2001), who
fitted remifentanil clearance in 68 patients undergoing coronary artery
surgery on cardiopulmonary bypass as CL × exp(θ (T − 37)). Their
temperatures ran from 20 to 35 °C, with 16 patients below 28 °C, so theirs
is the one human study that spans the whole range stanpumpR offers (28 to
37 °C). Remifentanil is cleared by nonspecific esterases in blood and
tissue, with almost no hepatic or renal component, so the fit describes
an enzymatic capacity largely free of the confounding effect of organ
blood flow.

### 4.2 The evidence, route by route

The case for one factor rests on the agreement of the routes. Table 1
gathers the studies we read in full, expressed as the fractional fall in
clearance (or in the rate of an enzyme reaction) per degree near body
temperature. Where a source reported a half-life or an activity at two
temperatures, the per-degree figure is the geometric rate between them.

**Table 1.** Temperature dependence of elimination, by route.

| Route | Drug or reaction | Preparation | Range, °C | Fall per °C | Source |
|---|---|---|---|---|---|
| Esterases (blood, tissue) | Remifentanil | Patients on bypass | 20–35 | 6.4% | Michelsen 2001 |
| Esterases | Remifentanil | Patients on bypass, within-patient | 28–37 | about 4% | Russell 1997 |
| Red-cell arylesterase | Esmolol | Human whole blood | 4–37 | 7.5% | Melendez 1990 |
| Butyrylcholinesterase | Mivacurium (two isomers) | Human serum | 28–37 | 6.3–8.4% | Gruber 2002 |
| Hofmann elimination and esterases | Atracurium | Human plasma | 23–37 | 7.2% | Merrett 1983 |
| CYP3A | Midazolam | Volunteers | 34.8–37.8 | 11.6% | Hostler 2010 |
| CYP3A4 | Ethylmorphine demethylation | Pig liver microsomes | 26–38 | 6% | Fritz 2005 |
| CYP (general) | — | Minipig liver microsomes | 33–38 | 8.9% | Stroe 2024 |
| CYP (by isoform) | Midazolam, diclofenac, dextromethorphan, chlorzoxazone | Rats, in vivo | 32.5–37.9 | 0–8.5% | Zhou 2011 |
| CYP2E1 | Chlorzoxazone | Rats after cardiac arrest | 30–37 | about 11% | Tortorici 2006 |
| CYP (microsomal oxidation) | Ethanol, dimethylnitrosamine, benzphetamine | Rat liver microsomes | 20–30 | 6.2–10.7% | Puntarulo 1989 |
| CYP (intrinsic clearance, Vmax/Km) | Propranolol, verapamil | Rat liver S-9 | 26–37 | 4.5%, 6.5% | McAllister 1980 |
| Glucuronidation | — | Rat liver microsomes | 26–56 | about 10% | Norling 1974 |
| Glucuronidation and CYP together | Morphine, midazolam | Cooled neonates | 33.5–36.5 | 6.8% (5.2–8.3) | Favié 2020 |
| Hepatic carrier uptake | Vecuronium | Patients | 34.3–38.0 | 11.3% | Caldwell 2000 |
| Glomerular filtration | Amoxicillin | Cooled neonates | 33.5–37 | 6.9% | Bijleveld 2018 |
| Glomerular filtration | Ceftazidime | Cooled neonates | 33.5–37 | 8.3% | van der Veer 2023 |
| Glomerular filtration | Doxacurium | Patients on bypass | 26–36 | about 6.3% | Asokumar 1998 |
| Glomerular filtration | Gentamicin | Cooled neonates, against controls | 33.5–37 | about 8% | Mark 2013 |

The central tendency is 6 to 9% per degree, and every class of
elimination falls within it: esterases, oxidation by cytochrome P450,
glucuronidation, filtration, and the chemical breakdown of atracurium. The
values outside it are informative. The steepest come from studies in which
clearance may have fallen for a second reason, or in which the range was
narrow. Hostler's fit spans only 34.8 to 37.8 °C, and the study did not
separate enzymatic capacity from hepatic blood flow. Caldwell's vecuronium
is taken up into the liver by a carrier rather than metabolised by
cytochrome P450, and its renal clearance fell as much as its hepatic.
Tortorici's rats had suffered cardiac arrest, which itself
suppresses CYP2E1 and CYP3A through interleukin-6 (Tortorici 2009). The
shallowest come from tissue preparations in which the drug's uptake into
the slice, rather than metabolism, may limit the rate (Mortensen and Dale
1995, whose oxazepam glucuronidation gave 2.2% per degree).

Several clinical studies found no effect of temperature, and each has a
reason in its design. Bastiaans and colleagues (2013) found midazolam
clearance unchanged in adults cooled to 33 °C after cardiac arrest.
Petros and colleagues (1995) found alfentanil clearance unchanged after
hypothermic bypass, but sampled for 8 hours, mostly after rewarming.
Bijleveld and colleagues (2016) found gentamicin clearance unchanged
during cooling in neonates, which they attributed to the delayed recovery
of renal function after asphyxia. Shellhaas and colleagues (2013) found no
effect on phenobarbital, whose half-life is long against 72 hours of
cooling.

The single factor was chosen over separate factors for each class of
enzyme because the classes do not differ beyond the scatter of the
studies, and over a factor on total clearance because the steepest human
values carry falls in blood flow that the cardiac output term already
represents.

### 4.3 Flow and capacity together: propofol

Propofol shows why the factor must act on capacity and not on clearance.
It has a hepatic extraction ratio of about 0.87 and a renal share of 27%
with a renal extraction ratio of 0.58, so its clearance is limited by
blood flow at both organs. Applied to capacity alone through Equation 1,
the temperature factor leaves propofol with 82% of its normal clearance
at 28 °C. Bartošová and colleagues (2026), fitting propofol in adults
cooled to 16–20 °C for circulatory arrest, found clearance at 42% of
normal at 28 °C, and noted that clearance was 2.3 times higher during
rewarming than during cooling at the same temperature, which they read as
an effect of flow. With cardiac output set to 0.4 of normal as well,
stanpumpR gives 44%. The two mechanisms together reproduce the
observation; neither alone does. This is the reason the two sliders are
separate, and the clinician sets each.

### 4.4 The shape of the curve

Enzyme kinetics predicts that the fall should steepen as temperature
drops. Hobbs and colleagues (2013) and Arcus and colleagues (2016) showed
that the heat capacity of activation of enzyme-catalysed reactions is
negative, −1 to −12 kJ/mol/K across the enzymes they examined, which bends
the Arrhenius plot downward at low temperature. Held to 6.6% per degree at
37 °C, such curvature would leave 46 to 29% of capacity at 28 °C, against
the exponential's 55%.

The drug-metabolising data within the slider's range show no such
curvature. Fritz and colleagues (2005) found 6% per degree for pig CYP3A4
on both 38–32 °C and 32–26 °C, and Michelsen's exponential fitted
remifentanil from 20 to 35 °C. The curvature appears lower down. Esmolol's
breakdown in blood falls 7.0% per degree from 37 to 25 °C but averages
8.9% per degree from 37 to 15 °C (Melendez 1990), and mivacurium's falls
similarly more steeply below 28 °C (Gruber 2002). Several microsomal
Arrhenius plots break near 20 °C (Puntarulo 1989; Masson and Balny 1986).
Within 28 to 37 °C an exponential is adequate; below 28 °C, which
stanpumpR does not offer, it would overstate the remaining capacity.

### 4.5 Why enzymes slow in the cold

Because cytochrome P450 oxidation so often begins with abstraction of a
hydrogen atom, it is natural to ask whether that step sets the
temperature dependence, and whether quantum-mechanical tunnelling, which
lets the hydrogen pass through rather than over the energy barrier, makes
it less sensitive to temperature than a classical reaction.

The evidence suggests not. In human liver microsomes the breaking of the
carbon–hydrogen bond limits the rate of some reactions (7-ethoxycoumarin
O-deethylation by CYP1A2, isotope effects 7.3–8.1) but not others (CYP2E1,
whose isotope effects are attenuated to 2–4), so the whole-enzyme rate
carries the temperature dependence of several steps (Kim 2006; Guengerich
2017). Tunnelling in P450 enzymes is real but modest. It was first shown
experimentally in an engineered bacterial enzyme at a single temperature
(Yang 2017). A gas-phase calculation for CYP3A4 put the tunnelling
coefficient near 3 at 300 K (Zhang and Lin 2009), and calculations that
include the protein find the enzyme's electric field widening the barrier
and suppressing tunnelling at body temperature, with the isotope effect
rising steeply only below about 250 K (Kalita 2025). The one measured
barrier for hydrogen abstraction by ordinary hydrogen, in the bacterial
peroxygenase OleT over 4–21 °C, corresponds to only about 2% per degree
(Dutra 2022). Its reported barriers, pre-exponential ratio and isotope
effect do not form one consistent set, and the reason is not known, so the
figure should be read with caution.

If the chemistry of hydrogen abstraction contributes only part of the 6
to 9% per degree seen in whole systems, the rest presumably lies in the
other steps of the catalytic cycle (electron transfer, oxygen and
substrate binding, product release) and in the motion of the protein.
That is a hypothesis these studies do not test. For the model it does not
matter: the factor rests on the measured rates of whole systems, not on
the chemistry of one step.

### 4.6 After rewarming

Some effects of hypothermia outlast it. In goats cooled to 22–23 °C, serum
cholinesterase was unchanged during cooling and fell by about 30% in the
week after rewarming, recovering only by the fourteenth day, which the
authors attributed to suppressed hepatic synthesis (Timet 1977). Cardiac
arrest, often the reason for therapeutic cooling, suppresses drug
metabolism for days through inflammation, and cooling attenuates that
suppression (Tortorici 2009). Concentrations of remifentanil, propofol and
midazolam at constant infusion rates fell by 16, 12 and 11% when patients
were rewarmed from 33 to 37 °C (Bjelland 2014). stanpumpR models none of
these after-effects: its factor returns to 1 at 37 °C.

## 5. The inhaled anaesthetics

The inhaled anaesthetics need no translation from physiology to
parameters, because their model is already physiological. Cardiac output,
its division among the vessel-rich group, muscle and fat, and the
partition coefficients of each tissue are the model's own parameters. The
liver, the kidney and the gut do not apply: the agents leave by the
lungs.

### 5.1 Cardiac output

Eger and Shafer describe two effects. A lower cardiac output carries less
agent away from the lungs, so the alveolar concentration approaches the
inspired concentration faster; the effect is large for a soluble agent
and small for a poorly soluble one. And in shock the circulation narrows
to the heart and brain, where flow is protected. stanpumpR gives the
brain and heart, 16% of resting cardiac output in the reference man (12%
and 4%; Williams and Leggett 1989), a flow that follows cardiac output to
the power 0.35, the same autoregulated relation used for ke0, and shares
what remains among the other tissues in their normal proportions. At a
tenth of normal output the brain and heart receive 7% of normal output,
nearly three quarters of all there is. So that the brain does not take
the slow filling of a lumped vessel-rich group, the brain and heart become
a compartment of their own whenever cardiac output departs from normal,
each part keeping the group's normal flow per litre, so that at normal
output the model is unchanged and its agreement with GasMan holds.

### 5.2 Body temperature

Hypothermia changes the inhaled agents in two directions. Solubility
rises: the blood:gas partition coefficients of desflurane, sevoflurane and
isoflurane rise by 3.0, 3.0 and 3.8% per degree as human blood cools from
37 to 17 °C (Zhou and colleagues 2002), and tissue:gas coefficients rise in
proportion, leaving tissue:blood unchanged (Zhou and Liu 2001). More agent
is carried from the lungs, so the alveolar concentration rises more
slowly. Potency rises too: isoflurane MAC in children falls by 4.6% of its
normothermic value per degree (Liu and colleagues 2001, from their own
regression; their text gives 5.1%). For nitrous oxide the only
measurement in any species is in rats, 2.1% per degree (Antognini and
colleagues 1994), applied to patients for want of human data, with
solubility rising 2.3% per degree in blood and aqueous tissue and a
predicted 1.1% in fat (Allott and colleagues 1973).

Because MAC falls faster than solubility rises, a given inspired
concentration is a deeper anaesthetic in the hypothermic patient; dialled
to the same multiple of MAC, induction is slower, most for the soluble
agents. Cardiac output and ventilation also fall in hypothermia, but each
has its own control and is not coupled to temperature, because fever and
cooling raise and lower cardiac output by different mechanisms.

## 6. Limitations

Each step above is a simplification, and several rest on weak evidence.

- **Organ blood flow as a fixed share of cardiac output.** Splanchnic and
  renal flow are redistributed in shock, sepsis and vasoconstriction.
- **The central volume.** Its scaling with cardiac output is an assumption
  in direction and size.
- **The coupling of brain flow to cardiac output.** The exponent of 0.35
  rests on three small studies and is contradicted on bypass.
- **One temperature factor for every route.** The routes agree within
  their scatter, but the scatter is wide, and individual drugs may differ.
- **The range.** The factor is applied only between 28 and 37 °C; below
  that the measured curves steepen.
- **What is not modelled.** Protein binding, which changes with
  hemodilution on bypass (remimazolam's unbound fraction rose threefold
  when bypass began; Inoue and colleagues 2026); the suppression of drug
  metabolism by cardiac arrest and inflammation; the effects that outlast
  rewarming; and the pharmacodynamic effects of temperature, apart from
  the MAC of the inhaled agents.
- **The evidence base.** Much of it comes from animals, from tissue
  preparations, or from patients whose illness and whose cooling cannot
  be separated. Papers by Gideon Koren were not used.

## 7. Conclusions

Carrying a pharmacokinetic model from normal to abnormal physiology needs
little more than one distinction, between the blood flow to a clearing
organ and the organ's capacity, and the well-stirred model to combine
them. Cardiac output acts on flow, on distribution and on the effect site;
organ function and temperature act on capacity. For temperature, the
evidence across every route of elimination agrees on about 6 to 9% per
degree between 37 and 28 °C, and one factor, taken from the human study
that spans that range, describes it. The agreement is the finding: the
temperature sensitivity of drug elimination appears to be a property of
enzymatic capacity in general, not of any one enzyme.

## References

Allott PR, Steward A, Flook V, Mapleson WW. *Br J Anaesth* 1973;45:294-300. doi:10.1093/bja/45.3.294

Anderson KB, Poloyac SM, Kochanek PM, Empey PE. *Ther Hypothermia Temp Manag* 2016;6:169-79. doi:10.1089/ther.2016.0003

Antognini JF, Lewis BK, Reitan JA. *Anesth Analg* 1994;79:980-2. doi:10.1213/00000539-199411000-00027

Arcus VL, et al. *Biochemistry* 2016;55:1681.

Asokumar B, et al. *Can J Anaesth* 1998;45:515.

Ballard BE. Pharmacokinetics and temperature. *J Pharm Sci* 1974;63:1345-58. doi:10.1002/jps.2600630903

Bartošová T, Šíma M, Prskavec T, et al. *Biomed Pharmacother* 2026;200:119523. doi:10.1016/j.biopha.2026.119523

Bastiaans DE, et al. *Int J Clin Pharm* 2013;35:210.

Bijleveld YA, et al. *Br J Clin Pharmacol* 2016;81:1067.

Bijleveld YA, Mathôt R, van der Lee JH, et al. *Clin Pharmacol Ther* 2018;103:458-67. doi:10.1002/cpt.748

Bjelland TW, et al. *Drug Metab Dispos* 2013;41:214.

Bjelland TW, et al. *Acta Anaesthesiol Scand* 2014;58:709.

Caldwell JE, et al. *Anesthesiology* 2000;92:84.

Castle-Kirszbaum M, Parkin WG, Goldschlager T, Lewis PM. *J Neurosurg Anesthesiol* 2022;34:352-63. doi:10.1097/ANA.0000000000000768

Cook DJ, Proper JA, Orszulak TA, Daly RC, Oliver WC. *J Cardiothorac Vasc Anesth* 1997;11:415-9. doi:10.1016/s1053-0770(97)90047-1

Dutra M, Amaya JA, McElhenney S, et al. *J Phys Chem B* 2022;126:3493-504. doi:10.1021/acs.jpcb.1c10280

Eger EI II, Shafer SL. The evolution of pharmacokinetics. Manuscript, 2017.

Favié LMA, et al. *PLoS One* 2019;14:e0211910.

Favié LMA, et al. *Clin Pharmacol Ther* 2020;108:1098.

Fritz HG, et al. *Anesth Analg* 2005;100:996.

Gruber M, Lindner R, Prasser C, Wiesner G. *Anesth Analg* 2002;95:397-9. doi:10.1097/00000539-200208000-00029

Guengerich FP. Kinetic deuterium isotope effects in cytochrome P450 reactions. *Methods Enzymol* 2017;596:217-38. doi:10.1016/bs.mie.2017.06.036

Hiraoka H, Yamamoto K, Miyoshi S, et al. *Br J Clin Pharmacol* 2005;60:176-82. doi:10.1111/j.1365-2125.2005.02393.x

Hobbs JK, et al. *ACS Chem Biol* 2013;8:2388.

Hostler D, et al. *Drug Metab Dispos* 2010;38:781.

Inoue M, Ueshima S, Kakumoto M, et al. *J Cardiothorac Vasc Anesth* 2026;40:1138-46. doi:10.1053/j.jvca.2025.12.025

Jacobs JR, Croughwell ND, Goodman DK, White WD, Reves JG. *J Clin Pharmacol* 1993;33:360-5. doi:10.1002/j.1552-4604.1993.tb04670.x

Kalita S, Danovich D, Shaik S. *J Am Chem Soc* 2025;147:35576-86. doi:10.1021/jacs.5c10488

Kim KH, Isin EM, Yun CH, Kim DH, Guengerich FP. *FEBS J* 2006;273:2223-31. doi:10.1111/j.1742-4658.2006.05235.x

Leys K, Stroe MS, Annaert P, et al. *Expert Opin Drug Metab Toxicol* 2023;19:461-77. doi:10.1080/17425255.2023.2237412

Levine BD, Giller CA, Lane LD, Buckey JC, Blomqvist CG. *Circulation* 1994;90:298-306. doi:10.1161/01.cir.90.1.298

Liu M, Hu X, Liu J. *Anesthesiology* 2001;94:429-32. doi:10.1097/00000542-200103000-00011

Lutz IC, Allegaert K, de Hoon JN, Marynissen H. *BMJ Paediatr Open* 2020;4:e000685. doi:10.1136/bmjpo-2020-000685

Mark LF, et al. *Ther Drug Monit* 2013;35:217.

Masson P, Balny C. *Biochim Biophys Acta* 1986;874:90-8. doi:10.1016/0167-4838(86)90106-8

McAllister RG, Tan TG. *Pharmacology* 1980;20:95-100. doi:10.1159/000137349

Melendez JA, Stone JG, Delphin E, Quon CY. *J Cardiothorac Anesth* 1990;4:704-6. doi:10.1016/s0888-6296(09)90007-2

Merrett RA, et al. *Br J Anaesth* 1983;55:61.

Michelsen LG, Holford NH, Lu W, Hoke JF, Hug CC, Bailey JM. *Anesth Analg* 2001;93:1100-5. doi:10.1097/00000539-200111000-00006

Mortensen ME, Dale O. *Acta Anaesthesiol Scand* 1995;39:199.

Norling A, Hänninen O. *Biochim Biophys Acta* 1974;350:183.

Ogoh S, Brothers RM, Barnes Q, et al. *J Physiol* 2005;569:697-704. doi:10.1113/jphysiol.2005.095836

Petros A, et al. *Anesth Analg* 1995;81:458.

Puntarulo S, Cederbaum AI. *Arch Biochem Biophys* 1989;269:569-75. doi:10.1016/0003-9861(89)90142-2

Russell D, et al. *Br J Anaesth* 1997;79:456.

Shellhaas RA, et al. *Pediatr Crit Care Med* 2013;14:194.

Smits A, et al. *Front Pharmacol* 2020;11:587.

Stroe MS, et al. *Basic Clin Pharmacol Toxicol* 2024;135:620.

Takizawa D, Hiraoka H, Goto F, Yamamoto K, Horiuchi R. *Anesthesiology* 2005;102:327-30. doi:10.1097/00000542-200502000-00014

Timet D, Mitin V, Tranger M. *Cryobiology* 1977;14:516-8. doi:10.1016/0011-2240(77)90017-7

Tortorici MA, et al. *Crit Care Med* 2006;34:785.

Tortorici MA, Kochanek PM, Poloyac SM. *Crit Care Med* 2007;35:2196-204. doi:10.1097/01.ccm.0000281517.97507.6e

Tortorici MA, et al. *Crit Care Med* 2009;37:263.

van den Broek MPH, Groenendaal F, Egberts ACG, Rademaker CMA. *Clin Pharmacokinet* 2010;49:277-94. doi:10.2165/11319360-000000000-00000

van der Veer MAA, de Haan TR, Franken LGW, et al. *Antimicrob Agents Chemother* 2023;67:e0170722. doi:10.1128/aac.01707-22

Vlastra W, van Nieuwkerk AC, Bronzwaer AGT, et al. *J Am Geriatr Soc* 2021;69:494-9. doi:10.1111/jgs.16882

Wang Q, Sheng X, Horner JH, Newcomb M. *J Am Chem Soc* 2009;131:10629-36. doi:10.1021/ja9031105

Wilkinson GR, Shand DG. *Clin Pharmacol Ther* 1975;18:377-90.

Williams LR, Leggett RW. *Clin Phys Physiol Meas* 1989;10:187-217. doi:10.1088/0143-0815/10/3/001

Yang CL, Lin CH, Luo WI, et al. *Chemistry* 2017;23:2571-82. doi:10.1002/chem.201603956

Zhang Y, Lin H. *J Phys Chem A* 2009;113:11501-8. doi:10.1021/jp901850c

Zhou J, Poloyac SM. *Expert Opin Drug Metab Toxicol* 2011;7:803-16. doi:10.1517/17425255.2011.574127

Zhou J, et al. *Drug Metab Dispos* 2011;39:2209.

Zhou JX, Liu J. *Anesth Analg* 2001;93:234-8. doi:10.1097/00000539-200107000-00047

Zhou J, Liu Y, Liu J. *Chin Med J (Engl)* 2002;115:1014-9.
