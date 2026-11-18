The **Physiology** panel in the left-hand column asks *what if*: what if hepatic
function were half of normal, the kidneys shut down, or cardiac output fell
by two thirds? It has four sliders of organ function, and a fifth for body
temperature (see *Body temperature* below). Each of the four is a multiple of
normal function, on a log scale from a tenth (0.1) to ten times (10). All four
start at 1, which is normal and changes nothing. **Reset to normal** puts them
back, and the temperature to 37 °C.

For the reasoning behind each rule and the evidence for it, see
[Physiology and pharmacokinetics: the evidence](help:models/physiology-and-pharmacokinetics).

## Sliders and covariates

Where a published model includes a feature of the patient (weight, age,
sex, serum creatinine, a genotype), that feature is a **covariate** of the
model, measured in the patients the model was fitted to, and it is entered
in the **Patient Profile**. The sliders are for something else. They explore
how a change in organ function or body temperature might alter a drug's
pharmacokinetics when no published model describes it, using the
fundamental relationships between physiology and pharmacokinetics set out
below. They are a *what if*, not a model fitted in patients with the
condition. For that reason the panel opens on a short statement to this
effect, and the sliders appear once it has been read (**I understand: show
the sliders**). A link or a scenario that changes a slider shows them at
once.

Do not enter one change twice. If a drug's model reads the serum
creatinine, entering a raised creatinine already lowers its renal
clearance; lowering the **Renal function** slider as well counts the
impairment twice.

The sliders change each drug's pharmacokinetic **parameters**, once, before
the simulation runs. As with any covariate, the parameters then stay fixed
for the whole simulation. They do not change over time.

## The liver: flow-limited and capacity-limited drugs

Hepatic clearance is liver blood flow times the extraction ratio,
CL<sub>H</sub> = Q × E. In the well-stirred model (Wilkinson and Shand, 1975)
the extraction ratio comes from the liver's intrinsic clearance,
CL<sub>int</sub>, the metabolic capacity V<sub>m</sub>/K<sub>m</sub>:

E = CL<sub>int</sub> / (Q + CL<sub>int</sub>),
CL<sub>H</sub> = Q × CL<sub>int</sub> / (Q + CL<sub>int</sub>).

Change hepatic blood flow by a factor *q* and metabolic capacity by a factor
*l*. Hepatic clearance then changes by

CL<sub>H</sub>′ / CL<sub>H</sub> = *q l* / (*q* (1 − E) + *l* E),

which needs nothing but the drug's extraction ratio E at normal physiology.

- A drug with a **high** extraction ratio (propofol, E 0.87) is **flow
  limited**. The liver can metabolize far more than reaches it, so its
  clearance follows hepatic blood flow and hardly changes with metabolic
  capacity.
- A drug with a **low** extraction ratio (alfentanil, E 0.28) is **capacity
  limited**. Its clearance follows metabolic capacity, as in liver disease or
  enzyme induction, and hardly changes with hepatic blood flow.

The **Liver function** slider is *l*. The **Cardiac output** slider is *q*,
because liver blood flow is taken as a fixed share of cardiac output (as is
renal blood flow, below). For a
drug given by mouth whose bioavailability includes a hepatic first pass
(morphine), the new extraction ratio E′ = *l* E / (*q* (1 − E) + *l* E) also
changes the oral bioavailability, F′ = F (1 − E′)/(1 − E), never above 1.

## The kidneys

**Renal function** scales only the renal share of a drug's clearance. Each drug's clearance is split into a renal
share, a hepatic share, and the rest. The rest is elimination outside the liver
and kidneys, such as plasma and tissue esterases (remifentanil) and Hofmann
elimination (cisatracurium), and no organ slider changes it (body temperature
does: see below). Setting renal function
to 0.1 removes 90% of the renal share and nothing else.

Some drugs are **metabolized** by the kidney, not just filtered. Propofol is
the main example. Its total clearance is greater than hepatic blood flow, and
the kidneys are the likely reason: they metabolize about 27% of it, with a
renal extraction ratio of 0.58 measured against renal blood flow (Takizawa
2005). The renal share of such a drug follows the same well-stirred rule as
the liver, with renal blood flow a fixed share of cardiac output:
CL<sub>R</sub>′/CL<sub>R</sub> = *q r* / (*q* (1 − E<sub>R</sub>) + *r* E<sub>R</sub>).
So both renal function and cardiac output change it. A renal share with no
renal extraction ratio is filtration of unchanged drug, which changes in
proportion to renal function. A Patient Profile serum creatinine still
changes the drugs whose models read it, and the slider acts on top of that.

## Cardiac output and distribution

Besides hepatic blood flow, cardiac output scales two things:

- **The intercompartmental clearances** (Q2 and Q3), because distribution to
  the tissues is carried by blood flow.
- **The central volume** (V1), in the same direction. With a low cardiac
  output, a bolus mixes into less blood on its first pass, so early
  concentrations are higher, which a mammillary model expresses as a smaller
  V1. The size of this effect is an assumption, V1 in proportion to cardiac
  output, and is not fitted to data.

## The effect site in low cardiac output

**ke0** is the rate at which the effect site equilibrates with blood. It is set
mainly by the drug's relative solubility in blood and brain, not by brain
blood flow; if it were set by flow, every drug acting in the brain would share
one ke0. A drug that is poorly soluble in brain equilibrates quickly, because
almost nothing has to cross before equilibrium is reached. A highly soluble
drug equilibrates slowly, because a great deal has to cross.

Cardiac output still matters. Picture the same arterial concentration in
normal physiology and in shock. With less brain blood flow, less drug is
delivered each minute, so the effect site rises more slowly. That can be made
exact. If the brain is one well-stirred tissue fed by arterial blood,

V dC<sub>brain</sub>/dt = Q E (C<sub>arterial</sub> − C<sub>brain</sub>/λ), with
E = 1 − exp(−PS/Q),

where V is the brain's volume, Q its blood flow, λ the brain:blood partition
coefficient (the solubility) and PS the permeability-surface product of the
blood-brain barrier. Writing the effect site as C<sub>e</sub> = C<sub>brain</sub>/λ
turns this into the ke0 equation,
dC<sub>e</sub>/dt = ke0 (C<sub>arterial</sub> − C<sub>e</sub>), with

ke0 = Q (1 − exp(−PS/Q)) / (V λ).

Two cases follow. For a drug whose entry is limited by flow (PS much larger
than Q), ke0 ≈ Q / (V λ): it falls in exact proportion to brain blood flow,
whatever the solubility, and solubility sets how many minutes that costs. For
a drug limited by the blood-brain barrier (PS much smaller than Q),
ke0 ≈ PS / (V λ): flow hardly matters.

The simulation below shows this. It uses a brain of 1.4 L with a normal blood
flow of 0.75 L/min, holds one arterial curve fixed, and lowers only brain
blood flow. Each drug's λ is chosen to give its ke0 at normal flow.

<!-- generated: physiology-ke0-figure -->

<!-- generated: physiology-ke0-table -->

The fractional change in ke0 is the same for every flow-limited drug. The delay
it causes is not: at a third of normal brain flow, the fast drug's
equilibration half-time grows by about 3 minutes and the highly soluble drug's
by about half an hour. The permeability-limited drug barely changes.

What remains is how brain blood flow follows cardiac output. While blood
pressure is within the range of cerebral autoregulation, brain flow follows
cardiac output only partly, about as cardiac output to the power 0.35:

- Levine 1994 lowered cardiac output 29.9% with lower-body negative pressure
  without systemic hypotension, and middle cerebral artery velocity fell 15.5%
  (an exponent of 0.47).
- Ogoh 2005 lowered and raised cardiac output with lower-body negative pressure
  and albumin, at constant blood pressure and arterial CO<sub>2</sub>, and
  middle cerebral artery velocity rose linearly with it (an exponent of about
  0.35).
- Vlastra 2021 measured grey-matter blood flow by MRI before and after aortic
  valve replacement: 8.2% more flow per added L/min of cardiac output, from
  4.0 L/min (an exponent of 0.33).

The evidence is not strong. On cardiopulmonary bypass with blood pressure held,
halving pump flow did not change cerebral blood flow (Cook 1997), and a
systematic review of 41 studies judged them too weak to confirm that cardiac
output and cerebral blood flow are coupled (Castle-Kirszbaum 2022). Below the
autoregulatory floor, as in true shock, brain flow becomes pressure-passive and
falls further than this.

So stanpumpR takes ke0 at normal physiology and multiplies it by
CO<sup>0.35</sup>, for every drug whose entry into the brain is limited by
flow. Liver function, renal function and GI absorption leave ke0 alone.

**Morphine is the exception.** It is polar, and it crosses the blood-brain
barrier slowly: its equilibration is limited by the barrier (PS much smaller
than brain blood flow), not by delivery. Its ke0 is then
PS / (V λ), and brain blood flow drops out, as the lowest panel of the figure
above shows. So morphine's ke0 is left as it is at every cardiac output; its
clearance and central volume still change with cardiac output like any other
drug's. A drug is marked this way in the physiology table
(`ke0_permeability_limited`); morphine is the only one so far.

In the app the slower ke0 meets the smaller V1. A 2 mg/kg propofol bolus at
three cardiac outputs, simulated by stanpumpR itself:

<!-- generated: physiology-propofol-table -->

The higher plasma concentration raises the effect-site peak; the slower ke0
delays it.

## The gut

**GI absorption** scales the absorption rate constant (ka) of the oral routes.
At a higher setting the drug arrives sooner, and at a lower one later. The
amount absorbed (the bioavailability) does not change.

## The inhaled anesthetics

The gas engine is already a model of physiology: cardiac output, and its split
among the vessel-rich group (the effect site), muscle and fat, are its own
parameters. So **Cardiac output** acts on the inhaled anesthetics directly,
with nothing scaled by assumption except how the flow is shared. Liver, renal
and GI function do not apply: the agents leave by the lungs, and the engine has
no metabolism or gut.

Eger and Shafer's account of inhaled anesthetic kinetics gives the two effects.

**Uptake.** Less blood passes the lungs, so less agent is carried away and the
end-tidal concentration (FA) rises toward the inspired (FI) faster. The effect
is largest for a soluble agent, whose rise is held back most by uptake, and
small for a poorly soluble one, whose FA/FI is near 1 within minutes whatever
the output.

**Redistribution.** "In the extreme case of hypovolemic shock, cerebral blood
flow is preserved at the expense of blood flow to everything else. In this
reduced heart-brain circuit, FA/FI rises extremely fast." So the flow to the
brain and heart does not fall with cardiac output: it follows the same
autoregulated relation as the effect site of the intravenous drugs, cardiac
output to the power 0.35 (see above). The brain and heart take 16% of resting
output (brain 12%, heart 4%, the 35-year-old reference man of Williams and
Leggett 1989, Table 17). What is left of the
output is shared among the rest of the vessel-rich group, muscle and fat in
their normal proportions. At a tenth of normal output the brain and heart
receive 7% of normal output, nearly three quarters of all there is, and the
rest of the body is all but cut off. At a high output the brain's share falls
and the extra flow goes to the other tissues.

The engine's vessel-rich group is one 6 L compartment. Lumped like that, the
flow the brain keeps in shock would be spread over the kidneys, liver and gut,
and the brain would fill far too slowly. So while the slider is away from
normal, the brain and heart become a compartment of their own, which is then
the effect site. Each part has the group's normal flow per litre, so at normal
output the two fill exactly as the one group did. The engine's agreement with
GasMan is unchanged. The Visualizer draws the whole group, so the agent it
holds still adds up.

Simulated by stanpumpR (70 kg, 6 L/min ventilation, fresh gas 6 L/min):

<!-- generated: physiology-gas-table -->

A low output raises FA/FI, much more for sevoflurane than for nitrous oxide.
The brain fills a little more slowly at first, because less blood reaches it
each minute, as for the intravenous drugs. It then rises above normal, because
its blood comes from lungs at a higher tension. At a high output FA/FI rises
more slowly, and the brain ends lower.

Other effects the same chapter
describes are not modelled yet. These are flow shifted to muscle (exercise,
excitement) or to fat,
ventilation-perfusion mismatch and one-lung ventilation, and children's larger
vessel-rich group. Ventilation itself is a dose-table row.

## Body temperature and the inhaled anesthetics

**Body temperature** (28 to 37 °C) acts on the volatile agents through the two
effects of hypothermia that Eger and Shafer's chapter names, and so does
nitrous oxide, less.

**Solubility rises.** Cold blood and tissue dissolve more agent. Zhou, Liu and
Liu measured the blood:gas partition coefficient of desflurane, sevoflurane
and isoflurane in human blood from 37 to 17 °C. Its logarithm rises in a
straight line as temperature falls, by 3.0, 3.0 and 3.8% a degree. In human
brain, heart, liver, muscle and fat the tissue:gas coefficients rise in
proportion, so tissue:blood does not change. Every partition coefficient of
the agent is raised by the same factor. More agent is carried away from the
lungs, so FA/FI rises more slowly, most for the more soluble isoflurane.

**MAC falls.** Liu, Hu and Liu measured isoflurane MAC in children at 37, 34
and 31 °C. Their regression, MAC = 0.0784 T − 1.203, falls by 4.6% of the
normothermic MAC per degree. (The paper's text says "approximately 5.1%",
which its own regression does not give.) The same fall, 4 to 5% a degree, was
measured for isoflurane and halothane in rats and goats and for desflurane in
rats, so it is applied to all three volatile agents. The MAC band and the
summed MAC follow it.

**Nitrous oxide MAC falls less.** The only measurement in any species is in
rats: Antognini, Lewis and Reitan found MAC 1.9 atm at 37.5 °C and 1.6 atm at
29.7 °C, with the regression MAC = 0.03827 T + 0.3836 atm. That is 2.1% of the
normothermic MAC per degree, about half the volatile agents' fall, as its
smaller change in lipid solubility predicts. It is applied to people because
there are no human data.

**Nitrous oxide is more soluble in the cold too.** Allott, Steward, Flook and
Mapleson gathered nearly 200 temperature coefficients of anaesthetic
solubility from the literature. For nitrous oxide in water and the aqueous
media, blood among them, solubility rises 2.3% a degree as temperature falls.
Their regression predicts 1.1% a degree in oil and fat, where no coefficient
had been measured. stanpumpR applies the first to blood, the vessel-rich group
and muscle, and the second to fat.

Simulated by stanpumpR (70 kg, age 40, ventilation 6 L/min, fresh gas 6 L/min):

<!-- generated: physiology-temperature-table -->

The end-tidal concentration rises more slowly in the cold, but MAC falls
faster than solubility rises. So a given inspired concentration is a deeper
anesthetic in a hypothermic patient. Dialled to the same multiple of MAC, as
the chapter says, induction is slower, most for the soluble agents.

**Not modelled.** Cardiac output and
ventilation also fall with metabolism in hypothermia, but they have their own
controls (the Cardiac output slider and the ventilation row), so they are not
counted twice. Lower them yourself to match the patient. The coupling is left
to you on purpose: fever raises cardiac output and cooling would by the same
logic lower it, but the two have different causes (inflammation, and heat lost
to the room), so stanpumpR does not tie one slider to the other. The fall in brain
blood flow is not modelled either. A threshold for the time until threshold is
a tension you typed and does not move with MAC; the summed MAC series and its
threshold do. GasMan models none of this:
its source code mentions temperature only in the vaporiser's vapour pressure.

## Body temperature and the intravenous drugs

Cooling slows the elimination of most intravenous drugs, because enzymes and
filtration work more slowly in the cold. stanpumpR multiplies each drug's
capacity to eliminate it by exp(−0.0658 (37 − T)), 6.4% less for each degree
below 37 °C, so 55% of normal at 28 °C. The figure is Michelsen and
colleagues' fit to remifentanil in patients on bypass between 20 and 35 °C,
the one human study that spans the whole slider. The rest of the evidence
agrees within its scatter, about 6 to 8% per degree: CYP enzymes in liver
tissue, glucuronidation, filtration by the kidney (amoxicillin, ceftazidime
and doxacurium), esterases (remifentanil) and the chemical breakdown of
atracurium.

The factor acts on what the liver and kidneys can do, not on how much blood
reaches them, which is the Cardiac output slider's. So a drug whose clearance
is limited by liver blood flow changes less: propofol keeps 82% of its
clearance at 28 °C. In patients cooled deeply on bypass, Bartošová and
colleagues found propofol clearance at 42% of normal at 28 °C, because flow
falls too. Lower the Cardiac output slider as well to reproduce that: at 0.4
of normal, propofol falls to 44%. Only drugs with a physiology row respond;
volumes, intercompartmental clearances and ke0 do not change.

## The sliders and the Bayesian update

When measured concentrations update a drug ([Bayesian updating](help:models/bayesian)),
the fit is made at normal physiology, and the sliders are applied to the
patient it finds. The measurements describe the patient as they found them, so
moving a slider afterwards asks what would happen to that patient, and does not
refit. The model's own line, "(population)", is drawn at the sliders too.

## Which drugs respond

The inhaled anesthetics respond to **Cardiac output** and **Body temperature**,
as described above.

Only drugs whose renal share and hepatic share were read in a published
source respond. Any other drug ignores the sliders, and the panel lists it
under **Not changed**. Rows marked "not yet" are still waiting for a readable
source for a missing value.

**A computed extraction ratio.** Where no hepatic extraction ratio has been
measured, it is computed from the hepatic share of the drug's clearance:
E = hepatic share × CL / (Q<sub>H</sub> × B:P), with CL the model's own
plasma clearance for the reference man, Q<sub>H</sub> normal hepatic blood
flow, 1.675 L/min (25% of a resting cardiac output of 6.7 L/min, Williams
and Leggett 1989), and B:P the blood-to-plasma concentration ratio. Where no
B:P was found it is taken as 1 and the row says so. The renal share is the
measured fraction of an intravenous dose excreted unchanged in the urine,
not the share a model's creatinine-clearance term implies; the two can
differ (melphalan 0.13 measured, 0.43 in Nath's model), and the covariate
stays the patient's. A route outside the liver and the kidney (chemical
hydrolysis, irreversible binding to protein, deamination throughout the
body) is changed only by **Body temperature**.

**Chemotherapy** (2026-11-17): the drugs with a row are listed in the
table. A drug that forms an active metabolite at its own rate constant
(doxorubicin, irinotecan, cisplatin, gemcitabine, capecitabine) does not
respond, since its metabolite's formation would not move with the parent's
clearance; nor do the saturable drugs (paclitaxel, fluorouracil,
ifosfamide, thioguanine). Where the program chooses a model by the
patient's age and the one chosen cannot be scaled (vincristine and
cyclophosphamide in children, oxaliplatin's biliary-store model), the
panel says **Not changed for this patient**.

<!-- generated: physiology-table -->

## References

Allott PR, Steward A, Flook V, Mapleson WW. Variation with temperature of the
solubilities of inhaled anaesthetics in water, oil and biological media.
*Br J Anaesth* 1973;45:294-300. doi:10.1093/bja/45.3.294

Antognini JF, Lewis BK, Reitan JA. Hypothermia minimally decreases nitrous
oxide anesthetic requirements. *Anesth Analg* 1994;79:980-2.
doi:10.1213/00000539-199411000-00027

Bartošová T, Šíma M, Prskavec T, et al. Deep hypothermia reduces the predictive accuracy of the Eleveld
propofol model: population pharmacokinetic modelling in cardiac surgery.
*Biomed Pharmacother* 2026;200:119523. doi:10.1016/j.biopha.2026.119523

Castle-Kirszbaum M, Parkin WG, Goldschlager T, Lewis PM. Cardiac output and
cerebral blood flow: a systematic review of cardio-cerebral coupling.
*J Neurosurg Anesthesiol* 2022;34:352-63. doi:10.1097/ANA.0000000000000768

Cook DJ, Proper JA, Orszulak TA, Daly RC, Oliver WC. Effect of pump flow rate
on cerebral blood flow during hypothermic cardiopulmonary bypass in adults.
*J Cardiothorac Vasc Anesth* 1997;11:415-9. doi:10.1016/s1053-0770(97)90047-1

Eger EI II, Shafer SL. The evolution of pharmacokinetics. Chapter 8 of Eger's
scientific autobiography, manuscript, 2017.

Hiraoka H, Yamamoto K, Miyoshi S, et al. Kidneys contribute to the
extrahepatic clearance of propofol in humans, but not lungs and brain.
*Br J Clin Pharmacol* 2005;60:176-82. doi:10.1111/j.1365-2125.2005.02393.x

Levine BD, Giller CA, Lane LD, Buckey JC, Blomqvist CG. Cerebral versus
systemic hemodynamics during graded orthostatic stress in humans.
*Circulation* 1994;90:298-306. doi:10.1161/01.cir.90.1.298

Liu M, Hu X, Liu J. The effect of hypothermia on isoflurane MAC in children.
*Anesthesiology* 2001;94:429-32. doi:10.1097/00000542-200103000-00011

Michelsen LG, Holford NH, Lu W, Hoke JF, Hug CC, Bailey JM. The pharmacokinetics
of remifentanil in patients undergoing coronary artery bypass grafting with
cardiopulmonary bypass. *Anesth Analg* 2001;93:1100-5.
doi:10.1097/00000539-200111000-00006

Ogoh S, Brothers RM, Barnes Q, et al. The effect of changes in cardiac output
on middle cerebral artery mean blood velocity at rest and during exercise.
*J Physiol* 2005;569:697-704. doi:10.1113/jphysiol.2005.095836

Takizawa D, Hiraoka H, Goto F, Yamamoto K, Horiuchi R. Human kidneys play an
important role in the elimination of propofol. *Anesthesiology*
2005;102:327-30. doi:10.1097/00000542-200502000-00014

Vlastra W, van Nieuwkerk AC, Bronzwaer AGT, et al. Cerebral blood flow in
patients with severe aortic valve stenosis undergoing transcatheter aortic
valve implantation. *J Am Geriatr Soc* 2021;69:494-9. doi:10.1111/jgs.16882

Wilkinson GR, Shand DG. Commentary: a physiological approach to hepatic drug
clearance. *Clin Pharmacol Ther* 1975;18:377-90.

Williams LR, Leggett RW. Reference values for resting blood flow to organs of
man. *Clin Phys Physiol Meas* 1989;10:187-217.
doi:10.1088/0143-0815/10/3/001

Zhou JX, Liu J. The effect of temperature on solubility of volatile
anesthetics in human tissues. *Anesth Analg* 2001;93:234-8.
doi:10.1097/00000539-200107000-00047

Zhou J, Liu Y, Liu J. Combined effect of hypothermia and crystalloid
hemodilution on the solubility of volatile anesthetics in human blood.
*Chin Med J (Engl)* 2002;115:1014-9.
