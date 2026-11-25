The **Physiology** panel in the left-hand column asks *what if*: what if hepatic
function were half of normal, the kidneys shut down, or cardiac output fell
by two thirds? It has four sliders of organ function, a fifth for body
temperature (see *Body temperature* below), and, while an inhaled anesthetic
is in the dose table, a slider for altitude and a box for endobronchial
intubation, which act on the inhaled anesthetics only (see *Altitude* and
*Endobronchial intubation* below). The Visualizer shows the settings under
its title. Each of the four is a multiple of
normal function, on a log scale from a tenth (0.1) to ten times (10). All four
start at 1, which is normal and changes nothing. **Reset to normal** puts them
back, the temperature to 37 °C, the altitude to sea level and the tube to the
trachea.

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
impairment twice. The same holds for the **Serum AST** and the **Liver
function** slider for a drug whose model reads the AST (epirubicin).

The sliders change each drug's pharmacokinetic **parameters**, once, before
the simulation runs. As with any covariate, the parameters then stay fixed
for the whole simulation, unless a **physiology event** changes them at a
time during the case (*Physiology events*, below).

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

Two limits of the liver slider matter for the chemotherapy drugs.

- **Prodrugs.** Cyclophosphamide and dacarbazine are activated in the
  liver. Lowering the slider raises the plotted parent drug, but the active
  metabolite, which is not plotted, falls.
- **What the studies found.** For some drugs, studies of patients with
  liver disease found no change in pharmacokinetics where the slider
  predicts one. Topotecan's clearance was unchanged in hepatic impairment
  (the Hycamtin label; O'Reilly 1996). Vinorelbine's was unchanged in liver
  dysfunction (Kitzen 2010; Gong 2019). Etoposide's total clearance was
  unchanged in obstructive jaundice, because more drug left in the urine
  (Hande 1990). For these drugs the slider shows what the well-stirred
  model predicts, not what was measured.

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
excitement) or to fat, the ventilation-perfusion mismatch of the normal
anaesthetised lung, and children's larger vessel-rich group. One-lung
ventilation is: see *Endobronchial intubation*, below. Ventilation itself is a
dose-table row.

## Body temperature and the inhaled anesthetics

**Body temperature** (28 to 38 °C) acts on the volatile agents through the two
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

**The thresholds move with MAC.** An inhaled agent's threshold for the time
until threshold, the default or one you type, is its value at 37 °C, which is
the value clinicians know: nobody knows what a tenth of sevoflurane's MAC is at
32 °C. It moves with temperature exactly as MAC does, as it already moves with
age, so a tenth of MAC stays a tenth of MAC. Sevoflurane's default at age 40,
0.18 per cent at 37 °C, is 0.138 per cent at 32 °C; after half an hour at
2 per cent in 6 L/min of oxygen (70 kg) the brain takes 12.8 minutes to fall to it, against 11.2 to 0.18 per
cent and 10.7 at 37 °C. With a temperature event, each moment's threshold is
the one at the temperature in force then. The summed MAC series and its
threshold, 0.1 MAC, follow temperature through the MAC they divide by. (Until
24 November 2026 a threshold was held at its typed tension whatever the
temperature, so at 32 °C the default was 0.13 of MAC.)

**Not modelled.** Cardiac output and
ventilation also fall with metabolism in hypothermia, but they have their own
controls (the Cardiac output slider and the ventilation row), so they are not
counted twice. Lower them yourself to match the patient. The coupling is left
to you on purpose: fever raises cardiac output and cooling would by the same
logic lower it, but the two have different causes (inflammation, and heat lost
to the room), so stanpumpR does not tie one slider to the other. The fall in brain
blood flow is not modelled either. GasMan models none of this:
its source code mentions temperature only in the vaporiser's vapour pressure.

## Altitude and the inhaled anesthetics

**Altitude** (0 to 7,000 m) sets the barometric pressure from West's model
atmosphere, P = exp(6.63268 − 0.1112 h − 0.00149 h²) Torr with h in km:
760 Torr at sea level, 604 at 2,000 m, 475 at 4,000 m and 324 at 7,000 m.
West built it for high mountain sites: it predicted the measured pressure at
14 of them, from 3,094 m to the summit of Everest, to within 1%, where the
aviation standard atmosphere (308 Torr at 7,000 m) reads too low. West warns
that it reads a few Torr high at latitudes of 45° and above. Altitude acts on
the inhaled anesthetics only.

**Potency follows partial pressure.** The tensions stanpumpR plots, and MAC,
are partial pressures in % of a sea-level atmosphere, so 1 MAC means the same
at every altitude. James and White proposed calling MAC the minimum alveolar
partial pressure for this reason. James and colleagues found that 50% nitrous
oxide raised the pain threshold by 71.5% at a sea-level pressure, 40% at
1,460 m and only 19% at a simulated 3,300 m, falling with its partial
pressure.

**What the machine delivers.** Room air, and every gas from a flowmeter, is a
fraction of the ambient pressure. So 67% nitrous oxide at 7,000 m is 29% of a
sea-level atmosphere. A variable-bypass vaporizer (sevoflurane, isoflurane)
delivers about its dial's sea-level partial pressure at any altitude, because
the agent's saturated vapour pressure does not depend on the barometric
pressure: James and White found that halothane vaporizers in a pressure
chamber at 5,000 and 10,000 feet delivered the same partial pressure as at
sea level, within 0.1 kPa. The desflurane vaporizer is a heated, pressurised
blender that delivers its dial's volume percent, which is a lower partial
pressure at altitude: to keep the same depth, turn the dial up by 760 over the
pressure. Andrews and Johnston's example turns it from 10% to 12.8% at
2,000 m.

Simulated by stanpumpR (70 kg, age 40, ventilation 6 L/min, fresh gas
6 L/min):

<!-- generated: physiology-altitude-table -->

**Water vapour.** Inspired gas is humidified in the airway to 47 Torr at
37 °C, whatever the pressure, so the other gases share what is left: 713 Torr
at sea level, 277 Torr at 7,000 m. The sea-level humidification is already
inside the tensions stanpumpR plots, and inside MAC, so only its change with
altitude enters: what the fresh gas brings to the alveoli is multiplied by
((P − 47)/P) / (713/760), exactly 1 at sea level, 0.97 at 3,000 m and 0.91 at
7,000 m. A vaporizer's agent and the oxygen are diluted by it alike. Gas the
patient rebreathes was humidified, dried in the circuit and humidified again,
so the factor cancels for it. The water vapour pressure is kept at its 37 °C
value at every body temperature.

**The volumes.** Apart from the humidification, the kinetics do not depend on
the pressure in partial pressures. What changes is where a partial pressure
becomes a volume. The same partial pressure taken up is a larger ambient
volume at altitude, so the concentration and second gas effects are larger,
and so is the volume the oxygen consumed takes. The carbon dioxide is a
larger share of the exhaled gas, so at low flows the absorber, by removing it,
concentrates the rebreathed agent more. At 2 L/min of oxygen with 2%
sevoflurane and a minute ventilation of 5 L/min, the end-tidal tension after
an hour is 1.60% at sea level and 1.65% at 7,000 m, the absorber's
concentration slightly outweighing the humidification.

**Oxygen.** Breathing air at 5 L/min, the alveolar oxygen after 30 minutes falls
from about 101 Torr at sea level to about 9 Torr at 7,000 m: the inspired
partial pressure falls, the water vapour takes its 47 Torr, and the oxygen
consumed takes the same partial pressure from what is left. Climbers at that
height survive by breathing far more than 5 L/min, which the ventilation row
can show.

**Not modelled.** Flows are
taken as true flows. A bobbin flowmeter calibrated at sea level delivers more
than it reads at altitude: James and White measured about 7% more at 4,800 ft
and 19 to 21% more at 10,000 ft above 4 L/min, close to the square root of 760
over the pressure, and much less at low flows. Acclimatisation (a larger haemoglobin mass, a
hypoxic drive to breathe) is not modelled, and cardiac output and ventilation
keep their own controls. The intravenous drugs do not respond.

## Endobronchial intubation

The **Endobronchial intubation** box, shown with Altitude while an inhaled
anesthetic is in the dose table, puts the tracheal tube in one main bronchus.
Eger and Shafer's chapter on inhaled anesthetic kinetics describes what
follows: the intubated lung receives all the ventilation, twice its normal
share, and the other lung is not ventilated but is still perfused. Its blood
takes up no agent and joins the arterial blood at the mixed venous partial
pressure, a right-to-left shunt, so end-tidal and arterial partial pressures
diverge. stanpumpR takes the chapter's idealisation: half the cardiac output
passes the unventilated lung, and the ventilated lung holds half the alveolar
gas. (The half is the chapter's, kept on Shafer's decision of
2026-11-19.)

How much the arterial rise is delayed depends on solubility. For a poorly
soluble agent the end-tidal tension approaches the inspired whatever the blood
takes, so doubling one lung's ventilation cannot make up for the other, and the
arterial partial pressure starts at about half of normal. For a highly soluble
agent doubling the ventilation doubles the delivery and makes up for it, and
the arterial rise is barely delayed. Eger and Severinghaus predicted this with
an analogue model, and Stoelting and Longnecker confirmed it in dogs and
people. In stanpumpR (fresh gas and ventilation 6 L/min), arterial is 72% of
normal for nitrous oxide at 2 minutes and 78% for isoflurane, 92% and 87% at
an hour, while end-tidal reads a little above normal. The Visualizer draws the
arterial bar below the end-tidal one.

**Hypoxaemia.** Doubling the ventilation of one lung nearly makes up for the
other lung's loss of carbon dioxide excretion, because carbon dioxide is freely
soluble in blood. It cannot make up for oxygen. Haemoglobin leaving the
ventilated lung is already nearly saturated, so the extra ventilation adds only
the little oxygen that dissolves, while the shunted half of the blood arrives
at mixed venous content. Hypoxaemia necessarily follows. stanpumpR computes the
arterial oxygen from the alveolar oxygen the engine carries: the oxygen content
of blood leaving the ventilated lung (Severinghaus's 1979 dissociation curve),
less the shunt's share of the body's oxygen use. The oxygen the ventilated lung
loads into its blood then equals the body's use exactly, the same oxygen the
gas engine draws from the alveoli. The result is the oxygen panel's second
line; the hover gives the PaO2 and the SaO2, and the Visualizer shows them
under its title while the tube is in. For a 70 kg patient at a cardiac output
of 5 L/min and oxygen use of 0.25 L/min, half the cardiac output shunted gives,
at 30 minutes and haemoglobin 15 g/dL:

| Fresh gas (6 L/min) | End-tidal PO2 | PaO2 | SaO2 |
|---|---|---|---|
| Oxygen | 666 mmHg | 50 mmHg | 85% |
| Oxygen 2 L/min, air 4 L/min | 293 mmHg | 44 mmHg | 79% |
| Air | 107 mmHg | 40 mmHg | 75% |

Even with pure oxygen, the PaO2 is about 50 mmHg. That is the shunt's
signature: more oxygen hardly helps, and neither does more ventilation (air at
3 L/min gives 69%, at 12 L/min 75%).

**Haemoglobin.** Ticking the box shows a **Haemoglobin** slider (4 to 20 g/dL,
15 by default). The oxygen the shunted half fails to carry is the same number
of millilitres per decilitre whatever the haemoglobin, so it is a larger share
of a smaller capacity, and an anaemic patient desaturates more: on pure oxygen
the SaO2 is 68% at 7 g/dL and 87% at 17 g/dL, on air 48% and 77%. A physiology
event records it, so a transfusion or a bleed can be followed. Without a shunt
it changes nothing the plot shows.

**Altitude.** The arterial oxygen starts from the alveolar oxygen, which falls
with the barometric pressure, so the hypoxaemia deepens with altitude. On air
the SaO2 is 75% at sea level, 66% at 3000 m and 43% at 5000 m; on oxygen 85%,
82% and 80%.

**Not modelled.** Hypoxic pulmonary vasoconstriction, which diverts part of the
blood from the unventilated lung, so the shunt is about half only at first and
the hypoxaemia then eases; the lungs' unequal sizes; the slow absorption of the
gas trapped in the unventilated lung; the blood's own oxygen store, so the
arterial oxygen changes at once with the tube or the fresh gas rather than over
a circulation time; and the dissociation curve's shift with temperature and pH.

## Body temperature and the intravenous drugs

Cooling slows the elimination of most intravenous drugs, because enzymes and
filtration work more slowly in the cold. stanpumpR multiplies each drug's
capacity to eliminate it by exp(−0.0658 (37 − T)), 6.4% less for each degree
below 37 °C, so 55% of normal at 28 °C, and 6.8% more at 38 °C. The figure is
Michelsen and colleagues' fit to remifentanil in patients on bypass between 20
and 35 °C, the one human study across most of the slider. The rest of the evidence
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

## Fever

The slider reaches 38 °C, where thermal stress starts. Every relation above
was measured in people at or below 37 °C (only Antognini's rats reached 38
°C), so the degree above 37 is an extrapolation of each of them: elimination
capacity 6.8% higher, the volatile agents 3 to 4% less soluble, and MAC 4.6%
higher (nitrous oxide's 2.1%). The slider stops at 38 °C because above it
enzymatic action starts to fail, which cannot be extrapolated (Steven Shafer,
25 November 2026).

The sliders move in small steps on a log scale, so the multiple a slider
shows is rounded: x0.50 applies 0.501, x2.00 applies 1.995. The difference is
far below anything the curves can show, and it is not an error.

## Physiology events: physiology that changes during a case

The sliders describe one physiology for the whole plot. To let it change
during a case, set the sliders and click **Create event**, below them, and
enter a time in the plot's time units. The settings are recorded as an event
labelled **Physiology** in the Events panel; resting the pointer on it shows
what it recorded. Once the event table holds any Physiology event:

* physiology is **normal from time 0** (every slider at 1, 37 °C, sea level,
  a tracheal tube), and steps to each event's settings at its time, as a
  step, not a ramp. For a gradual change, create several events, a few
  minutes apart;
* the sliders no longer set the plot; they set the next event to be created,
  and the panel says so;
* at each step **the drug in each compartment stays where it is**. Volumes,
  clearances and flows change; amounts do not move from one compartment to
  another. A central volume that halves therefore doubles the plasma
  concentration at that instant. This is the conversion Steven Shafer wrote
  for the Zuppa dexmedetomidine model, whose parameters change on and off
  bypass, and it is checked against an exact matrix-exponential solution;
* for the inhaled anesthetics, the partial pressures in the breathing
  circuit and the lungs carry across a step in cardiac output, temperature
  or intubation, and a cooled tissue keeps the agent it held at a lower
  partial pressure, because it now dissolves more of it. A step in altitude
  moves gas in or out of the lungs and the circuit, which keep their volume:
  going up, gas leaves at the composition it has, so every partial pressure
  falls with the pressure; going down, the lungs draw in fresh gas, as
  much as the fresh gas flow supplies of what they inspire (with no fresh
  gas flowing, nothing is drawn, and the vaporiser adds nothing).
  Dissolved agent does not move at the step. MAC steps with the temperature;
* the time until threshold, at each moment, assumes the physiology in force
  at that moment continues.

Events are edited, moved and deleted with the other events (double-click the
Events panel); the dialog shows each Physiology event's settings, only those
away from normal. They travel in a bookmark and are listed in the exported
workbook. A clinical event that changes a drug's model (bypass, for
dexmedetomidine) and a physiology event act together.

What does not follow the events: a drug whose model the sliders do not change
(*Which drugs respond*, below); a drug on the saturable (Michaelis-Menten),
transit-chain or other special engines, or one that forms a metabolite, whose
PK the event engine cannot switch during a run; a drug changed by the
antiseizure interactions, whose own segments take precedence; and a dose
table with a rate into the tissue depot (a regional-anaesthesia infusion, or
an extended-release oral form the program releases that way), which keeps
normal physiology throughout and is named in the panel. A TCI pump plans on
the physiology in force at time 0 (an event at time 0 included, as the
sliders are without events); a later event is one the pump does not know of,
so the concentration drifts from the target, as it would in a patient. The Visualizer shows the physiology in force as the run plays, an
endobronchial tube pulled back (or slipping in) during the run included: the
unventilated lung is greyed in the 2D view until the tube is tracheal. It
does not yet draw an inhaled agent whose body temperature changes during the
run.

## The sliders and the Bayesian update

When measured concentrations update a drug ([Bayesian updating](help:models/bayesian)),
the fit is made at normal physiology, and the sliders are applied to the
patient it finds. The measurements describe the patient as they found them, so
moving a slider afterwards asks what would happen to that patient, and does not
refit. The model's own line, "(population)", is drawn at the sliders too.
With physiology events the same holds: the fit is made at normal physiology
throughout, and the events are applied to the patient it finds.

## Which drugs respond

The inhaled anesthetics respond to **Cardiac output**, **Body temperature**,
**Altitude** and **Endobronchial intubation**, as described above.

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

**Antiseizure drugs** (2026-11-18): thirteen have a row. Most models are oral
fits whose clearance is CL/F; with bioavailability near 1 and extraction
ratios at most 0.06, the difference is immaterial, and each row says so.
Levetiracetam's hydrolysis in blood and tissues is changed only by **Body
temperature**. Phenytoin's saturable elimination is outside the sliders,
and ethosuximide's renal share (0.25) is Bauer 1982's renal against total clearance in healthy adults. The rows act through the
antiseizure interactions too, which rebuild each victim at the sliders'
physiology.

**The remaining anaesthetic drugs** (2026-11-18): fentanyl, hydromorphone,
ketamine, rocuronium and vecuronium now have computed extraction ratios
(0.39, 0.48, 0.54, 0.50 and 0.46; rocuronium's 0.21 since its model became
Kleijn 2011 on 2026-11-23), and sufentanil has Lange's measured 0.92
with 62% of its clearance hepatic. Hydromorphone's blood-to-plasma ratio is
1.35. Rocuronium and vecuronium were measured in plasma and are quaternary
amines, which stay out of red cells, so their ratio is taken as
1 − haematocrit, 0.55; fentanyl's and ketamine's were not found and are
taken as 1. Each row's notes give the measurements that disagree.

**High-extraction drugs** (2026-11-18): propranolol, metoprolol,
verapamil, diltiazem, nicardipine, naloxone, flumazenil, buprenorphine and
intravenous haloperidol have extraction ratios computed from their
intravenous clearances (0.51 to 0.75), so their clearance follows cardiac
output more than liver function. Where an oral bioavailability is known
(metoprolol, verapamil, diltiazem, propranolol), it agrees with the computed
ratio or implies a somewhat larger one. Oxymorphone and labetalol are held
back: their models' clearances exceed hepatic blood flow, so no extraction
ratio can be computed without a measured blood-to-plasma ratio.

**Kidney-cleared drugs** (2026-11-18): the antibiotics cefazolin,
cefalexin, meropenem, piperacillin, cefepime, gentamicin, tobramycin,
amikacin and vancomycin; enoxaparin, fondaparinux and dabigatran;
gabapentin, pregabalin, lisinopril, hydrochlorothiazide, metformin,
milrinone, digoxin, dofetilide, sotalol, ganciclovir, tranexamic acid,
mannitol, lithium, pramipexole, amantadine and magnesium sulfate. Each
renal share is the fraction its label gives as excreted unchanged; where a
label says only "almost exclusively" or "predominantly" (gentamicin,
tobramycin, sotalol, lithium), the row says the number is a judgement.
Most of these models already read the patient's serum creatinine; the
**Renal function** slider acts on top of it, as a what-if beyond the
measured value.

**Local anaesthetics** (2026-11-18): bupivacaine (E 0.28) and mepivacaine
(E 0.27) have computed extraction ratios; the sliders change their systemic
disposition, not their absorption from the injection site. Lidocaine and
ropivacaine form their toxic metabolites (MEGX, PPX) at their own rate, so,
like the other metabolite-forming models, they are not scaled.

<!-- generated: physiology-table -->

## References

Allott PR, Steward A, Flook V, Mapleson WW. Variation with temperature of the
solubilities of inhaled anaesthetics in water, oil and biological media.
*Br J Anaesth* 1973;45:294-300. doi:10.1093/bja/45.3.294

Andrews JJ, Johnston RV. The new Tec 6 desflurane vaporizer.
*Anesth Analg* 1993;76:1338-41. doi:10.1213/00000539-199376060-00027

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

Eger EI II, Severinghaus JW. Effect of uneven pulmonary distribution of blood
and gas on induction with inhalation anesthetics. *Anesthesiology*
1964;25:620-6.

Eger EI II, Shafer SL. The evolution of pharmacokinetics. Chapter 8 of Eger's
scientific autobiography, manuscript, 2017.

Gong J, Cho M, Gupta R, Synold TW, Frankel P, Ruel C, Fakih M, Chung V, Lim D,
Chao J. A pilot study of vinorelbine safety and pharmacokinetics in patients
with varying degrees of liver dysfunction. *Oncologist* 2019;24:1137-1145.
doi:10.1634/theoncologist.2018-0336

Hande KR, Wolff SN, Greco FA, Hainsworth JD, Reed G, Johnson DH. Etoposide
kinetics in patients with obstructive jaundice. *J Clin Oncol*
1990;8:1101-7. doi:10.1200/JCO.1990.8.6.1101

Hiraoka H, Yamamoto K, Miyoshi S, et al. Kidneys contribute to the
extrahepatic clearance of propofol in humans, but not lungs and brain.
*Br J Clin Pharmacol* 2005;60:176-82. doi:10.1111/j.1365-2125.2005.02393.x

James MF, Manson ED, Dennett JE. Nitrous oxide analgesia and altitude.
*Anaesthesia* 1982;37:285-8. doi:10.1111/j.1365-2044.1982.tb01100.x

James MF, White JF. Anesthetic considerations at moderate altitude.
*Anesth Analg* 1984;63:1097-105. PMID 6239572

Kitzen JJ, Puozzo C, de Jonge MJ, Brandely M, Verweij J. Mild to moderate
liver dysfunction does not require dose reduction of oral or intravenous
vinorelbine: results of a pharmacokinetic study. *Eur J Cancer*
2010;46:266-9. doi:10.1016/j.ejca.2009.10.031

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

O'Reilly S, Rowinsky E, Slichenmyer W, et al. Phase I and pharmacologic
studies of topotecan in patients with impaired hepatic function. *J Natl
Cancer Inst* 1996;88:817-24. doi:10.1093/jnci/88.12.817

Stoelting RK, Longnecker DE. Effect of right-to-left shunt on rate of
increase in arterial anesthetic concentration. *Anesthesiology*
1972;36:352-6.

Takizawa D, Hiraoka H, Goto F, Yamamoto K, Horiuchi R. Human kidneys play an
important role in the elimination of propofol. *Anesthesiology*
2005;102:327-30. doi:10.1097/00000542-200502000-00014

Vlastra W, van Nieuwkerk AC, Bronzwaer AGT, et al. Cerebral blood flow in
patients with severe aortic valve stenosis undergoing transcatheter aortic
valve implantation. *J Am Geriatr Soc* 2021;69:494-9. doi:10.1111/jgs.16882

West JB. Prediction of barometric pressures at high altitude with the use of
model atmospheres. *J Appl Physiol* 1996;81:1850-4.
doi:10.1152/jappl.1996.81.4.1850

Wilkinson GR, Shand DG. Commentary: a physiological approach to hepatic drug
clearance. *Clin Pharmacol Ther* 1975;18:377-90.

Williams LR, Leggett RW. Reference values for resting blood flow to organs of
man. *Clin Phys Physiol Meas* 1989;10:187-217.
doi:10.1088/0143-0815/10/3/001

Zhou J, Liu Y, Liu J. Combined effect of hypothermia and crystalloid
hemodilution on the solubility of volatile anesthetics in human blood.
*Chin Med J (Engl)* 2002;115:1014-9.

Zhou JX, Liu J. The effect of temperature on solubility of volatile
anesthetics in human tissues. *Anesth Analg* 2001;93:234-8.
doi:10.1097/00000539-200107000-00047
