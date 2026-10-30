The plot normally shows each drug's plasma concentration and its effect site. **Graph Options → Peripheral compartments** adds, directly under each drug's panel, a panel for every other compartment of its model, so the whole of the model can be watched at once. The drug's own panel is the plasma, which is the central compartment, V1, by definition.

| Panel | Shown for | What it plots | Units |
|---|---|---|---|
| **V2** | A drug whose model has a second compartment | The concentration in the rapidly equilibrating peripheral compartment | The drug's concentration units |
| **V3** | A drug whose model has a third compartment | The concentration in the slowly equilibrating peripheral compartment | The drug's concentration units |
| **MRG** | An inhaled agent | The tension in the muscle group | % of one atmosphere |
| **FG** | An inhaled agent | The tension in the fat group | % of one atmosphere |
| **depot** | A dose given by mouth, under the tongue, intramuscularly, intranasally, subcutaneously or by tissue injection (RA) | The amount still to be absorbed | mg for a drug in mcg/mL, mcg for one in ng/mL, ng for one in pg/mL, mmol for one in mmol/L, U for insulin, IU for an anti-Xa activity |

Each panel is drawn in the drug's colour and labelled with the compartment alone; it belongs to the drug panel above it. A drug with only one compartment, given only intravenously, has no further panels. Hover over a compartment panel to read its value; click it to add a dose of its drug, as on the drug's own panel.

## The peripheral concentrations

In the [three-compartment model](help:models/three-compartment) drug moves between the plasma and each peripheral compartment in proportion to the difference in concentration:

dC2/dt = k21 (C1 − C2),   dC3/dt = k31 (C1 − C3)

so each peripheral compartment follows the plasma with its own delay, as the effect site does with ke0. It takes up drug while the plasma is above it and gives drug back whenever the plasma is below it, so it peaks when it crosses the plasma. At steady state each holds the plasma concentration. The plasma usually falls below a compartment soon after delivery stops, but it can also do so while drug is still running, after a large bolus followed by a small infusion. The concentrations are exact: the engine carries the plasma as a sum of exponentials, one per mode of the system, and the concentration in V2 is k21 Σ s/(k21 − μ) over those modes (s the mode's share of the plasma, μ its rate constant), with the same for V3. A formed active metabolite is drawn in its own drug's compartments, added to whatever of that drug was given directly, just as its plasma is.

The volumes are the model's. Where a model reports apparent oral volumes (V/F), the compartments are apparent too. A drug whose saturable elimination is integrated numerically (phenytoin, GHB, exenatide, cetagliptin, linagliptin, vildagliptin) shows its peripheral compartment where its model has one.

## The depot

The depot is what is still to be absorbed: the model's absorption compartment, summed over every depot the drug has (a second oral depot, or the fast and slow tissue depots of a regional block). It holds the **bioavailable** amount, F times the dose, so 30 mg of morphine by mouth puts 8.7 mg in it. A dose enters the depot when its absorption lag has passed, so a lagged dose appears as a step after the lag rather than at the time it was given; see [Oral, intramuscular, intranasal, subcutaneous and regional anesthesia doses](help:models/absorption). A continuous perineural infusion fills the tissue depot at its rate, and the part of a subcutaneous dose that the model delivers at a constant rate straight into the circulation never passes through it.

An oral dose that the model delivers at a constant rate (carvedilol), rather than from a first-order depot, has no depot to show.

## The inhaled agents

For an inhaled agent the drug's panel is the alveolar tension and the effect site is the vessel-rich group (brain). The two further compartments of the [inhaled-gas engine](help:models/gas-engine) are the **muscle group** (MRG) and the **fat group** (FG), shown as tensions in % of one atmosphere, the same units as the alveolar panel. The fat group fills slowly for hours, which is why emergence after a long anaesthetic is slower than after a short one.

## When the panels are not shown

- Under **Normalize to → Peak plasma** or **Peak effect site**: a compartment has no peak of its own to normalize to.
- For a drug plotted as something other than a concentration in V1: mannitol's serum osmolality, the INR of warfarin, fluindione and acenocoumarol, and heparin on bypass's activated clotting time.
- For a drug given only intravenously whose model has a single compartment.

Scenarios: [Propofol: filling and emptying the peripheral compartments](scenario:propofol-peripheral-compartments) and [Oral morphine: emptying the depot](scenario:oral-morphine-depot).
