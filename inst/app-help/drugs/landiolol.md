### One name, the model chosen for the patient

stanpumpR holds 3 published models of landiolol. You do not choose between them: stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [landiololHonda](help:drugs/landiololHonda) | Honda 2008 | 47 healthy Japanese men, 20-33 years, 50.7-77.2 kg |
| [landiololKunisawa2014](help:drugs/landiololKunisawa2014) | Kunisawa 2015 (*J Anesth*) | 9 anaesthetised women having gynaecologic surgery, 37-71 years, 47.1-73.0 kg |
| [landiololKunisawa2015](help:drugs/landiololKunisawa2015) | Kunisawa 2015 (*Ther Clin Risk Manag*) | 8 anaesthetised patients with peripheral arterial disease, 64-84 years, 40.4-71.8 kg |

**The rule.** Among the models whose studied age and weight ranges include the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for landiolol.** At ordinary weights, adults up to and including 35 years get Honda's healthy volunteers, the largest study; above 35 and up to 71 years the gynaecologic patients' model is chosen (so 35.5 years takes it); and above 71 years (71.5 included) the model from patients with peripheral arterial disease. Weight changes the choice at two edges. A 35-year-old lighter than 50.7 kg, Honda's lightest, gets the gynaecologic model. A patient of 64 to 71 years lighter than 47.1 kg, the gynaecologic study's lightest, gets the peripheral arterial disease model: from 40.4 kg because it is the only study that included both the age and the weight, and below 40.4 kg, lighter than anyone in any of the three studies, as the nearest. Children get Honda's model as the nearest; no landiolol model has been fitted in children.

All three are Japanese studies with two compartments per kilogram. Their clearances agree within 20% (36.6, 34.0 and 30.7 mL/min/kg). Their central volumes differ more (101, 74.9 and 65.0 mL/kg), as expected when one study sampled venous whole blood in volunteers and the other two arterial plasma during anaesthesia.

**What is common to all three:**
- Each fitted a lag of 0.63-0.82 minutes on the infusion. Honda attributes it to the time the drug takes to reach the sampling site: from a forearm vein through the heart to the other forearm, whose capillary bed it crosses before the vein that was sampled. stanpumpR does not draw it, as Kunisawa's group left it out of their own target-controlled infusions (STANPUMP cannot take one), so every curve is drawn under a minute early.
- Every parameter is per kilogram. With the fat-free-mass switch on, the volumes and clearances are multiplied by the pharmacokinetic weight, which follows fat-free mass; with it off, by total body weight, as published.
- Doses are entered as landiolol base, as the US product (Rapiblyk) is labelled.
- The effect site peaks **2 minutes** after a bolus: the time of the lowest heart rate after 0.2 and 0.3 mg/kg in healthy volunteers (Krumpl and colleagues, *Eur J Clin Pharmacol* 2017;73:417-428). No heart rate is drawn.
- There is no therapeutic concentration range; landiolol is titrated to heart rate.
- The US label gives a clearance of 57 mL/kg/min in awake European volunteers, measured in whole blood: almost twice the Japanese models'.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters and its limits.

See [the vasoactive and resuscitation drugs](help:models/vasoactive).
