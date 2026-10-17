### Research model — not for clinical dosing

This is a research and teaching model of plasma concentration. It is **not** validated clinical decision support: a simulated concentration is not a dilute thrombin time, an ecarin clotting time, a bleeding or clotting risk, or a safe time for surgery or a neuraxial block.

### The model

Dabigatran kinetics are from Liesenfeld and colleagues (*J Thromb Haemost* 2011;9:2168-2175), a two-compartment model with first-order absorption and a 0.63-hour lag, fitted to 27 706 concentrations from 9522 atrial-fibrillation patients in the RE-LY trial (110 or 150 mg twice daily). The parameters are **apparent**, so the model takes **oral** doses only, entered as milligrams of the prodrug **dabigatran etexilate** (the capsule strength); the roughly 6.5% absolute bioavailability of dabigatran from the capsule is already inside them. The central volume is 673 L, the peripheral volume 345 L, the inter-compartmental clearance 35.5 L/h and the absorption rate constant 0.754 /h; at the RE-LY medians the apparent clearance is 69.6 L/h and the half-life is in the 12-17 hour range.

### Renal clearance

Apparent clearance rises with creatinine clearance through an Emax (Hill) function — a maximum of 124 L/h, half-maximal at a creatinine clearance of 56.7 mL/min, with a Hill coefficient of 1.29 — so clearance falls steeply as renal function declines. Creatinine clearance is a Cockcroft–Gault value from the serum creatinine in the [Patient Profile](help:patient-profile), or an assumed normal value when blank. RE-LY excluded patients with a creatinine clearance below 30 mL/min, so below that the function extrapolates.

### The patient this represents

The model is fixed to the clinical atrial-fibrillation patient on no interacting co-medication, non-South-Asian, without heart failure, and at the median haemoglobin — the patient for whom dabigatran is held before surgery. The library has no input for ethnicity, heart failure, haemoglobin or co-medication: a South Asian patient had 20.3% lower clearance (about 25% higher exposure), a patient with heart failure 6.7% lower clearance, and verapamil, amiodarone and proton-pump inhibitors changed the bioavailability by +23%, +12% and −12.5%. None of those is represented.

### Covariates

Age and sex act on clearance (−0.41% per year above 72, and −8.3% in women), and body weight acts on the central volume (+0.77% per kg above 80.3 kg), evaluated at the pharmacokinetic weight under the [fat-free-mass switch](help:models/fat-free-mass); the paper found no weight effect on clearance. Creatinine clearance, through Cockcroft–Gault, also carries the patient's weight.

### Provenance note

The structural and covariate parameters are the final-model column of the paper's Table 5, with the clearance, age, sex and volume equations taken verbatim from its footnote. The typical clearance of 69.6 L/h at the RE-LY medians (creatinine clearance 68.6 mL/min, age 72, male) is reproduced exactly.
