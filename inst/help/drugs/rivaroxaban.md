### Research model — not for clinical dosing

This is a research and teaching model of plasma concentration. It is **not** validated clinical decision support: a simulated concentration is not an anti-Xa level, a prothrombin time, a bleeding or clotting risk, or a safe time for surgery or a neuraxial block.

### The model

Rivaroxaban kinetics are from Willmann and colleagues (*CPT Pharmacometrics Syst Pharmacol* 2018;7:309-320), an integrated one-compartment model with first-order absorption pooled from 4918 patients in seven trials across every approved indication. The parameters are **apparent**, so the model takes **oral** doses only, with a bioavailability of 1. At the population means (82.5 kg, 60.5 y, creatinine clearance 97 mL/min, a man) clearance is 6.58 L/h, scaled by (CrCl / 97)^0.406 and (weight / 82.5)^−0.278 and multiplied by 0.849 for atrial fibrillation; the volume is 62.5 L, scaled by (weight / 82.5)^0.216, (age / 60.5)^−0.189 and 0.889 for a woman; the absorption rate constant is 0.821 /h. The half-life is about 8 hours.

### The patient this represents

The indication factor is fixed to **atrial fibrillation**, the patient for whom rivaroxaban is most often held before surgery. The model has no input for comedication (a strong CYP3A4 inhibitor raised exposure only about 2%, and is not applied).

### Covariates

Renal function enters through a Cockcroft–Gault creatinine clearance, truncated at 140 mL/min × body surface area / 1.73 m² as the paper specified, from the serum creatinine in the [Patient Profile](help:patient-profile) or an assumed normal value when blank. Weight, age and sex act on the volume, weight and creatinine clearance on the clearance; all are evaluated at the pharmacokinetic weight under the [fat-free-mass switch](help:models/fat-free-mass).

### Dose-dependent bioavailability

Relative bioavailability falls with the dose (0.998 at 10 mg, 0.84 at 20 mg), following an exponential function recovered from the group's later papers and checked against their printed paediatric values. The trial doses of 15 and 20 mg were taken with food, as the label directs, so the curve describes fed dosing; a 20 mg dose taken fasting has a lower bioavailability the model does not separate.

### Provenance note

The covariate reference values are taken as the published population means; the exact centring values are in a supplement that could not be read when this was written (2026-10-17). A reference a few percent off moves the clearance by one or two percent.
