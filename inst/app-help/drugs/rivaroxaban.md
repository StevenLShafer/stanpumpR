### Research model — not for clinical dosing

This is a research and teaching model of plasma concentration. It is **not** validated clinical decision support: a simulated concentration is not an anti-Xa level, a prothrombin time, a bleeding or clotting risk, or a safe time for surgery or a neuraxial block.

### The model

Rivaroxaban kinetics are from Willmann and colleagues (*CPT Pharmacometrics Syst Pharmacol* 2018;7:309-320), an integrated one-compartment model with first-order absorption pooled from 4918 patients in seven trials across every approved indication. The parameters are **apparent**, so the model takes **oral** doses only, with a bioavailability of 1. At the population medians (81 kg, 61 y, creatinine clearance 93 mL/min, a man) clearance is 6.58 L/h, scaled by (CrCl / 93)^0.406 and (weight / 81)^−0.278 and multiplied by 0.849 for atrial fibrillation; the volume is 62.5 L, scaled by (weight / 81)^0.216, (age / 61)^−0.189 and 0.889 for a woman; the absorption rate constant is 0.821 /h. The half-life is about 8 hours.

### The patient this represents

The indication factor is fixed to **atrial fibrillation**, the patient for whom rivaroxaban is most often held before surgery. The model has no input for comedication, so none of Willmann's interacting-drug factors on clearance is applied — not the strong CYP3A4 inhibitor, nor the moderate CYP3A4 inhibitor (which lowered clearance to about 0.86) or the inducer (which raised it to about 1.30). A patient on any of these is not represented.

### Covariates

Renal function enters through a Cockcroft–Gault creatinine clearance, truncated at 140 mL/min × body surface area / 1.73 m² as the paper specified, from the serum creatinine in the [Patient Profile](help:patient-profile) or an assumed normal value when blank. Weight, age and sex act on the volume, weight and creatinine clearance on the clearance; all are evaluated at the pharmacokinetic weight under the [fat-free-mass switch](help:models/fat-free-mass).

### Dose-dependent bioavailability

Relative bioavailability falls with the dose (0.998 at 10 mg, 0.84 at 20 mg), following the exponential function in the published NONMEM control stream. The trial doses of 15 and 20 mg were taken with food, as the label directs, so the curve describes fed dosing; a 20 mg dose taken fasting has a lower bioavailability the model does not separate.

### Provenance note

The covariate reference values are the population medians hard-coded in the published NONMEM control stream (81 kg, 93 mL/min, 61 y), and the dose–bioavailability equation is that control stream's `F1` line exactly — both confirmed against the article's supplementary material.
