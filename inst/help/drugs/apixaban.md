### Research model — not for clinical dosing

This is a research and teaching model of plasma concentration. It is **not** validated clinical decision support: a simulated concentration is not an anti-Xa level, a bleeding or clotting risk, or a safe time to operate or to place a neuraxial block. Those decisions follow published perioperative interval guidance, not a simulated curve.

### The model

Apixaban kinetics are from Cirincione and colleagues (*CPT Pharmacometrics Syst Pharmacol* 2018;7:728-738), a two-compartment model with first-order absorption fitted to 4385 subjects across twelve phase I–III studies. The parameters are **apparent** (divided by the unmeasured oral bioavailability of about 50%), so the model takes **oral** doses only, with a bioavailability of 1. Clearance is split into a renal and a non-renal part:

- renal CL/F = 1.57 × (CrCl / 80) L/h,
- non-renal CL/F = 2.02 × (age / 65)^−0.429 × (0.784 if female) L/h,

and the total is multiplied by 0.861 for a patient with nonvalvular atrial fibrillation. The central volume is 30.0 × (weight / 70)^0.790 × 0.960 L, the intercompartmental clearance 1.91 L/h and the peripheral volume 27.0 L. The absorption rate constant is 0.473 /h. The typical atrial-fibrillation patient (65 y, 70 kg, creatinine clearance 80 mL/min) has a total clearance of 3.09 L/h; 5 mg twice daily reaches a steady-state peak near 170 ng/mL, the paper's simulated median.

### The patient this represents

The library has no input for indication, race, co-medication or the time of day a dose is taken, so the model is fixed to the paper's **atrial-fibrillation** patient — the one for whom apixaban is most often held before surgery — taken to be non-Asian, on no strong or moderate CYP3A4/P-glycoprotein inhibitor, and dosed in the morning or afternoon. An evening dose (43% slower absorption) or an interacting drug (14.6% lower clearance) is not represented.

### Covariates

Renal function enters through the Cockcroft–Gault creatinine clearance, from the serum creatinine in the [Patient Profile](help:patient-profile), or an assumed normal value for age and sex when the field is blank — so renal impairment is shown only when a creatinine is entered. Renal clearance scales linearly with creatinine clearance (fixed exponent 1, no ceiling), as the published model does. Age and sex act on the non-renal clearance, weight on the central volume; all are evaluated at the pharmacokinetic weight under the [fat-free-mass switch](help:models/fat-free-mass).

### Dose-dependent bioavailability

Relative bioavailability falls as the dose rises, 1 at 2.5 mg to 0.58 at 50 mg, following the paper's power model. Each dose is scaled by its own size before the engine, so superposition still holds for doses given apart in time.
