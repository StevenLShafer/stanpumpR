### Research model — not for clinical dosing

This is a research and teaching model of plasma concentration. It is **not** validated clinical decision support: a simulated concentration is not an anti-Xa level, a bleeding or clotting risk, or a safe time to operate or to place a neuraxial block. Those decisions follow published perioperative interval guidance, not a simulated curve.

### A second apixaban model

The library carries two apixaban models. This one, **`apixabanVTE`**, is the **venous-thromboembolism (VTE) treatment** model; the other, `apixaban`, is the **nonvalvular-atrial-fibrillation** model (Cirincione 2018). They are fitted to different trial populations and dosing, so they give slightly different curves for the same dose; pick the one that matches the indication you want to teach. (This mirrors the two dabigatran models, `dabigatran` and `dabigatranHip`.)

### The model

Apixaban kinetics are from Byon and colleagues (*CPT Pharmacometrics Syst Pharmacol* 2017;6:340-349), a two-compartment model with first-order absorption fitted to 970 subjects (8323 concentrations) across eight phase I, one phase II and two phase III (AMPLIFY, AMPLIFY-EXT) VTE-treatment studies. The parameters are **apparent** (divided by the unmeasured oral bioavailability of about 50%), so the model takes **oral** doses only, with a bioavailability of 1. Clearance is split into a renal and a non-renal part:

- renal CL/F = 1.83 × min(CrCl, 150) / 100 L/h,
- non-renal CL/F = 2.52 × (age / 60)^−0.267 × (0.777 if female) L/h.

The central volume is 32.1 × (weight / 85)^0.523 L, the intercompartmental clearance 1.62 L/h and the peripheral volume 19.8 L. The absorption rate constant is 0.440 /h. The reference VTE patient (60 y, 85 kg, creatinine clearance 100 mL/min, non-Asian man) has a total clearance of **4.35 L/h**; 5 mg twice daily reaches a steady-state daily exposure near the paper's simulated median (2446 ng·h/mL, peak ≈ 132 ng/mL).

### The patient this represents

The library has no input for race, co-medication or the time of day a dose is taken, so the model is fixed to a **non-Asian** patient, on no strong or moderate CYP3A4/P-glycoprotein inhibitor, and dosed in the morning or afternoon. An Asian patient (16.8% lower clearance), an evening dose (46% slower absorption, ka 0.239 /h) or an interacting drug (20.3% lower clearance) is not represented.

### Covariates

Renal function enters through the Cockcroft–Gault creatinine clearance, from the serum creatinine in the [Patient Profile](help:patient-profile), or an assumed normal value for age and sex when the field is blank — so renal impairment is shown only when a creatinine is entered. Renal clearance rises with creatinine clearance up to a **150 mL/min breakpoint**, above which it is held constant, as the published model does; severe impairment (creatinine clearance 15 mL/min) lowers total clearance by about 36% and raises exposure by about 56%. Age and sex act on the non-renal clearance, weight on the central volume; all are evaluated at the pharmacokinetic weight under the [fat-free-mass switch](help:models/fat-free-mass).

### What is left out

Apixaban absorption becomes dissolution-rate-limited above 10 mg, and the published model carries a relative-bioavailability term for that fall. Its parameters are not printed in the paper (they were fixed from earlier apixaban analyses), so no dose-dependent bioavailability is applied here. At the VTE-treatment doses (2.5, 5 and 10 mg) that term is near 1, so this changes nothing in the therapeutic range; a simulated dose above 10 mg would be over-predicted. The paper's anti-factor Xa relationship (a linear 0.0159 IU/mL per ng/mL on one assay) is documentation only and is not plotted.
