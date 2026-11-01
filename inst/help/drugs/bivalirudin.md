### Research model — not for clinical dosing

This is a research and teaching model of concentration. It is **not** validated clinical decision support: a simulated value is not an ACT, an ecarin clotting time, or a safe time for a procedure.

### The model

Bivalirudin kinetics are from Zhang and colleagues (*Acta Pharmacol Sin* 2012;33:1387-1394), a two-compartment model fitted to 36 young healthy Chinese volunteers given 0.5, 0.75 or 1.05 mg/kg as an intravenous bolus, or the percutaneous-intervention regimen of a 0.75 mg/kg bolus followed by 1.75 mg/kg/h for four hours. Every parameter is per kilogram of body weight, with no estimated covariate: clearance 0.323 L/h/kg, central volume 0.086 L/kg, intercompartmental clearance 0.0957 L/h/kg and peripheral volume 0.0554 L/kg. At 70 kg the model's two half-lives are about 8 and 35 minutes; the label's terminal half-life in normal renal function is 25 minutes, so the model's terminal phase is somewhat slower than the label's.

### Covariates

Size only, entered per kilogram. A per-kilogram model fitted on total body weight is scaled to fat-free mass under the [fat-free-mass switch](help:models/fat-free-mass), like the other such models in the library; unticking the box restores strict proportionality to total body weight.

### Pharmacodynamics

The study also fitted the activated clotting time as a function of concentration (baseline 134 s, maximum (ceiling) ACT 318 s — a rise of at most 184 s above baseline — half-maximal at 2.44 mg/L, with the half-maximal concentration depending on the red-cell count). That relationship is **not** plotted: the clotting time depends on the device and reagent, and the fit is specific to the study. The curve here is plasma concentration only.

### Where to be careful

These are young, healthy volunteers with normal renal function. Bivalirudin is partly renally cleared; the label reports lower clearance in renal impairment and on dialysis, and a lower clearance in percutaneous-intervention patients (0.204 against this study's 0.323 L/h/kg). This model has no renal covariate and will under-predict concentrations in an older or renally impaired patient. The label's clearance is deliberately not spliced in, because it belongs to a different fit with different volumes.
