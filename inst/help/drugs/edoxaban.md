### Research model — not for clinical dosing

This is a research and teaching model of plasma concentration. It is **not** validated clinical decision support: a simulated concentration is not an anti-Xa level, a prothrombin time, a bleeding or clotting risk, or a safe time for surgery or a neuraxial block.

### The model

Edoxaban kinetics are from Niebecker and colleagues (*Br J Clin Pharmacol* 2015;80:1374-1387), a two-compartment model with first-order absorption preceded by a 0.25-hour lag, fitted to 17 406 concentrations from 4130 individuals — 13 phase 1 studies pooled with the Hokusai-VTE phase 3 trial in deep-vein thrombosis and pulmonary embolism. The parameters are **apparent**, so the model takes **oral** doses only (60 mg once daily, or 30 mg when dose-reduced), with a bioavailability of 1. The non-renal clearance is 15.2 L/h, the central volume 209 L, the peripheral volume 92.3 L, the inter-compartmental clearance 5.91 L/h and the absorption rate constant 3.36 /h; at a typical Hokusai patient (80.5 kg, creatinine clearance 99 mL/min) the apparent clearance is about 36 L/h and the half-life roughly 10-12 hours.

### Renal clearance

Apparent clearance is the sum of the non-renal term and a renal term that rises with creatinine clearance in two straight-line segments: a slope of 0.202 L/h per mL/min up to 90 mL/min, and a shallower slope above it. In the phase 3 patients the above-90 slope is 2.74 times the healthy-volunteer value (0.088 L/h per mL/min) — renal clearance keeps rising more steeply with good renal function than it did in the volunteers. Creatinine clearance is a Cockcroft–Gault value from the serum creatinine in the [Patient Profile](help:patient-profile), or an assumed normal value when blank.

### The patient this represents

The model is fixed to the phase 3, non-dose-reduced Hokusai-VTE patient, non-Asian, on no P-glycoprotein inhibitor — the clinical patient for whom edoxaban is held before surgery. The library has no input for concomitant P-gp inhibitors (which raised exposure substantially in phase 1), the protocol dose reduction, or race; an Asian patient had a 22.6% larger central volume (same steady-state exposure, lower peak). A patient on a P-gp inhibitor such as verapamil, quinidine, dronedarone, erythromycin or ketoconazole is not represented.

### Covariates

Allometrically scaled body weight acts on all disposition parameters (clearances to the 0.75 power, volumes to the first power), evaluated at the pharmacokinetic weight under the [fat-free-mass switch](help:models/fat-free-mass); weight also enters the clearance through the Cockcroft–Gault creatinine clearance.

### Provenance note

The structural and covariate parameters are the "final model" column of the paper's Table 3. The phase 3 above-90 renal slope is the published scaling of 274% applied multiplicatively (0.0321 × 2.74), as the table footnote specifies. The per-parameter allometric exponents, which the article's online full text renders ambiguously, are taken as the conventional fixed values (0.75 for clearances, 1 for volumes); this affects only the peripheral compartment away from 70 kg.
