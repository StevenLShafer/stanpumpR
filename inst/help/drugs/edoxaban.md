### Research model — not for clinical dosing

This is a research and teaching model of plasma concentration. It is **not** validated clinical decision support: a simulated concentration is not an anti-Xa level, a prothrombin time, a bleeding or clotting risk, or a safe time for surgery or a neuraxial block.

### The model

Edoxaban kinetics are from Niebecker and colleagues (*Br J Clin Pharmacol* 2015;80:1374-1387), a two-compartment model with first-order absorption preceded by a 0.25-hour lag, fitted to 17 406 concentrations from 4130 individuals — 13 phase 1 studies pooled with the Hokusai-VTE phase 3 trial in deep-vein thrombosis and pulmonary embolism. The parameters are **apparent**, so the model takes **oral** doses only (60 mg once daily, or 30 mg when dose-reduced), with a bioavailability of 1. The non-renal clearance is 15.2 L/h, the central volume 209 L, the peripheral volume 92.3 L, the inter-compartmental clearance 9.73 L/h in the phase 3 patients (the base 5.91 L/h plus the 64.6% patient increase) and the absorption rate constant 3.36 /h; at a typical Hokusai patient (80.5 kg, creatinine clearance 99 mL/min) the apparent clearance is about 38 L/h and the terminal half-life about 10 hours.

### Renal clearance

Apparent clearance is the sum of the non-renal term and a renal term that rises with creatinine clearance in two straight-line segments: a slope of 0.202 L/h per mL/min up to 90 mL/min, and a shallower slope above it. In the phase 3 patients the above-90 slope is increased by 274% over the healthy-volunteer value — 0.0321 × (1 + 2.74) ≈ 0.120 L/h per mL/min — so renal clearance keeps rising with good renal function. The model truncated creatinine clearance at 150 mL/min, so the renal term is held flat above that. Creatinine clearance is a Cockcroft–Gault value from the serum creatinine in the [Patient Profile](help:patient-profile), or an assumed normal value when blank.

### The patient this represents

The model is fixed to the phase 3, non-dose-reduced Hokusai-VTE patient, non-Asian, on no P-glycoprotein inhibitor — the clinical patient for whom edoxaban is held before surgery. The library has no input for concomitant P-gp inhibitors (which raised exposure substantially in phase 1), the protocol dose reduction, or race; an Asian patient had a 22.6% larger central volume (same steady-state exposure, lower peak). A patient on a P-gp inhibitor such as verapamil, quinidine, dronedarone, erythromycin or ketoconazole is not represented.

### Covariates

Allometrically scaled body weight acts on all disposition parameters, with the paper's own fixed exponents: the apparent clearance and the peripheral volume scale as (weight / 70) to the 0.75 power, and the central volume and the inter-compartmental clearance to the first power. Weight is evaluated at the pharmacokinetic weight under the [fat-free-mass switch](help:models/fat-free-mass), and also enters the clearance through the Cockcroft–Gault creatinine clearance.

### Provenance note

The structural and covariate parameters are the "final model" column of the paper's Table 3. The phase 3 above-90 renal slope and the phase 3 inter-compartmental clearance are the abstract's "+274%" and "+64.6%" patient increases, applied as (1 + the fraction) per the Table 3 footnotes (0.0321 × 3.74 ≈ 0.120 and 5.91 × 1.646 ≈ 9.73); the nlmixr2lib reproduction of this model uses the same 0.120. Creatinine clearance is truncated at 150 mL/min as the Methods specify, and the per-parameter allometric exponents (clearance and peripheral volume 0.75; central volume and inter-compartmental clearance 1) are the Table 3 footnote's own pairing.
