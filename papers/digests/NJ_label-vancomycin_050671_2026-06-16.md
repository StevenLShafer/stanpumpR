---
file: NJ_label-vancomycin_050671_2026-06-16.pdf
pmid: none
doi: none
citation: "VANCOMYCIN injection, for intravenous use (GALAXY premixed frozen bags), US prescribing information, Baxter; NDA 050671 (s043), FDA label dated 2026-06-16. https://www.accessdata.fda.gov/drugsatfda_docs/label/2026/050671s043lbl.pdf"
kind: label
drugs: [vancomycin]
source_read: text layer; read Highlights, contents, 2.1-2.5 (Dosage and Administration), 12.2 Pharmacodynamics, 12.3 Pharmacokinetics
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
---

# Vancomycin injection label 2026 (NDA 050671): prescribing information

## Study
Regulatory label. Premixed vancomycin injection in fixed strengths (500 mg, 750 mg, 1 g, 1.25 g, 1.5 g); IV
only. Infuse over ≥60 min; ≤10 mg/min associated with fewer infusion reactions; ≤5 mg/mL in adults (2.1).

## Model and parameters
No compartmental model. Values (12.3):

```yaml
parameters:
  - {name: conc_1g_60min, value: "approx 63 / 23 / 8", unit: mcg/mL, source: "12.3", note: "multiple 1 g (15 mg/kg) doses over 60 min: end of infusion / 2 h after / 11 h after end of infusion; normal renal function"}
  - {name: conc_500mg_30min, value: "about 49 / 19 / 10", unit: mcg/mL, source: "12.3", note: "end of infusion / 2 h / 6 h after"}
  - {name: distribution_coefficient, value: "0.3 to 0.43", unit: L/kg, source: "12.3 Distribution"}
  - {name: protein_binding, value: "approximately 55", unit: "% bound", source: "12.3 Distribution", note: "by ultrafiltration, at serum vancomycin 10-100 mcg/mL; temperature and centrifugal conditions not stated"}
  - {name: CL_plasma, value: 0.058, unit: L/kg/h, source: "12.3 Elimination", note: "mean; (derived: 4.1 L/h at 70 kg)"}
  - {name: CL_renal, value: 0.048, unit: L/kg/h, source: "12.3 Elimination"}
  - {name: t_half, value: "4 to 6", unit: h, source: "12.3", note: "normal renal function"}
  - {name: t_half_anephric, value: 7.5, unit: days, source: "12.3"}
  - {name: urinary_excretion_24h, value: "about 75", unit: "% of dose", source: "12.3 Excretion", note: "glomerular filtration; no apparent metabolism"}
```

Dosing (2.2-2.4): adults 2 g/day as 500 mg q6h or 1 g q12h, initial daily dose no less than 15 mg/kg;
children ≥1 month 10 mg/kg q6h; neonates 15 mg/kg then 10 mg/kg q12h (first week) or q8h; renal impairment:
initial ≥15 mg/kg, guide by trough levels; anephric 15 mg/kg, level at 24 h.

## Pharmacodynamics and end points
12.2: in animal models activity correlates with AUC/MIC (including MRSA); the PK/PD index best tied to cure
has not been established in clinical trials. No target trough or AUC value is stated in the label. Not an
opioid: no MEAC, ke0 or potency ratio.

## Covariates
Renal function, age (elderly, neonatal postconceptional age), obesity mentioned qualitatively.

## Caveats
Binding figure is a single method (ultrafiltration) without stated conditions; volumes and clearance are
old summary means.

## Used by stanpumpR
- R/drugs_vancomycin.R:68 (header, threshold section): the label's "about 55% bound" comes from
  ultrafiltration at room temperature or high g-force and overstates binding; fu taken as 0.70 (Dejaco 2026).
- inst/help/drugs/vancomycin.md:11: the same statement.

## Verification against stanpumpR
- **"about 55% bound" from ultrafiltration (vancomycin.R:68; vancomycin.md:11)**: label: approximately 55%
  bound, by ultrafiltration, at 10-100 mcg/mL. **Matches** for the value and the method ("about" paraphrases
  "approximately"). The further claim that this ultrafiltration was at room temperature or high g-force
  **cannot be verified from the label**, which states no conditions; that attribution must rest on Dejaco 2026
  or other secondary sources.
- Context (not cited locations): label CL 0.058 L/kg/h (~4.1 L/h at 70 kg) vs Thomson 2.99 L/h at CrCl 66
  mL/min; label distribution 0.3-0.43 L/kg vs Thomson V1+V2 1.41 L/kg. The label's older, normal-renal-function
  figures differ from the hospitalised-patient model; this is a difference of sources, not a transcription
  error. The threshold 1.4 mcg/mL (CSV endCe, row 53) uses fu 0.70, not the label; with the label's 55% bound
  (fu 0.45) it would be about 2.2 (derived), as the header itself notes.
