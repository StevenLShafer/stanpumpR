---
file: NJ_label-angiomax_020873_2016-03-22.pdf
pmid: null
doi: null
citation: "ANGIOMAX (bivalirudin) for injection, for intravenous use. US prescribing information, NDA 020873 (supplement s036), revised 03/2016, The Medicines Company. FDA label PDF https://www.accessdata.fda.gov/drugsatfda_docs/label/2016/020873s036lbl.pdf (Reference ID 3905651)."
kind: label
drugs: [bivalirudin]
source_read: "text layer, all 20 pages (Highlights, sections 1-17); page image rendered and read for PDF page 14 only (12.2, 12.3, Table 6); Table 6 confirmed on the rendered page 14 because the text layer misaligns its rows"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: discrepancy
open_questions:
  - "[source] R/drugs_bivalirudin.R:30-31 says bivalirudin is 'about 20% renally cleared', attributed in context to the label; the label (8.6, PDF p11-12) says only that clearance falls ~20% in moderate and severe renal impairment and gives no renal fraction. Which source supports a 20% renal fraction (e.g. Robson 2002 or the FDA review), or should the sentence be reworded?"
---

# Angiomax label 2016: bivalirudin prescribing information

## Study
Not a study: US prescribing information for Angiomax (bivalirudin, a 20-amino-acid synthetic direct thrombin
inhibitor, MW 2180 Da; section 11, PDF p12). Single-dose vial of 250 mg (as ~275 mg trifluoroacetate salt).
PK data in 12.3 are from patients undergoing PTCA, including a renal-impairment study; the label gives no n,
assay, or model for the PK numbers. Clinical studies (14.1): REPLACE-2 (6002 ITT PCI patients, median weight
85.5 kg, range 35-199), BAT (4312 unstable-angina PTCA patients, median 80 kg), AT-BAT (51 HIT/HITTS patients).

Dosing (2.1-2.2, PDF p2-3; confirmed on Highlights p1 text):
- PCI/PTCA (with or without HIT/HITTS): 0.75 mg/kg IV bolus, then 1.75 mg/kg/h for the procedure; ACT 5 min
  after the bolus, extra 0.3 mg/kg bolus if needed. May continue up to 4 h post-procedure (to be considered in
  STEMI); thereafter 0.2 mg/kg/h for up to 20 h if needed.
- Renal: no bolus reduction; CrCl 30-59 mL/min 1.75 mg/kg/h; CrCl <30 mL/min consider 1 mg/kg/h; hemodialysis
  0.25 mg/kg/h.
- Preparation: 5 mg/mL (250 mg in 50 mL) or 0.5 mg/mL for the low-rate infusion; Table 1 (PDF p4) is a
  weight-band dosing table (not re-checked against images; not used by stanpumpR).

## Model and parameters
No compartmental model. Numbers given (12.3, PDF p14, rendered page read):
- Linear PK after IV dosing in PTCA patients.
- Mean steady-state concentration 12.3 ± 1.7 mcg/mL after 1 mg/kg bolus + 2.5 mg/kg/h for 4 h.
- No plasma protein binding (other than thrombin), none to red cells.
- Elimination: renal mechanisms plus proteolytic cleavage; half-life 25 min in normal renal function.
- Hemodialysis removes approximately 25%.

Table 6 (PDF p14, rendered; the text layer shifts the clearance column down one row):

| Renal function (GFR, mL/min) | CL (mL/min/kg) | t½ (min) |
|---|---|---|
| Normal (≥90) | 3.4 | 25 |
| Mild (60-89) | 3.4 | 22 |
| Moderate (30-59) | 2.7 | 34 |
| Severe (10-29) | 2.8 | 57 |
| Dialysis-dependent (off dialysis) | 1.0 | 3.5 hours |

Section 8.6 (PDF p11-12): clearance reduced ~20% in moderate and severe impairment and ~80% in
dialysis-dependent patients. Whether the t½ values are terminal or single-exponential is not stated; whether
they are means or medians is not stated.

```yaml
parameters:
  - {name: Css, value: 12.3, unit: mcg/mL, source: "12.3, PDF p14", note: "mean ± 1.7; 1 mg/kg bolus + 2.5 mg/kg/h x 4 h, PTCA patients"}
  - {name: CL_normal, value: 3.4, unit: mL/min/kg, source: "Table 6, PDF p14", note: "GFR >=90 mL/min (derived: 0.204 L/h/kg)"}
  - {name: CL_mild, value: 3.4, unit: mL/min/kg, source: "Table 6, PDF p14", note: "GFR 60-89"}
  - {name: CL_moderate, value: 2.7, unit: mL/min/kg, source: "Table 6, PDF p14", note: "GFR 30-59"}
  - {name: CL_severe, value: 2.8, unit: mL/min/kg, source: "Table 6, PDF p14", note: "GFR 10-29"}
  - {name: CL_dialysis, value: 1.0, unit: mL/min/kg, source: "Table 6, PDF p14", note: "dialysis-dependent, off dialysis"}
  - {name: thalf_normal, value: 25, unit: min, source: "12.3 and Table 6, PDF p14"}
  - {name: thalf_mild, value: 22, unit: min, source: "Table 6, PDF p14"}
  - {name: thalf_moderate, value: 34, unit: min, source: "Table 6, PDF p14"}
  - {name: thalf_severe, value: 57, unit: min, source: "Table 6, PDF p14"}
  - {name: thalf_dialysis, value: 3.5, unit: h, source: "Table 6, PDF p14"}
  - {name: fraction_removed_by_hemodialysis, value: 0.25, unit: fraction, source: "12.3, PDF p14", note: approximately}
```

## Pharmacodynamics and end points
- End points: ACT, aPTT, PT, TT prolonged in a dose- and concentration-dependent way (12.2, PDF p14). Immediate
  effect after IV dosing; coagulation times return to baseline ~1 h after stopping.
- In 291 PTCA patients, 1 mg/kg + 2.5 mg/kg/h x 4 h then 0.2 mg/kg/h gave maximal ACT >300 s in all patients.
- REPLACE-2 (14.1, PDF p15-16): 0.75 mg/kg + 1.75 mg/kg/h; median 5-min ACT 358 s (IQR 320-400), <225 s in 3%;
  end-of-procedure median 334 s (Hemochron). AT-BAT: median ACT at device activation 379 s (high dose) and
  317 s (lower dose). BAT: median ACT 345 s at 5 min, 346 s at 45 min.
- No therapeutic or toxic plasma concentration range, no EC50, no ke0/tPeak. Bivalirudin affects INR (5.4).
- Not an opioid: no MEAC or potency ratio applies.

## Covariates
Renal function (GFR category; Table 6, dosing in 2.2). Doses are per kg of body weight. Elderly: more bleeding
(8.5). Pediatric safety not established.

## Caveats
Label summary values without n, variability (except Css SD), assay or method. Table 6 clearances for moderate
(2.7) and severe (2.8) are non-monotone as printed. The PK population (PTCA patients) is not the same as the
REPLACE-2 PCI population. Text-layer extraction of Table 6 is wrong (values shifted by one row); use the
rendered page.

## Used by stanpumpR
- `R/drugs_bivalirudin.R:23-26, 29-37` — label terminal half-life 25 min compared with the Zhang 2012 model;
  label reports lower clearance in renal impairment/dialysis and 3.4 mL/min/kg (0.204 L/h/kg) in PCI patients,
  not spliced in.
- `inst/help/drugs/bivalirudin.md:7, 19` — same comparisons in the help page.
- `tests/testthat/test-drugs-bivalirudin.R:55` — comment noting label terminal half-life 25 min.
- `inst/extdata/therapeuticRanges.csv:154` — registry row source ("Angiomax label (2016): clotting-time targets
  only").

## Verification against stanpumpR
- **25 min half-life** (`R/drugs_bivalirudin.R:24-26`, `inst/help/drugs/bivalirudin.md:7`,
  `tests/testthat/test-drugs-bivalirudin.R:55`): label 12.3 and Table 6 give 25 min in normal renal function
  (rendered p14). The label calls it "half-life", not explicitly "terminal half-life"; the paraphrase is
  reasonable. **matches**.
- **3.4 mL/min/kg = 0.204 L/h/kg** (`R/drugs_bivalirudin.R:32-33`, help line 19): Table 6 gives 3.4 mL/min/kg
  for normal and mild renal function (rendered p14). 3.4 × 60 / 1000 = 0.204 L/h/kg (derived, correct).
  The label's PK patients are PTCA patients; the code says "PCI patients" — a minor wording difference (PTCA is a
  form of PCI). **matches**.
- **Lower clearance in moderate/severe impairment and on dialysis** (`R/drugs_bivalirudin.R:31-32`, help line
  19): label 8.6 and Table 6 support this (2.7, 2.8, 1.0 mL/min/kg). **matches**.
- **"about 20% renally cleared"** (`R/drugs_bivalirudin.R:30-31`): the label gives no fraction renally excreted;
  its "approximately 20%" is the fall in clearance in moderate/severe impairment (8.6, PDF p11-12). The label
  does not support this sentence as written. **discrepancy** (see open_questions).
- **therapeuticRanges.csv:154** (no established range; clotting-time targets only, ACT 5 min after bolus): the
  label gives no therapeutic concentration range and specifies an ACT 5 min after the bolus (2.1). It does
  report a mean Css of 12.3 mcg/mL on the higher BAT regimen (12.3), which is a reported exposure, not a target,
  and is consistent with the row's mention of 12-15 mcg/mL from EACTS 2024 (not checked here). **matches**.
- Zhang 2012 parameters in the model are not from this label and were not checked here.
