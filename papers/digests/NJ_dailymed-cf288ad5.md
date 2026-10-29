---
file: NJ_dailymed-cf288ad5.pdf
pmid: null
doi: null
citation: "Roche Pharmaceuticals. DEMADEX (torsemide) tablets 5, 10, 20, 100 mg and DEMADEX (torsemide) injection 10 mg/mL ampuls. DailyMed SPL setid cf288ad5-d4e7-4b0b-bfd0-41c5327e6984; label text Rev. April 2003, SPL revised 2/2007; archived 2026-10-21."
kind: label
drugs: [torsemide]
source_read: "text layer, all 15 PDF pages (old non-PLR format): Description and Clinical Pharmacology incl. Pharmacokinetics and Metabolism p1-2; Clinical Effects p2-3; Indications, Contraindications, Warnings p3-4; Precautions incl. Pregnancy, Nursing, Pediatric and Geriatric Use p4-8; Adverse Reactions p8-9; Overdosage, Dosage and Administration incl. IV infusion stability p9-10; How Supplied p10-11; SPL product data p11-15 (Revised 2/2007 on p15). PDF p1 checked on a rendered image."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# DEMADEX (torsemide) tablets and injection, Roche, US label (SPL 2/2007)

Torsemide is registry-only in stanpumpR (no drug model).

## Study
Not a study: the combined Roche label for torsemide tablets and the intravenous injection (10 mg/mL in polyethylene
glycol-400 and tromethamine; ampuls of 2 mL = 20 mg and 5 mL = 50 mg; PDF p1, p10-11). Torsemide is a
pyridine-sulfonylurea loop diuretic, MW 348.43, pKa 7.1 (PDF p1).

## Model and parameters
No model. Descriptive PK (Pharmacokinetics and Metabolism, PDF p1-2):

```yaml
parameters:
  - {name: F_oral, value: 0.80, unit: fraction, source: "PDF p1", note: "approximately; 90% CI 0.75-0.89; little first pass"}
  - {name: Tmax_oral, value: "<= 1", unit: h, source: "PDF p1-2", note: "food delays by about 30 min, AUC unchanged"}
  - {name: dose_proportional_range, value: "2.5-200", unit: mg, source: "PDF p2", note: "Cmax and AUC proportional"}
  - {name: Vd, value: "12-15", unit: L, source: "PDF p2", note: "normal adults, mild-moderate renal failure, CHF; about doubled in cirrhosis"}
  - {name: t_half, value: 3.5, unit: h, source: "PDF p2", note: "approximately, normal subjects"}
  - {name: fraction_hepatic_CL, value: 0.80, unit: fraction, source: "PDF p2", note: "approximately; renal about 20% with normal renal function"}
  - {name: protein_binding, value: ">0.99", unit: fraction, source: "PDF p2"}
```
(derived: CL ~ ln2 x 13.5 L / 3.5 h = 2.7 L/h, about 45 mL/min; not stated in the label.) In decompensated CHF total
clearance is about 50% of healthy; in renal failure renal clearance falls but total clearance is unchanged; in cirrhosis
Vd, half-life and renal clearance rise with total clearance unchanged; elderly lose renal clearance only (PDF p2). No
separate intravenous PK is given: the label states oral and IV doses are therapeutically equivalent because of the
high bioavailability (Dosage and Administration, PDF p9).

## Pharmacodynamics and end points
- Diuretic activity correlates better with urinary excretion rate than with blood concentration (Mechanism, PDF p1).
- IV: diuresis begins within 10 min, peaks within the first hour; oral: onset within 1 h, peak in the first or second
  hour; duration 6-8 h by either route (Clinical Effects, PDF p2). Sodium excretion linear in dose over 2.5-20 mg in
  healthy subjects.
- Hypertension: 5-10 mg once daily, near-maximal effect at 4-6 weeks, up to 12 weeks (PDF p2-3).
- Acute renal failure study at 520-1200 mg/day: seizures in 6/32 torsemide, 6/32 furosemide, 1/32 placebo (PDF p3).
- IV injection to be given slowly over 2 minutes, single doses not above 200 mg (Ototoxicity, PDF p4).
- Not an opioid; no concentration-effect parameters, no therapeutic range. Not dialyzable (Overdosage, PDF p9).

Dosing (PDF p9-10): CHF 10-20 mg once daily oral or IV, titrate by doubling, single doses above 200 mg not adequately
studied; chronic renal failure 20 mg; cirrhosis 5-10 mg with an aldosterone antagonist, above 40 mg not studied;
hypertension 5 mg, to 10 mg after 4-6 weeks. IV as a 2-minute bolus or continuous infusion (200 mg in 250 mL D5W or
0.9% NaCl, or 50 mg in 500 mL, stable 24 h; PDF p9-10).

## Covariates
Qualitative only: CHF, renal failure, cirrhosis, age (above). Pregnancy category B; pediatric safety not established
(PDF p7-8).

## Caveats
- Historical label (text 2003, SPL 2007); the injection is the product of interest and its PK is not separately
  described.
- No study sizes are given for the PK statements.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:123` (from `data-raw/antihypertensiveRegistry.R:341-345`): record
  `torsemide__injection__iv__catalog__none__label`, IV, "injection (historical US product)", analyte torsemide, matrix
  plasma and urine, descriptive only, no stanpumpR drug.

## Verification against stanpumpR
- "A historical US label establishes the ampule": **matches** (10 mg/mL ampuls, 20 mg and 50 mg, PDF p1, p10-11; SPL
  dated 2/2007).
- "Oral F and Tmax do not apply; IV PK and the urinary response are missing": **matches**; the label gives IV onset
  and duration of diuresis but no IV PK parameters and no exposure-response model.
- Setid in the registry URL matches this file. Separation of the IV record from the oral record is consistent with the
  label's two dosage forms.
