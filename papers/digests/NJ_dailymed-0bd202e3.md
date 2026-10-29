---
file: NJ_dailymed-0bd202e3.pdf
pmid: ""
doi: ""
citation: "Nadolol Tablets, USP, 20, 40 and 80 mg. Alembic Pharmaceuticals Limited (ANDA211763). DailyMed SPL setid 0bd202e3-c673-4945-b828-1a5bc6a896ae; labelling text revised 09/2021, SPL revised 6/2023; retrieved 2026-10-21."
kind: label
drugs: [nadolol]   # not a stanpumpR drug model; antihypertensive registry catalogue row only
source_read: "text layer, all 15 PDF pages: Description and Clinical Pharmacology incl. PK (p1-3), Indications (p3-4), Contraindications and Warnings (p4-5), Precautions incl. renal, pregnancy, nursing, paediatric (p6-7), Adverse Reactions (p7-8), Overdosage (p8-9), Dosage and Administration incl. renal interval table (p9), How Supplied and package labels (p10-12), SPL product data (p12-15). Old-format label without numbered 12.x sections. PDF p9 (renal table) confirmed from the rendered page image"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# Nadolol tablets (Alembic), DailyMed SPL 0bd202e3

## Study
Generic US label for nadolol, a non-selective beta-blocker without intrinsic sympathomimetic or membrane-stabilising
activity, low lipophilicity (PDF p1-2). Indications: long-term management of angina pectoris; hypertension (PDF p3).

Dosing (PDF p9): angina, 40 mg once daily, increased by 40-80 mg every 3-7 days, usual 40-80 mg, up to 160-240 mg
once daily; hypertension, 40 mg once daily, usual 40-80 mg, up to 240-320 mg once daily; without regard to meals;
taper over 1-2 weeks on withdrawal. Renal failure, dose interval by creatinine clearance (mL/min/1.73 m2): above 50,
24 h; 31-50, 24-36 h; 10-30, 24-48 h; below 10, 40-60 h.

## Model and parameters
Clinical Pharmacology (PDF p3), descriptive:
- oral absorption variable, averaging about 30%; food does not change rate or extent.
- peak serum concentration usually 3-4 h after an oral dose.
- about 30% bound to plasma protein.
- not metabolised; excreted unchanged, mainly by the kidneys.
- half-life of therapeutic doses about 20-24 h, longer in renal failure; steady state in 6-9 days of once-daily dosing
  with normal renal function.
- removed by haemodialysis (Overdosage, PDF p8).

```yaml
parameters:
  - {name: F_oral, value: 0.30, unit: fraction, source: "Clinical Pharmacology, PDF p3", note: "about, variable"}
  - {name: tmax, value: "3-4", unit: h, source: "Clinical Pharmacology, PDF p3", note: serum}
  - {name: t_half, value: "20-24", unit: h, source: "Clinical Pharmacology, PDF p3", note: "therapeutic doses, normal renal function"}
  - {name: protein_binding, value: 30, unit: "%", source: "Clinical Pharmacology, PDF p3"}
  - {name: time_to_steady_state, value: "6-9", unit: days, source: "Clinical Pharmacology, PDF p3", note: once daily}
```

## Pharmacodynamics and end points
Beta-blockade shown by reduced resting and exercise heart rate and cardiac output, reduced systolic and diastolic
pressure, inhibition of isoproterenol tachycardia and of reflex orthostatic tachycardia (PDF p1). 40-320 mg/day
lowered standing and supine pressure with the effect lasting about 24 h after dosing (PDF p2). Renal blood flow and
GFR preserved. No dose-response or concentration-effect numbers.

## Covariates
Renal function: the dose-interval table above; half-life rises in renal failure. Neonatal bradycardia and
hypoglycaemia after maternal use at parturition; excreted in human milk (PDF p7). Paediatric safety not established.

## Caveats
Ranges and "about" values only; no clearance or volume; the label's renal interval table is the only quantitative
covariate.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:91` (generator `data-raw/antihypertensiveRegistry.R:135`): model
  `nadolol__tablet__oral__catalog__none__label`, oral tablet, analyte and matrix NA, `official_label_descriptive`,
  `descriptive_only`, BP simulation FALSE; limitations: "Active-molecule and tablet identity only; oral PK and resting
  BP are missing."

## Verification against stanpumpR
- Drug, active moiety and tablet formulation: **matches**.
- "Oral PK ... missing" with analyte and matrix NA: **discrepancy**. The label (PDF p3) gives descriptive oral PK of
  the kind other `official_label_descriptive` rows record (hydralazine row 66's half-life, for example): oral absorption
  about 30%, serum tmax 3-4 h, half-life 20-24 h, 30% protein binding, renal excretion unchanged, steady state in 6-9
  days, and a renal dose-interval table (PDF p9); the analyte is nadolol in serum. If "missing" means no fitted
  parameter vector, it is true, but the row does not say so.
- "Resting BP ... missing": the label gives no quantitative blood-pressure response, only that 40-320 mg/day lowers
  pressure for about 24 h: **matches**.
