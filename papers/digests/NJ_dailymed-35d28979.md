---
file: NJ_dailymed-35d28979.pdf
pmid: null
doi: null
citation: "ANI Pharmaceuticals. Inderal LA (propranolol hydrochloride) Long-Acting Capsules 60, 80, 120, 160 mg (NDA018553). DailyMed SPL setid 35d28979-36b1-4630-b85e-a44e0a443734; SPL revised 8/2026 (insert 10611 Rev 04/26); archived 2026-10-21."
kind: label
drugs: [propranolol]
source_read: "text layer, all 23 PDF pages (old non-PLR format): Description p1; Clinical Pharmacology, Pharmacokinetics and Drug Metabolism incl. special populations and drug interactions p1-7; Pharmacodynamics and Clinical Effects p7-8; Indications p8-9; Contraindications, Warnings, Precautions incl. Pregnancy, Nursing, Pediatric and Geriatric Use p9-14; Adverse Reactions p14-15; Overdosage p15; Dosage and Administration p15-16; How Supplied, carton text and SPL data p16-23. All PK numbers are prose; no table to render."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Inderal LA (propranolol extended-release capsules), DailyMed SPL, revised 8/2026

## Study
Not a study: the current brand SPL for Inderal LA, a once-daily sustained-release capsule of propranolol hydrochloride
(60, 80, 120, 160 mg HCl = 52.60, 70.14, 105.21, 140.28 mg base; PDF p1). NDA018553, labeler ANI (PDF p19-23). Its
pharmacokinetic text is the same as the FDA-archived 2023 label (`NJ_label-inderalla_018553_2023-11-08`).

## Model and parameters
No model. Label values (PDF p2-3, p5):
```yaml
parameters:
  - {name: F_oral_propranolol, value: 0.25, unit: fraction, source: "Absorption, PDF p3", note: "on average about 25% (propranolol generally)"}
  - {name: Tmax_LA, value: 6, unit: h, source: "Absorption, PDF p3", note: "peak blood levels at about 6 h"}
  - {name: AUC_ratio_LA_vs_IR, value: "0.60-0.65", unit: ratio, source: "Metabolism and Elimination, PDF p3", note: "steady state over 24 h vs comparable divided daily dose of Inderal tablets"}
  - {name: plateau, value: 12, unit: h, source: "PDF p3", note: "levels fairly constant about 12 h, then exponential decline"}
  - {name: t_half_apparent_LA, value: 10, unit: h, source: "PDF p3", note: "apparent plasma half-life"}
  - {name: V, value: 4, unit: L/kg, source: "Distribution, PDF p3", note: approximate}
  - {name: protein_binding, value: 0.90, unit: fraction, source: "Distribution, PDF p3", note: "albumin and AAG"}
```
The lower AUC is attributed to greater hepatic metabolism with slower absorption (PDF p3). Food effect not studied.
Not a mg-for-mg substitute for conventional propranolol; blood levels are lower (PDF p2, p15).

## Pharmacodynamics and end points
The label says that in hypertension and angina there is little correlation between plasma levels and clinical effect,
and LA was therapeutically equivalent to the same mg of conventional Inderal on 24-h BP and exercise responses (PDF p2).
Four crossover studies (74 patients) of LA 160 mg once daily vs propranolol 160 mg (PDF p7). No concentration target. Not an opioid.

## Covariates
LA not studied in patients over 65, or in renal or hepatic impairment (PDF p4-5). Propranolol data: elderly half-life 11 vs
5 h; renal failure Cmax 161 +/- 41 vs 26 +/- 1 ng/mL (40 mg oral); cirrhosis (6 vs 7, long-acting 160 mg daily for 7 days)
steady-state concentration 2.5-fold higher, IV 10 mg half-life 7.2 vs 2.9 h (PDF p4-5). Race and sex as in the IR label.

Dosing (PDF p15-16): hypertension 80 mg once daily initially, usual 120-160 mg, up to 640 mg; angina 80 mg, average optimum
160 mg, above 320 mg not established; migraine 80 mg, 160-240 mg; HSS 80-160 mg, all once daily.

## Caveats
- No pharmacokinetic table, no absorption rate constant; values are rounded label statements.
- The 25% bioavailability is the label's statement for propranolol generally; for LA only the AUC ratio to IR is given.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:109` (built by `data-raw/antihypertensiveRegistry.R:297-301`): record
  `propranolol__la_capsule__oral__catalog__none__label`, oral LA capsule, analyte propranolol, plasma, descriptive only,
  source_url this SPL; limitations "peak near 6 h and AUC 60-65% of divided immediate-release at the same daily
  milligrams, so the IR absorption model must not be reused. The label itself notes plasma levels correlate poorly with BP."
- Named in `tests/testthat/test-antihypertensive-registry.R:165-167`, `docs/antihypertensives.md:29-34` and
  `inst/help/models/antihypertensives.md:19` (IR and LA are separate records).

## Verification against stanpumpR
- "peak near 6 h": **matches** (PDF p3).
- "AUC 60-65% of divided immediate-release at the same daily milligrams": **matches** (PDF p3, steady state, 24 h).
- "The label itself notes plasma levels correlate poorly with BP": **matches** in substance (PDF p2 says little correlation
  between plasma levels and clinical effect in hypertension or angina).
- Separate record from IR, IR absorption not reused: consistent with the label's warning that LA is not a mg-for-mg
  substitute (PDF p2, p15).
