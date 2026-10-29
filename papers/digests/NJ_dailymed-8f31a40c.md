---
file: NJ_dailymed-8f31a40c.pdf
pmid: null
doi: null
citation: "Bryant Ranch Prepack (repackager; distributed by Unichem Pharmaceuticals (USA)). Propranolol Hydrochloride Tablets, USP (ANDA078955), 20 mg package. DailyMed SPL setid 8f31a40c-7c79-457b-af6e-d945e4807cff; SPL revised 3/2026 (insert dated May 2025); archived 2026-10-21."
kind: label
drugs: [propranolol]
source_read: "text layer, all 19 PDF pages (old non-PLR format, no numbered sections): Description p1; Clinical Pharmacology incl. Pharmacokinetics and Drug Metabolism (absorption, distribution, metabolism, enantiomers, special populations, drug interactions) p1-6; Pharmacodynamics and Clinical Effects p6-8; Indications p8-9; Contraindications, Warnings p9-11; Precautions incl. Pregnancy, Nursing, Pediatric and Geriatric Use p11-14; Adverse Reactions p14-15; Overdosage p15; Dosage and Administration p15-17; How Supplied and SPL data p17-19. All PK numbers are prose; no table to render."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Propranolol HCl tablets (generic, immediate release), DailyMed SPL, revised 3/2026

## Study
Not a study: a repackager's SPL (Bryant Ranch Prepack, 20 mg tablets, NDC 71335-3097, ANDA078955, marketing start
09/19/2024; PDF p17-19) of Unichem's immediate-release propranolol hydrochloride tablets. The description lists 10, 20,
40, 60 and 80 mg tablets (PDF p1); the package data cover 20 mg only. Strengths are of the hydrochloride (MW 295.80; basis
of strength propranolol hydrochloride, PDF p18). The text follows the Inderal tablet label (compare
`NJ_label-inderal_016418_2011-03-22`).

## Model and parameters
No model. Label values (Pharmacokinetics and Drug Metabolism, PDF p2-3):
```yaml
parameters:
  - {name: F_oral, value: 0.25, unit: fraction, source: "Absorption, PDF p2", note: "on average about 25%; high hepatic first pass; almost completely absorbed"}
  - {name: Tmax, value: "1-4", unit: h, source: "Absorption, PDF p2", note: "after an oral dose"}
  - {name: food_effect_F, value: "+50%", unit: relative, source: "Absorption, PDF p2", note: "protein-rich food; no change in Tmax, binding or half-life"}
  - {name: protein_binding, value: 0.90, unit: fraction, source: "Distribution, PDF p2", note: "albumin and AAG, enantioselective"}
  - {name: V, value: 4, unit: L/kg, source: "Distribution, PDF p2", note: approximate}
  - {name: t_half, value: "3-6", unit: h, source: "Metabolism and Elimination, PDF p3", note: plasma}
  - {name: metabolic_route_split, value: "42/41/17", unit: "%", source: "PDF p2", note: "ring hydroxylation / N-dealkylation-side-chain oxidation / glucuronidation"}
```
Enantiomers (PDF p3): S(-) about 100 times as potent as R(+) at beta receptors; after oral racemate S(-) concentrations
exceed R(+) by 40-90%.

## Pharmacodynamics and end points
No concentration-effect relationship or therapeutic range is given. Clinical-effect summaries only (hypertension,
angina, AF, post-MI BHAT, migraine, tremor, HSS, pheochromocytoma; PDF p6-8). Overdose: glucagon 50-150 mcg/kg IV then
1-5 mg/h (PDF p15). Not an opioid.

## Covariates
Special populations (PDF p3-4): elderly (12 vs 12) S(-) clearance lower, half-life 11 vs 5 h; clearance falls with age by
oxidation, not conjugation; testosterone raises clearance in men (conflicting studies); African-American men R(+)/S(-)
clearance about 76%/53% higher than Caucasian; Chinese 18-45% higher unbound fraction; chronic renal failure Cmax 161 +/- 41
vs 26 +/- 1 ng/mL in healthy (40 mg oral, 5 vs 5), not dialyzable; cirrhosis (7 vs 9, 80 mg q8h) unbound steady state 3-fold
higher, half-life 11 vs 4 h. CYP2D6 EM vs PM: no difference in oral clearance or half-life. Substrate of CYP2D6, 1A2, 2C19.

Dosing (PDF p15-17): hypertension 40 mg bid initially, usual 120-240 mg/day, up to 640 mg/day; angina 80-320 mg/day;
AF 10-30 mg tid-qid; post-MI 180-240 mg/day divided; migraine 80 mg/day initially, 160-240 mg/day; tremor 40 mg bid,
usually 120 mg/day; HSS 20-40 mg tid-qid; pheochromocytoma 60 mg/day for 3 days preoperatively, 30 mg/day if inoperable.

## Caveats
- A generic repackager's SPL, not the brand label; old (non-PLR) format with no pharmacokinetic table.
- Half-life, F and Tmax are rounded label statements, not study estimates. Analyte is racemic propranolol.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:107` (built by `data-raw/antihypertensiveRegistry.R:292-296`): record
  `propranolol__ir_tablet__oral__catalog__none__label`, oral IR tablet, analyte "propranolol (total and unbound)", plasma,
  `official_label_descriptive`, `descriptive_only`, BP simulation FALSE; source_url is this SPL; limitations "Label F about
  25%, Tmax 1-4 h, half-life 3-6 h. The one fitted human record is EXERCISE heart rate against total and unbound
  concentrations; its coefficients are not BP coefficients."
- The record is also named in `tests/testthat/test-antihypertensive-registry.R:165-167` (IR vs LA must be separate records).

## Verification against stanpumpR
- "Label F about 25%, Tmax 1-4 h, half-life 3-6 h" (CSV :107): **matches** (PDF p2-3).
- Route oral, formulation immediate-release tablet, descriptive only with no fitted parameters: **matches**.
- "total and unbound" analyte and the exercise-heart-rate remark refer to the Lalonde 1987 record (CSV :108), not to this
  label; the label does mention unbound concentrations (cirrhosis, Chinese subjects), so nothing conflicts. **Not
  applicable** to this label.
