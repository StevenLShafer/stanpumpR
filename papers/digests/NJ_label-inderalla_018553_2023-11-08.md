---
file: NJ_label-inderalla_018553_2023-11-08.pdf
pmid: null
doi: null
citation: "ANI Pharmaceuticals. Inderal LA (propranolol hydrochloride) Long-Acting Capsules, NDA 018553 (supplements S-048, S-052). FDA-approved labeling, Reference ID 5273910, signed 11/07/2023 (insert 10611 Rev 11/23); Drugs@FDA PDF 018553Orig1s048s052lbl.pdf."
kind: label
drugs: [propranolol]
source_read: "text layer, all 19 PDF pages: Description p1-2; Clinical Pharmacology and Pharmacokinetics and Drug Metabolism incl. special populations p2-5; drug interactions p5-7; Pharmacodynamics and Clinical Effects p7-8; Indications p8-9; Contraindications, Warnings, Precautions incl. Pregnancy, Nursing, Pediatric and Geriatric Use p9-14; Adverse Reactions, Overdosage p14-15; Dosage and Administration p15; How Supplied p16; p17 blank; p18 carton images (garbled text layer, not used); p19 FDA signature page. All PK numbers are prose."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Inderal LA FDA label, November 2023 (NDA 018553)

## Study
Not a study: the FDA-archived approved label for Inderal LA, sustained-release propranolol hydrochloride capsules 60, 80,
120, 160 mg (52.60, 70.14, 105.21, 140.28 mg base; PDF p1, p16), for once-daily dosing. The pharmacokinetic and dosing text
is the same as the later DailyMed SPL (`NJ_dailymed-35d28979`, revised 8/2026), including the hypoglycaemia counselling
paragraph (PDF p10).

## Model and parameters
No model. Label values (PDF p2-3):
```yaml
parameters:
  - {name: F_oral_propranolol, value: 0.25, unit: fraction, source: "Absorption, PDF p2", note: "on average about 25% (propranolol generally)"}
  - {name: Tmax_LA, value: 6, unit: h, source: "Absorption, PDF p3", note: "peak blood levels at about 6 h"}
  - {name: AUC_ratio_LA_vs_IR, value: "0.60-0.65", unit: ratio, source: "PDF p3", note: "steady state, 24 h, vs comparable divided daily dose of Inderal tablets"}
  - {name: t_half_apparent_LA, value: 10, unit: h, source: "PDF p3", note: "after about 12 h of fairly constant levels"}
  - {name: V, value: 4, unit: L/kg, source: "Distribution, PDF p3", note: approximate}
  - {name: protein_binding, value: 0.90, unit: fraction, source: "Distribution, PDF p3", note: "albumin and AAG"}
```

## Pharmacodynamics and end points
Little correlation between plasma levels and clinical effect in hypertension and angina; LA equivalent to the same mg of
conventional Inderal on 24-h BP and exercise heart rate, systolic pressure and rate-pressure product (PDF p2). Crossover
trials: hypertension (4 studies, 74 patients, LA 160 mg daily vs 160 mg propranolol once daily or 80 mg bid) and angina
(12 and 13 patients, LA 160 mg vs 40 mg qid) (PDF p7-8). No concentration target. Not an opioid.

## Covariates
LA not studied over 65 y or in renal or hepatic impairment (PDF p4-5). Propranolol data: cirrhosis (6 vs 7, long-acting
160 mg daily for 7 days) steady state 2.5-fold higher; after 10 mg IV half-life 7.2 vs 2.9 h (PDF p5). Elderly, sex, race
and renal-failure statements as in the IR label.

Dosing (PDF p15): hypertension 80 mg once daily initially, usual 120-160 mg, up to 640 mg; angina from 80 mg, average
optimum 160 mg; migraine 80 mg then 160-240 mg; HSS 80-160 mg.

## Caveats
- The page header warns the label may not be the latest approved; a 2026 SPL exists with the same PK text.
- No PK table or absorption rate constant.

## Used by stanpumpR
The repo does not link this PDF. The record it supports by identity, `propranolol__la_capsule__oral__catalog__none__label`,
is at `inst/extdata/antihypertensiveRegistry.csv:109` (source_url is the DailyMed SPL setid 35d28979, i.e.
`NJ_dailymed-35d28979`), and is named in `tests/testthat/test-antihypertensive-registry.R:165-167`,
`docs/antihypertensives.md:29-34` and `inst/help/models/antihypertensives.md:19` (IR and LA are separate records). The
context file marks the inclusion doubtful for this reason.

## Verification against stanpumpR
- Record limitations "peak near 6 h and AUC 60-65% of divided immediate-release at the same daily milligrams":
  **matches** this label too (PDF p3).
- "plasma levels correlate poorly with BP": **matches** in substance (PDF p2).
- IR and LA as distinct products (test :165-167; docs :29-31): consistent with the label's mg-for-mg warning (PDF p2, p15).
- The repo cites the DailyMed SPL, not this file; nothing in the code depends on this document.
