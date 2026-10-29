---
file: NJ_dailymed-ed6c506c.pdf
pmid: null
doi: null
citation: "Hikma Pharmaceuticals USA Inc. Phentolamine Mesylate for Injection, USP, 5 mg/vial (IV/IM), ANDA040235. DailyMed SPL setid ed6c506c-5535-4b7c-ae1a-d63f09d796c9; label text revised May 2022, SPL revised 3/2024; archived 2026-10-21."
kind: label
drugs: [phentolamine]
source_read: "text layer, all 9 PDF pages (Description and Clinical Pharmacology p1; Indications p1-2; Contraindications, Warnings, Precautions incl. Pregnancy, Nursing, Pediatric Use p2-3; Adverse Reactions and Overdosage p3-4; Dosage and Administration p4-5; How Supplied p5-6; carton panels and SPL product data p6-9). Pages 1 and 5 checked on rendered images."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Hikma phentolamine mesylate for injection: US label (DailyMed)

## Study
Not a study: an old-format (non-PLR) generic US label, no numbered sections and no 12.1-12.3 subsections. Product:
lyophilized phentolamine mesylate 5 mg with mannitol 25 mg per vial, for IV or IM use (p1); salt
C17H19N3O·CH4O3S, M.W. 377.47 (p1). Hikma, ANDA040235, marketing start 05/15/1998 (p9). The label prose ends
"Revised May 2022" (p6); the SPL footer reads "Revised: 3/2024" (p9). How Supplied gives 2 mL vials (p5), while the
SPL packaging rows give "1 mL in 1 VIAL" and a strength of 5 mg in 1 mL (p8-9): a label-internal inconsistency
(fill vs vial size), not material to PK.

No PK study, population, assay, n or sampling is described anywhere in the label.

## Model and parameters
No model. The whole PK content is two sentences in Clinical Pharmacology (p1):
- half-life in the blood of 19 min after intravenous administration (no n, no assay, no matrix detail beyond
  "blood", no indication of whether it is a terminal or a single-exponential half-life);
- about 13% of a single IV dose recovered unchanged in urine.

No CL, V, protein binding, metabolism, bioavailability (IM), tmax or Cmax is given. No therapeutic or toxic
concentration is given.

```yaml
parameters:
  - {name: t_half_blood, value: 19, unit: min, source: "Clinical Pharmacology, PDF p1", note: "after IV administration; no n, assay or model given"}
  - {name: fe_urine_unchanged, value: 0.13, unit: fraction of dose, source: "Clinical Pharmacology, PDF p1", note: "approximate; single IV dose"}
  - {name: MW_mesylate_salt, value: 377.47, unit: g/mol, source: "Description, PDF p1", note: "salt, not base"}
```

## Pharmacodynamics and end points
Alpha-adrenergic blockade "of relatively short duration", with weaker direct positive inotropic and chronotropic
effects and vascular smooth-muscle vasodilation (p1). No concentration-effect relationship.

Phentolamine blocking test (Dosage and Administration, p4-5), end point arterial blood pressure in hypertensive
patients:
- IV: 5 mg adult, 1 mg child, injected rapidly; BP every 30 s for 3 min then every 60 s for 7 min. Positive:
  fall of more than 35 mm Hg systolic and 25 mm Hg diastolic; typical positive response 60/25 mm Hg; maximal effect
  usually within 2 min of injection; return to preinjection pressure commonly within 15-30 min (p5).
- IM: 5 mg adult, 3 mg child; BP every 5 min for 30-45 min; positive if the fall is at least 35/25 mm Hg within
  20 min (p5).

These are clinical-response timings in pheochromocytoma screening, not a fitted onset or ke0.

Dosing (p4): pheochromocytoma, 5 mg (children 1 mg) IV or IM 1-2 h before surgery, repeated if needed, and 5 mg
(children 1 mg) IV during surgery as indicated; norepinephrine extravasation, 10 mg added per litre of
norepinephrine solution for prevention, or 5-10 mg in 10 mL saline infiltrated within 12 h for treatment.

## Covariates
None. Pediatric use refers to Dosage and Administration only (fixed 1 mg IV or 3 mg IM doses; p3-5). No renal,
hepatic, age or weight information. Pregnancy category C, animal data only (p2-3).

## Caveats
- One unreferenced half-life with no study behind it; the 13% urinary recovery implies mostly non-renal elimination
  but the label names no pathway.
- The label says the urinary catecholamine assays have largely replaced the blocking test (Warnings, p2).
- The response timings come from a diagnostic test in patients with sustained catecholamine excess, so they describe
  that population, not general use.
- Two revision dates (May 2022 prose, 3/2024 SPL) and the 1 mL vs 2 mL vial wording, as above.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:103` (at 8baa733): registry record
  `phentolamine__injection__iv__catalog__none__label`, route iv, matrix blood, `pk_evidence`
  `official_label_descriptive`, `pd_evidence` `proposed_unfitted`, `execution_status` `descriptive_only`,
  `bp_simulation_enabled` FALSE, `stanpumpr_drug` NA, `source_url` this setid. The record is built by
  `data-raw/antihypertensiveRegistry.R:455-459`. Phentolamine is not a selectable stanpumpR drug (no other hit for
  "phentolamine" in the repo at 8baa733).

## Verification against stanpumpR
- Registry `limitations` text: "label blood half-life about 19 min" matches the label's 19 min after IV (p1);
  "maximal diagnostic-test effect usually within 2 min" matches the IV blocking-test interpretation (p5); "No CL, no
  V, no fitted response" is correct, the label gives none. **matches**.
- `matrix` blood matches the label's "half-life in the blood". **matches**.
- `route` iv, formulation injection: the label covers both IV and IM; the registry records only the IV route, which
  is the route the 19 min half-life refers to. Not an error, just narrower than the label. **matches**.
- `source_url` setid ed6c506c-5535-4b7c-ae1a-d63f09d796c9 is this document (Hikma, ANDA040235). **matches**.
- The registry does not use the 13% urinary recovery, the IM test, or the doses; nothing to check there.
