---
file: NJ_dailymed-59cd3331.pdf
pmid: null
doi: null
citation: "Indapamide Tablets, USP 2.5 mg (film coated), DailyMed SPL, repackager Preferred Pharmaceuticals Inc. (NDC 68788-7247), manufactured for Teva (ANDA074722; text Rev. A 10/2022), SPL revised 5/2026. https://dailymed.nlm.nih.gov/dailymed/lookup.cfm?setid=59cd3331-7afe-432a-8b50-39533fd5f392"
kind: label
drugs: [indapamide]
source_read: "text layer, all 12 PDF pages (Description and Clinical Pharmacology pp. 1-2; Indications p. 2; Contraindications, Warnings, Precautions incl. Pediatric and Geriatric Use pp. 3-6; Adverse Reactions pp. 6-9; Overdosage and Dosage and Administration p. 9; How Supplied and package label pp. 10-12); pp. 1 and 12 rendered"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Indapamide tablets (DailyMed SPL, Preferred Pharmaceuticals repack of Teva ANDA 074722)

The PDF is an old-format (non-PLR) generic label for indapamide 2.5 mg tablets, repackaged by Preferred
Pharmaceuticals (marketing start 08/20/2018, source NDC 0228-2571, imprint R;571), "Revised: 5/2026" on the SPL
(p. 12, rendered). The PDF does not print the DailyMed setid, so the link between this file and
setid 59cd3331-7afe-432a-8b50-39533fd5f392 rests on the archive's file name; the product, ingredient and labeler
are consistent with an indapamide SPL. The label text describes both 1.25 mg and 2.5 mg tablets, but this SPL
lists only the 2.5 mg product.

## Study
A label. Indapamide, an indoline thiazide-like diuretic; MW 365.84, weak acid pKa 8.8 (Description, p. 1).
Dosage and Administration (p. 9): hypertension 1.25 mg once daily in the morning, to 2.5 mg after 4 weeks,
then 5 mg after a further 4 weeks (consider adding another drug instead); edema of heart failure 2.5 mg, to
5 mg after 1 week. Doses of 5 mg or more add hypokalemia without more effect.

## Model and parameters
No model, no PK table. Clinical Pharmacology, pp. 1-2 (prose only):

```yaml
parameters:
  - {name: Cmax_blood_2.5mg, value: 115, unit: ng/mL, source: "Clinical Pharmacology, p. 1", note: "approximate peak in blood within 2 h, 2.5 mg as two 1.25 mg tablets, male subjects"}
  - {name: Cmax_blood_5mg, value: 260, unit: ng/mL, source: "Clinical Pharmacology, p. 1", note: "approximate peak in blood within 2 h, 5 mg as two 2.5 mg tablets, healthy male subjects"}
  - {name: tmax, value: "<= 2", unit: h, source: "Clinical Pharmacology, p. 1"}
  - {name: t_half_whole_blood, value: 14, unit: h, source: "Clinical Pharmacology, p. 1", note: approximately}
  - {name: blood_plasma_ratio, value: "6 at peak; 3.5 at 8 h", unit: ratio, source: "Clinical Pharmacology, p. 1", note: "preferential, reversible erythrocyte uptake"}
  - {name: plasma_protein_binding, value: "0.71-0.79", unit: fraction, source: "Clinical Pharmacology, p. 1"}
  - {name: fe_unchanged_48h, value: 0.07, unit: fraction, source: "Clinical Pharmacology, pp. 1-2", note: "about 7% of dose unchanged in urine in 48 h; extensively metabolised"}
  - {name: renal_elimination, value: ">= 0.70", unit: fraction of dose, source: "Clinical Pharmacology, p. 1", note: "plus 23% gastrointestinal, probably including biliary"}
  - {name: t_half_14C_urinary, value: 26, unit: h, source: "Clinical Pharmacology, p. 2", note: "terminal half-life of urinary excretion of total radioactivity, biphasic"}
```

All concentrations are whole-blood values; no plasma Cmax, clearance, volume or bioavailability is given.

## Pharmacodynamics and end points
Dose-related blood-pressure effect over 1.25-10 mg/day (5 and 10 mg indistinguishable) and 0.5-5 mg/day in
other trials (2.5 and 5 mg indistinguishable) (pp. 1-2). Serum potassium fell by a mean 0.28, 0.61 and 0.76
mEq/L at 1.25, 5 and 10 mg; uric acid rose about 0.69 mg/100 mL (1.25 mg) or 1 mg/100 mL (2.5-5 mg). After
8 weeks at 1.25 mg: potassium -0.28, sodium -0.63, chloride -2.60 mEq/L (table, p. 8); glucose +6.47 mg/dL
at 1.25 mg (Precautions, p. 4).
No concentration-effect relationship.

## Covariates
No PK covariate data. Antihypertensive effect kept in renal impairment though diuresis falls with renal
function (p. 2); caution in severe renal disease and hepatic impairment (p. 4); severe hyponatremia with
hypokalemia mainly in elderly women (pp. 3, 6); pediatric safety not established.

## Caveats
- The 115 ng/mL (2.5 mg, "male subjects") and 260 ng/mL (5 mg, "healthy male subjects") figures come from
  different descriptions of the subjects and are not dose-proportional (derived ratio 2.26 for a 2x dose); the
  label gives no n or study detail.
- Whole-blood values are several-fold higher than plasma because of red-cell uptake (ratio 6:1 at peak).

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:70: model_id `indapamide__ir_tablet__oral__catalog__none__label`,
  immediate-release US tablet, analyte indapamide, matrix whole blood, pk_evidence `official_label_descriptive`,
  execution_status `descriptive_only`, no stanpumpR drug, source this SPL. No other indapamide location in R/,
  inst/help or inst/extdata at 8baa733.

## Verification against stanpumpR
- Registry limitation text: whole-blood half-life about 14 h = label (p. 1). **matches**.
- Reversible red-cell uptake with blood:plasma ratio near 6:1 at peak = label (p. 1; falls to 3.5:1 at 8 h,
  which the row does not mention). **matches**.
- `matrix` whole blood: the label's concentrations and half-life are whole-blood values. **matches**.
- `descriptive_only`, no fitted PK, `bp_simulation_enabled` FALSE: consistent with a label that has no model
  or PK table. **matches**.
- The row's statement about a non-US 1.5 mg sustained-release product is not in this label; **cannot verify**
  here (outside this document's scope).
