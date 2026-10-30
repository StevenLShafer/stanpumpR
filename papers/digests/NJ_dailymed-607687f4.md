---
file: NJ_dailymed-607687f4.pdf
pmid: null
doi: null
citation: "Captopril Tablets, USP (12.5, 25, 50, 100 mg), Hikma Pharmaceuticals USA Inc., ANDA074505. DailyMed SPL setid 607687f4-63cf-411c-a20b-21d238a6b7a7, label Rev. 08/24, SPL revised 12/2024 (downloaded 2026-10-21)."
kind: label
drugs: [captopril]
source_read: "text layer, all 25 pages: Boxed warning and Description (p1), Clinical Pharmacology incl. Pharmacokinetics and Pharmacodynamics (p1-4), Indications (p4-5), Warnings incl. Fetal Toxicity (p5-9), Precautions incl. Nursing Mothers and Pediatric Use (p9-13), Overdosage and Dosage and Administration incl. renal impairment (p16-18), How Supplied and SPL product data (p18-25)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Captopril tablets (Hikma, ANDA074505), DailyMed label

## Study
Old-format (non-PLR) generic label; no numbered section 12. The document is what its name says: the Hikma captopril
tablet SPL, setid as in the URL, ANDA074505, marketing start 02/13/1996, labeler Hikma Pharmaceuticals USA Inc.

## Model and parameters
No population model, clearance or volume. Clinical Pharmacology, Pharmacokinetics (PDF p2):
```yaml
parameters:
  - {name: time to peak blood level, value: 1, unit: h, source: "Pharmacokinetics, p2", note: "about one hour after oral therapeutic doses"}
  - {name: food effect on absorption, value: "-30 to -40", unit: "%", source: "Pharmacokinetics, p2", note: "give one hour before meals"}
  - {name: minimal absorption, value: 75, unit: "%", source: "Pharmacokinetics, p2", note: "average, from carbon-14 labelling"}
  - {name: urinary elimination in 24 h, value: ">95", unit: "% of absorbed dose", source: "Pharmacokinetics, p2", note: "40-50% unchanged; rest mostly disulfide dimer and captopril-cysteine disulfide"}
  - {name: plasma protein binding, value: "25-30", unit: "%", source: "Pharmacokinetics, p2"}
  - {name: half-life total radioactivity, value: "<3", unit: h, source: "Pharmacokinetics, p2", note: "probably less than"}
  - {name: half-life unchanged captopril, value: "<2", unit: h, source: "Pharmacokinetics, p2", note: "accurate determination not possible; probably less than"}
  - {name: milk to maternal blood, value: 0.01, unit: ratio, source: "Nursing Mothers, p13", note: "about one percent"}
```
Renal impairment: retention of captopril; slower approach to and higher steady state for a given dose (Dosage
Adjustment in Renal Impairment, p18); no creatinine-clearance table. Removed by haemodialysis in adults, not by
peritoneal dialysis (Overdosage, p16).

## Pharmacodynamics and end points
Blood pressure reduction usually maximal 60-90 min after an oral dose; duration dose related; full effect may take
several weeks; additive with thiazides, less than additive with beta-blockers (Pharmacodynamics, p2). Antihypertensive
effect outlasts measurable inhibition of circulating ACE (p2). No concentration-effect relation.

Dosing (p16-18): hypertension 25 mg b.i.d. or t.i.d., up to 50 mg b.i.d./t.i.d., then with diuretic 100-150 mg
b.i.d./t.i.d.; usual 25-150 mg b.i.d. or t.i.d.; maximum 450 mg/day. Heart failure 25 mg t.i.d. (6.25-12.5 mg t.i.d. if
volume depleted), most satisfied at 50-100 mg t.i.d. Post-MI 6.25 mg test dose, 12.5 mg t.i.d., target 50 mg t.i.d.
Diabetic nephropathy 25 mg t.i.d. Take one hour before meals.

## Covariates
Renal impairment (qualitative dose reduction). Pediatric safety and effectiveness not established; neonates and infants
more susceptible to hypotension (p13). Boxed fetal-toxicity warning (p1, p8-9).

## Caveats
Descriptive label values, mostly qualitative ("probably less than"); "blood levels" without a stated matrix or assay;
no IV data, no clearance, no volume.

## Used by stanpumpR
inst/extdata/antihypertensiveRegistry.csv:21 (model_id captopril__ir_tablet__oral__catalog__none__label,
source_url = this SPL; also data-raw/antihypertensiveRegistry.R:154); the record is exercised in
tests/testthat/test-antihypertensive-registry.R:474 (refused as descriptive_only).

## Verification against stanpumpR
- antihypertensiveRegistry.csv:21 limitations: "Label unchanged-parent half-life under 2 h and a BP peak at 60-90 min"
  matches p2 (half-life "probably less than 2 hours"; BP reduction maximal at 60-90 min). Route oral, immediate-release
  tablet, source type official_label, pk_evidence official_label_descriptive, execution_status descriptive_only: all
  consistent with a label that gives no fitted parameters. **matches**
- "disulfide metabolites must not be assumed ACE-active": the label names the disulfide metabolites but says nothing of
  their activity; a repo caution, not a label statement. Not contradicted.
- "saturable binding to converting enzyme ... a linear fit is the wrong structure" and "58 unscreened candidate
  sources": not in the label; cannot verify here.
- "captopril is no longer in clinical use" (maintainer's decision, 2026-10-20): the label is a currently listed US
  generic (SPL revised 12/2024). A policy statement, not a label fact; noted, not scored.
- test-antihypertensive-registry.R:474 expects the record to be refused with "descriptive_only"; consistent with the
  registry row.
