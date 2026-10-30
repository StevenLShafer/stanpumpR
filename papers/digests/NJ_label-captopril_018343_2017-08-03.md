---
file: NJ_label-captopril_018343_2017-08-03.pdf
pmid: null
doi: null
citation: "CAPOTEN (captopril tablets, USP) 12.5/25/50/100 mg prescribing information, NDA 018343 (s087), Par Pharmaceutical; R08/2017, FDA Reference ID 4133107; Drugs@FDA posting 2017-08-03. https://www.accessdata.fda.gov/drugsatfda_docs/label/2017/018343s087lbl.pdf"
kind: label
drugs: [captopril]
source_read: "text layer, all 14 pages: Boxed warning and Description (p1), Clinical Pharmacology incl. Pharmacokinetics and Pharmacodynamics (p1-3), Indications (p3), Warnings incl. Fetal Toxicity (p4-7), Precautions incl. Nursing Mothers and Pediatric Use (p7-11), Overdosage and Dosage and Administration incl. renal impairment (p12-13), How Supplied (p14)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: not-applicable
---

# CAPOTEN (captopril) US label, NDA 018343, 2017

## Study
Old-format (non-PLR) reference-listed-drug label, Par Pharmaceutical (manufacturer and distributor), revision R08/2017,
Reference ID 4133107; the file is the product its name says. Content is the same as the Hikma generic label
(NJ_dailymed-607687f4) in every section compared; differences are the brand name, "Pregnancy Category D" under Fetal
Toxicity (p6), "approximately 7000 patients" in Adverse Reactions, inactive ingredients, and the How Supplied details
(NDC 49884-793/794/795/796; desiccant-charcoal canister; store at or below 30 C).

## Model and parameters
No population model. Pharmacokinetics (PDF p2):
```yaml
parameters:
  - {name: time to peak blood level, value: 1, unit: h, source: "Pharmacokinetics, p2", note: "about one hour"}
  - {name: food effect on absorption, value: "-30 to -40", unit: "%", source: "Pharmacokinetics, p2"}
  - {name: minimal absorption, value: 75, unit: "%", source: "Pharmacokinetics, p2", note: "average, carbon-14"}
  - {name: urinary elimination in 24 h, value: ">95", unit: "% of absorbed dose", source: "Pharmacokinetics, p2", note: "40-50% unchanged; rest mostly disulfide dimer and captopril-cysteine disulfide"}
  - {name: plasma protein binding, value: "25-30", unit: "%", source: "Pharmacokinetics, p2"}
  - {name: half-life total radioactivity, value: "<3", unit: h, source: "Pharmacokinetics, p2"}
  - {name: half-life unchanged captopril, value: "<2", unit: h, source: "Pharmacokinetics, p2", note: "probably less than; not accurately determinable"}
  - {name: milk to maternal blood, value: 0.01, unit: ratio, source: "Nursing Mothers, p10"}
```

## Pharmacodynamics and end points
Blood pressure reduction usually maximal 60-90 min after a dose; duration dose related (p2). Dosing (p12-13): initial 25
mg b.i.d. or t.i.d.; usual 25-150 mg b.i.d./t.i.d.; maximum 450 mg/day; heart failure 25 mg t.i.d. (6.25-12.5 mg t.i.d.
if volume depleted); post-MI target 50 mg t.i.d.; diabetic nephropathy 25 mg t.i.d.; one hour before meals. Renal
impairment: reduce the initial dose and titrate slowly (p13), no numeric table.

## Covariates
Renal impairment (qualitative); pediatric use not established; neonatal hypotension.

## Caveats
Descriptive, mostly qualitative PK; no clearance, volume or IV data.

## Used by stanpumpR
Context file: tests/testthat/test-antihypertensive-registry.R:318, the registry model_id
captopril__ir_tablet__oral__catalog__none__label, flagged at the O-round review as a doubtful inclusion; URL formerly in
inst/extdata/antihypertensiveRegistry.csv.

## Verification against stanpumpR
- At 8baa733 neither the NDA 018343 URL (018343s087lbl) nor "Capoten" appears anywhere in the repository (git grep). The
  captopril registry row (inst/extdata/antihypertensiveRegistry.csv:21; data-raw/antihypertensiveRegistry.R:154) now
  cites the DailyMed Hikma SPL (setid 607687f4), and the test that names the model_id is at
  tests/testthat/test-antihypertensive-registry.R:474, not 318. This label is not cited. **not-applicable**
- For the record, the row's label facts (unchanged-parent half-life under 2 h, BP peak at 60-90 min) are also stated here
  (p2), so the change of source does not change them.
