---
file: NJ_dailymed-a3eb7294.pdf
pmid: null
doi: null
citation: "Hydrochlorothiazide Capsules, USP 12.5 mg. US prescribing information, Actavis Pharma (manufactured for Teva Pharmaceuticals; made by Watson Pharma Private Ltd); NDA authorized generic of NDA020504 (MICROZIDE), NDC 0591-0347; Rev. 12/2023. DailyMed setid a3eb7294-f913-434b-896c-da028bfebc73; downloaded 2026-10-21."
kind: label
drugs: [hydrochlorothiazide]
source_read: "text layer, all 9 PDF pages (form-feed chunks = pages; read in full). Old-format label with no numbered sections: Description and Clinical Pharmacology (p1), Pharmacokinetics and Metabolism, Pharmacodynamics, Clinical Studies, Indications (p2), Contraindications, Warnings (p3), Precautions incl. pregnancy, nursing, pediatric and elderly use (pp3-6), Adverse Reactions (pp6-7), Overdosage, Dosage and Administration, How Supplied (p7), SPL product data (pp8-9). Rendered page 1 to confirm identity."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Hydrochlorothiazide 12.5 mg capsules (Actavis/Teva authorized generic) label, 2023

## Study
This is the DailyMed SPL for Actavis's hydrochlorothiazide 12.5 mg capsules (imprint WATSON 347). It is an NDA
authorized generic of NDA020504, marketed since 12/27/1996, label revised 12/2023 (pp7-9). Page 1 is that product,
so there is no source file problem.

The text matches the MICROZIDE label (`NJ_label-hydrochlorothiazide_020504_2020-08-20`) word for word, apart from
the product name. The PK paragraph is identical.

The product is a capsule only, despite the registry's "tablet or capsule", and it is indicated for hypertension.

**Dosage (p7):** one capsule (12.5 mg) once daily for an adult, alone or combined. More than 50 mg/day is not
recommended. Elderly patients start at 12.5 mg and titrate in 12.5 mg steps (p6).

## Model and parameters
Descriptive values only (p2). There is no compartmental model, CL or V.
```yaml
parameters:
  - {name: F (oral absorption), value: "65-75", unit: "%", source: "p2", note: "reduced in congestive heart failure"}
  - {name: Tmax, value: "1-5", unit: h, source: "p2"}
  - {name: Cmax range, value: "70-490", unit: ng/mL, source: "p2", note: "after 12.5 to 100 mg; plasma concentrations linear with dose"}
  - {name: whole blood/plasma ratio, value: "1.6-1.8", unit: ratio, source: "p2"}
  - {name: protein binding, value: "40-68", unit: "%", source: "p2"}
  - {name: plasma elimination t1/2, value: "6-15", unit: h, source: "p2", note: "prolonged in renal disease"}
  - {name: urinary recovery, value: "55-77", unit: "% of dose", source: "p2", note: ">95% of absorbed dose unchanged in urine"}
  - {name: food effect, value: "F -10%, Cmax -20%", unit: "", source: "p2", note: "Tmax 1.6 to 2.9 h fasted to fed"}
```

## Pharmacodynamics and end points
- **Time course (p2):** onset within 2 h, peak effect at about 4 h, duration up to 24 h. Normal blood pressure is
  not affected.
- **Clinical study:** an 87-patient, 4-week placebo-controlled trial showed a significant fall in seated systolic
  and diastolic pressure. In published trials, 12.5 mg preserved most of the placebo-corrected fall seen with
  25 mg.
- **Concentrations:** the label gives no concentration-effect relation and no therapeutic range.

## Covariates
Qualitative only:
- Renal disease raises concentrations and prolongs the half-life.
- CHF reduces absorption.
- Resins reduce absorption: cholestyramine by up to 85%, colestipol by up to 43% (p4).
- The elderly may show a greater blood-pressure response.

## Caveats
- The values are ranges with no source study, n or assay.
- It is a 12.5 mg capsule label, so it says nothing about tablets of other strengths.
- Postmarketing (p7): an increased risk of non-melanoma skin cancer, mainly squamous cell carcinoma at high
  cumulative dose.

## Used by stanpumpR
`inst/extdata/antihypertensiveRegistry.csv:67` (generated from `data-raw/antihypertensiveRegistry.R:240-244`),
record `hydrochlorothiazide__tablet__oral__catalog__none__label`:
- formulation "tablet or capsule", matrix plasma;
- `official_label_descriptive`, `descriptive_only`, BP simulation FALSE;
- limitations: "Label F 65-75%, Tmax 1-5 h, which is the independent check on the implemented model's 2.3 h",
  with the fitted Van Wart record kept separate and no chronic BP coefficients.

`tests/testthat/test-drugs-hydrochlorothiazide.R:245-257` points to this catalog row for the 1-5 h check.

## Verification against stanpumpR
- **F 65-75%:** the label (p2) says "well absorbed (65% to 75%)". **matches**
- **Tmax 1-5 h:** the label (p2) says peak within 1 to 5 hours. **matches**
- **The implemented model's 2.3 h:** a model output, which the label does not address. It lies between the label's
  fasted 1.6 h and fed 2.9 h. **consistent**
- **No BP coefficients and descriptive only:** the label gives onset, peak and duration of the antihypertensive
  effect but no concentration-effect relation. **matches**
- **Formulation "tablet or capsule":** this document covers only a 12.5 mg capsule. The row is an ingredient-level
  catalog entry, so the wider wording is not wrong, but no tablet label is behind it. **matches (note)**
