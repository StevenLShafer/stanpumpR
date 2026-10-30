---
file: NJ_dailymed-2c9b4d8f.pdf
pmid: ""
doi: ""
citation: "LASIX (furosemide) Tablets 20, 40 and 80 mg (Validus Pharmaceuticals LLC; NDA016273). DailyMed SPL setid 2c9b4d8f-0770-482d-a9e6-9c616a440b1a, label text dated August 2018 (60031-05), SPL revised 9/2025; retrieved 2026-10-21."
kind: label
drugs: [furosemide]   # not a stanpumpR drug model; antihypertensive registry catalogue record only
source_read: "text layer, all 15 PDF pages: Boxed warning, Description, Clinical Pharmacology (p. 1-2), Indications and Contraindications (p. 2), Warnings and Precautions incl. Pregnancy, Pediatric and Geriatric Use (p. 3-7), Adverse Reactions (p. 7-9), Overdosage and Dosage and Administration (p. 9-10), How Supplied (p. 10), display panels (p. 11), SPL product data (p. 12-15). Clean prose; nothing rendered."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# LASIX (furosemide) tablets, DailyMed SPL (Validus)

Old-format label. Brand product, NDA016273 (marketing start 04/24/1978), Validus Pharmaceuticals, product of Canada; tablets 20 mg (NDC 30698-067), 40 mg (-060), 80 mg (-066). Body text matches the FDA-archived label NJ_label-lasix_016273_2020-03-19 (same August 2018 text; that copy is 60031-04, this is 60031-05).

## Study
Not a study.

## Model and parameters
No population model. Clinical Pharmacology (p. 2):

```yaml
parameters:
  - {name: protein_binding, value: "91-99", unit: "%", source: "Clinical Pharmacology p. 2", note: "at 1-400 ug/mL; unbound 2.3-4.1% at therapeutic concentrations"}
  - {name: F_tablet, value: 64, unit: "%", source: "Clinical Pharmacology p. 2", note: "relative to IV, fasted normal men"}
  - {name: F_oral_solution, value: 60, unit: "%", source: "Clinical Pharmacology p. 2", note: "relative to IV"}
  - {name: absorption_time_solution, value: 50, unit: min, source: "Clinical Pharmacology p. 2"}
  - {name: absorption_time_tablet, value: 87, unit: min, source: "Clinical Pharmacology p. 2", note: "Cmax and AUC not different between tablet and solution"}
  - {name: t_half_terminal, value: 2, unit: h, source: "Clinical Pharmacology p. 2", note: "approximately"}
```

Glucuronide the main metabolite; more unchanged drug in urine after IV than oral; time to peak independent of dose; elderly men have lower renal clearance after IV (p. 2). No pediatric PK section (unlike the injection label). Hemodialysis does not accelerate elimination (p. 9).

## Pharmacodynamics and end points
Oral onset of diuresis within 1 h, peak in the first or second hour, duration 6-8 h (p. 2). No therapeutic or toxic concentration (toxic level not known, p. 9).

## Covariates
Age (renal clearance, descriptive).

## Caveats
- Dosage (p. 9-10): edema, adults 20-80 mg as a single dose, repeat or raise by 20-40 mg no sooner than 6-8 h, up to 600 mg/day in severe edema; pediatric 2 mg/kg, increments of 1-2 mg/kg no sooner than 6-8 h, not above 6 mg/kg. Hypertension, adults 80 mg/day usually as 40 mg twice daily; reduce other antihypertensives by at least 50% when adding.
- Indications: edema (adults and children) and hypertension (adults) (p. 2).

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:61 (8baa733): source_url of record furosemide__tablet__oral__catalog__none__label (official_label_descriptive, descriptive_only, matrix "plasma and urine"); data-raw/antihypertensiveRegistry.R:326-330; record named in tests/testthat/test-antihypertensive-registry.R:169-170, docs/antihypertensives.md:30 and inst/help/models/antihypertensives.md:19.

## Verification against stanpumpR
- Product and route identity (furosemide tablet, oral): **matches**.
- The record's note (the fitted population record uses a Weibull oral input and renal-function-dependent clearance; endpoint urinary output, not BP) refers to Van Wart 2013, not this label; nothing here contradicts it. No numeric value is taken from the label.
