---
file: NJ_dailymed-5b6f8a18.pdf
pmid: null
doi: null
citation: "Benicar (olmesartan medoxomil) tablets, film coated. DailyMed SPL setid 5b6f8a18-8f8a-4521-81c3-3a39c73c6646, labeler Cosette Pharmaceuticals, Inc. (NDC 0713-0860/0861/0862), NDA021286, marketing start 09/01/2022; prescribing information revised 2/2022, SPL revised 7/2023; downloaded 2026-10-21."
kind: label
drugs: [olmesartan]
source_read: "text layer, all 22 pages: Highlights (PDF p1-2), contents (p2-3), 1-2 Indications and Dosage (p3-5), 5 Warnings (p5-6), 8 Use in Specific Populations, 10 Overdosage, 11 Description (p9-12), 12.1-12.3 Clinical Pharmacology (p12-15), 16 How Supplied and package labels (p17-19), SPL product data (p19-22). Page 1 rendered to confirm identity."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Benicar (olmesartan medoxomil) DailyMed SPL, Cosette, 2022

The document is the brand Benicar label as distributed by Cosette Pharmaceuticals under NDA021286, not a generic.
Its sections 12.2-12.3 are identical, character for character apart from whitespace, to the Daiichi Sankyo
10/2019 label (NJ_label-benicar_021286_2019-10-18; checked by comparing the two text layers); the Highlights
differ in labeler, contact number and revision date.

## Study
Not a study. Regulatory label; hypertension in adults and children 6 y and over. Boxed warning: fetal toxicity.

## Model and parameters
No model. Section 12.3 (PDF p13-14):
- Prodrug completely hydrolysed to olmesartan during absorption; suspension bioequivalent.
- Absolute bioavailability about 26%; Cmax at 1 to 2 hours; no food effect.
- Volume of distribution about 17 L; 99% protein bound.
- Total plasma clearance 1.3 L/h, renal 0.6 L/h; 35% to 50% of the absorbed dose in urine.
- Biphasic elimination, terminal half-life about 13 hours; linear to 320 mg single and 80 mg multiple doses;
  steady state in 3 to 5 days.

```yaml
parameters:
  - {name: absolute bioavailability, value: 26, unit: "%", source: "12.3 p13", note: approximately}
  - {name: tmax, value: "1-2", unit: h, source: "12.3 p13"}
  - {name: volume of distribution, value: 17, unit: L, source: "12.3 p13", note: approximately}
  - {name: total plasma clearance, value: 1.3, unit: L/h, source: "12.3 p13"}
  - {name: renal clearance, value: 0.6, unit: L/h, source: "12.3 p13"}
  - {name: terminal half-life, value: 13, unit: h, source: "12.3 p13", note: "approximately, biphasic"}
```

## Pharmacodynamics and end points
12.2 (p13): inhibition of the angiotensin I pressor response, over 90% at 24 h above 40 mg. No concentration
target or range.

## Dosing (2.1-2.2, PDF p4-5)
Adults 20 mg once daily, 40 mg after 2 weeks if needed; children 6 y and over 10 mg (20 to under 35 kg) or 20 mg
(35 kg and over), maxima 20 and 40 mg; not recommended under 1 year.

## Covariates (12.3 p14; 8.5-8.7 p11)
Elderly AUCss 33% higher (renal clearance about 30% lower); paediatric clearance similar to adults adjusted for
weight; women 10-15% higher AUC and Cmax; moderate hepatic impairment AUC about 60% higher; severe renal
impairment (CrCl under 20 mL/min) AUC about tripled.

## Caveats
- Summary values without study detail; the label does not say whether 17 L and 1.3 L/h are systemic (IV) values.

## Used by stanpumpR
Registry source for `inst/extdata/antihypertensiveRegistry.csv:99` (row
`olmesartan__medoxomil_tablet__oral__catalog__none__label`; generator `data-raw/antihypertensiveRegistry.R:185-189`).

## Verification against stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:99` (8baa733): drug "olmesartan medoxomil", moiety and analyte
  "olmesartan", oral tablet, plasma: **match** (11, 12.3). Limitations "Presystemic hydrolysis" **matches** (12.3,
  p13); "Label CL about 1.3 L/h" **matches** (p13; total plasma clearance, not apparent oral); "terminal half-life
  about 13 h with a biphasic profile" **matches** (p13); "no full fit, no BP coefficients" is consistent with a label
  that prints neither. Status fields (`official_label_descriptive`, `descriptive_only`, BP simulation FALSE,
  `stanpumpr_drug` NA for this catalog row) are consistent. **Matches.**
