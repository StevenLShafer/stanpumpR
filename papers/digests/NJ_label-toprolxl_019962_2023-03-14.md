---
file: NJ_label-toprolxl_019962_2023-03-14.pdf
pmid: none
doi: none
citation: "TOPROL-XL (metoprolol succinate) extended-release tablets, for oral use, NDA 019962 (manufactured by AstraZeneca AB, distributed by Melinta Therapeutics), prescribing information revised 03/2023, Reference ID 5139940, posted on Drugs@FDA 2023-03-14. https://www.accessdata.fda.gov/drugsatfda_docs/label/2023/019962s050s052lbl.pdf"
kind: label
drugs: [metoprolol]
source_read: "text layer, all 16 pages: highlights and contents (p1), 1 Indications (p2), 2 Dosage and Administration (pp2-3), 3-7 (pp3-6), 8 Use in Specific Populations (pp6-9), 10-11 Description (p9), 12.1-12.5 Clinical Pharmacology (pp10-12), 13-17 (pp12-16)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Toprol-XL label (2023): metoprolol succinate extended release

## Study
US prescribing information for Toprol-XL 25, 50, 100 and 200 mg extended-release tablets (multiple-unit pellet
system); "Initial U.S. Approval: 1992" (p1). Not a study.

## Model and parameters
No model. Label facts:
```yaml
parameters:
  - {name: succinate_per_tartrate_strength, value: "23.75 / 47.5 / 95 / 190", unit: mg, source: "11 p9", note: "metoprolol succinate equivalent to 25 / 50 / 100 / 200 mg metoprolol tartrate"}
  - {name: F_rel_ER_vs_IR, value: 0.77, unit: fraction, source: "12.3 p11", note: "steady state, 50-400 mg once daily, relative to conventional tablets"}
  - {name: Cmax_ratio_ER_vs_IR, value: "0.25-0.5", unit: ratio, source: "12.3 p11", note: "peak plasma levels one-fourth to one-half"}
  - {name: first_pass, value: 0.5, unit: fraction, source: "12.3 p11", note: "IR peak about 50% of IV peak"}
  - {name: albumin_binding, value: 12, unit: "%", source: "12.3 p11", note: "about"}
  - {name: CSF_plasma, value: 0.78, unit: ratio, source: "12.3 p11"}
  - {name: t_half, value: "3-7", unit: h, source: "12.3 p11", note: "plasma"}
  - {name: conc_30_80pct_beta1, value: "30-540", unit: nmol/L, source: "12.2 p10", note: "8-23% reduction in exercise heart rate"}
```
Dosing (pp2-3): hypertension 25-100 mg once daily, up to 400 mg; children 6 years or older 1 mg/kg once daily (max
initial 50 mg, not studied above 2 mg/kg or 200 mg); angina 100 mg once daily, up to 400 mg; heart failure 25 mg
(NYHA II) or 12.5 mg once daily, doubled every 2 weeks up to 200 mg; switch from IR at the same total daily dose;
tablets may be halved, not crushed. CL/F rises linearly with weight; age, sex and race not significant; children
6-17 years similar to adults (12.3, p12). Renal failure: no clinically significant change (8.7, 12.3). Hepatic
impairment not studied (8.6). Bioavailability rises with dose, not proportionally; food has no significant effect.

## Pharmacodynamics and end points
Exercise heart rate as the beta-1 marker (12.2); comparable 24-h beta-1 blockade to IR given once to four times
daily at 100-400 mg/day. No time to peak effect. Not an opioid.

## Covariates
Body weight on CL/F (no equation given); CYP2D6 phenotype and inhibitors.

## Caveats
Descriptive label; no population model or absorption constants for the ER product.

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:84 (metoprolol__succinate_er_tablet__oral__catalog__none__label),
  a catalog-only record; tests/testthat/test-antihypertensive-registry.R:179-180 (record ID only).

## Verification against stanpumpR
- Product and route identity (metoprolol succinate, oral, extended-release tablet, plasma metoprolol):
  **matches** the label. The record's URL is a DailyMed setid (342846fb...), not this FDA file; I cannot confirm
  they are the same version. The record's note on a published IR/ER population model concerns another source and
  cannot be checked here.
- Test :179-180 asserts only that the record IDs exist: **not applicable**.
- stanpumpR offers no extended-release metoprolol unit; the label's statement that Toprol-XL strengths are
  expressed as tartrate equivalents is consistent with the repo's tartrate dose basis
  (R/drugs_metoprolol.R:38-39), though the repo does not cite this label for it.
