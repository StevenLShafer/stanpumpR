---
file: NJ_label-catapres_017407_2012-06-06.pdf
pmid: null
doi: null
citation: "Catapres (clonidine hydrochloride, USP) oral antihypertensive tablets 0.1, 0.2 and 0.3 mg prescribing information, NDA 017407 (s037), Boehringer Ingelheim; label text revised October 2011, FDA Reference ID 3138562, posted 2012-06-06."
kind: label
drugs: [clonidine]
source_read: "text layer, all 9 pages: Description (PDF p1), Clinical Pharmacology incl. Pharmacokinetics (p1-2), Warnings/Precautions incl. withdrawal and perioperative use (p3-4), Pediatric Use (p6), Overdosage (p7), Dosage and Administration and Renal Impairment (p7-8), How Supplied (p8), back matter (p9)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Catapres oral tablets, FDA label NDA 017407 (2011 text)

## Study
This is the brand label, not a study. Tablets are 0.1, 0.2 and 0.3 mg clonidine HCl; 0.1 mg equals 0.087 mg of free base (Description, p1). How Supplied (p8): tan BI 6, orange BI 7, peach BI 11; NDC 0597-0006/0007/0011-01. Each page carries the FDA banner saying this may not be the latest approved label.

## Model and parameters
No model. Pharmacokinetics (p2):

```yaml
parameters:
  - {name: F_oral, value: "0.70-0.80", unit: fraction, source: "Pharmacokinetics p2", note: absolute}
  - {name: tmax_oral, value: "1-3", unit: h, source: "Pharmacokinetics p2", note: approximately}
  - {name: dose_proportional_range, value: "100-600", unit: mcg, source: "Pharmacokinetics p2"}
  - {name: t_half_distribution_IV, value: 20, unit: min, source: "Pharmacokinetics p2", note: about}
  - {name: t_half_elimination_IV, value: "12-16", unit: h, source: "Pharmacokinetics p2", note: "up to 41 h, severe renal impairment"}
  - {name: fraction_unchanged_urine_24h, value: "0.40-0.60", unit: "fraction of absorbed dose", source: "Pharmacokinetics p2"}
  - {name: antihypertensive_concentration_range, value: "0.2-2.0", unit: ng/mL, source: "Pharmacokinetics p2", note: "normal excretory function; no added effect above"}
```

- About 50% of the absorbed dose is metabolised in the liver. Food and race do not affect the PK (p2).

## Pharmacodynamics and end points
- Blood pressure falls within 30-60 min, with the maximum fall within 2-4 h (Clinical Pharmacology, p1).
- The concentration range is 0.2-2.0 ng/mL, with a plateau above it (p2).
- Dosing (p7-8): start 0.1 mg twice daily; titrate by 0.1 mg/day weekly; usual 0.2-0.6 mg/day; 2.4 mg/day is the maximum effective dose.
- Perioperative: continue to within 4 h of surgery and resume as soon as possible (p4).
- Withdrawal: taper over 2-4 days (p3).

## Covariates
- Renal: lower initial dose; dialysis removes little (p8).
- Elderly patients may need a lower starting dose (p7).
- Paediatric safety is not established (p6).

## Caveats
This is a 2011 label posted in 2012, and later revisions exist. The PK is given as summary ranges only.

## Used by stanpumpR
`tests/testthat/test-antihypertensive-registry.R:184-185` (context gives line 74): the record `clonidine__ir_tablet__oral__catalog__none__label` must exist, separate from the transdermal record. The record's `source_url` (`antihypertensiveRegistry.csv:38`) is a DailyMed generic SPL, not this PDF. The repo cites this PDF only through the model_id.

## Verification against stanpumpR
- **test:184-185**: this label establishes an oral immediate-release tablet. That supports a record separate from the patch. **Matches.**
- **Registry:38** limitations, checked against this brand text: F 70-80%, distribution half-life about 20 min, elimination 12-16 h, BP nadir 2-4 h. All match p1-2.
