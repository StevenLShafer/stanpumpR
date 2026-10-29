---
file: NJ_dailymed-aff713e8.pdf
pmid: null
doi: null
citation: "Physicians Total Care, Inc. (repackager). DEMADEX (torsemide) tablets 10 and 20 mg, NDA 020136 (source NDC 0037-5010, 0037-5020, Meda). DailyMed SPL setid aff713e8-2cb6-4e49-94a2-c55c5491c822; revised 8/2009; archived 2026-10-21."
kind: label
drugs: [torsemide]
source_read: "text layer, all 12 PDF pages (old non-PLR format): Description, Clinical Pharmacology incl. Pharmacokinetics and Metabolism p1-2; Clinical Effects p2-3; Indications, Contraindications, Warnings p3-4; Precautions incl. Drug Interactions, Pregnancy, Nursing, Pediatric and Geriatric Use p4-7; Adverse Reactions p7-9; Overdosage, Dosage and Administration, How Supplied p9; SPL product and marketing data p10-12 (Revised 8/2009 on p12). PDF p1 checked on a rendered image."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# DEMADEX (torsemide) tablets, repackager label (Physicians Total Care, 8/2009)

Torsemide is registry-only in stanpumpR (no drug model).

## Study
Not a study: a repackager's SPL of the Meda DEMADEX tablet label (10 mg and 20 mg bottles, NDA 020136; marketing start
dates 11/28/2000 and 10/21/2009; PDF p9-12). Oral tablets only; the injection text of the Roche label is absent. The
clinical pharmacology text is the same as the Roche 2003/2007 label (see `NJ_dailymed-cf288ad5`), without the IV
statements.

## Model and parameters
No model. Descriptive PK (Pharmacokinetics and Metabolism, PDF p1-2):

```yaml
parameters:
  - {name: F_oral, value: 0.80, unit: fraction, source: "PDF p1", note: "approximately; 90% CI 0.75-0.89"}
  - {name: Tmax_oral, value: "<= 1", unit: h, source: "PDF p1", note: "food delays about 30 min; AUC and diuresis unchanged"}
  - {name: dose_proportional_range, value: "2.5-200", unit: mg, source: "PDF p1"}
  - {name: Vd, value: "12-15", unit: L, source: "PDF p1", note: "normal adults, mild-moderate renal failure, CHF; about doubled in cirrhosis"}
  - {name: t_half, value: 3.5, unit: h, source: "PDF p2", note: "approximately, normal subjects"}
  - {name: fraction_hepatic_CL, value: 0.80, unit: fraction, source: "PDF p2", note: "renal about 20% with normal renal function"}
  - {name: protein_binding, value: ">0.99", unit: fraction, source: "PDF p2"}
```
(derived: CL ~ ln2 x 13.5 / 3.5 = 2.7 L/h.) Decompensated CHF: total clearance about 50% of healthy; renal failure:
total clearance and half-life normal, less drug reaches the luminal site; cirrhosis: Vd, half-life and renal clearance
increased, total clearance unchanged; elderly: lower renal clearance only (PDF p2).

## Pharmacodynamics and end points
Oral onset of diuresis within 1 h, peak in hour 1-2, duration 6-8 h; sodium excretion linear over 2.5-20 mg single doses
(Clinical Effects, PDF p2). Diuretic activity tracks urinary excretion rate rather than blood concentration (PDF p1).
Hypertension effect near-maximal at 4-6 weeks (PDF p3). No concentration-effect model, no therapeutic range; not an
opioid. Dosing as in the Roche label, oral only (PDF p9).

## Covariates
Qualitative only (CHF, renal, hepatic, age). Pregnancy category B (PDF p6); pediatric use not established (PDF p7).

## Caveats
- A repackager's label, not the NDA holder's, and 2009 text; the current FDA label (2017, `NJ_label-demadex_020136_2017-02-06`)
  adds CYP2C9 metabolism and urinary recovery of metabolites.
- No study sizes behind the PK statements.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:124` (from `data-raw/antihypertensiveRegistry.R:336-340`): record
  `torsemide__tablet__oral__catalog__none__label`, oral tablet, descriptive only; limitations "Label F about 80%, Tmax
  within 1 h, Vd 12-15 L, half-life about 3.5 h ... BP is missing."

## Verification against stanpumpR
- F about 80%, Tmax within 1 h, Vd 12-15 L, half-life about 3.5 h: all **match** (PDF p1-2).
- Matrix "plasma and urine" and "BP is missing": consistent; the label describes serum PK and urinary action but gives
  no BP model.
- The limitations' remark about the PBPK/PD record's negative exponent refers to Jeong 2022 (`PMID_36559213`), not to
  this label.
- Setid matches the registry URL.
