---
file: NJ_dailymed-fd676896.pdf
pmid: none
doi: none
citation: "ATACAND (candesartan cilexetil) tablets 8 mg, Carilion Materials Management repackaging of AstraZeneca NDA020838 (marketing start 09/14/1998); prescribing information Revised 8/2016, DailyMed SPL setid fd676896-5f5b-48da-becc-3736a6fc1f14, archived 2026-10-21. https://dailymed.nlm.nih.gov/dailymed/lookup.cfm?setid=fd676896-5f5b-48da-becc-3736a6fc1f14"
kind: label
drugs: [candesartan cilexetil]
source_read: "text layer, all 22 pages (one form-feed chunk per page; letter-spaced font artefacts in places). Read: Highlights and contents p.1-3; 2 Dosage and Administration p.4-5; 8 Use in Specific Populations p.9-10; 10 Overdosage and 11 Description p.10-11; 12.1-12.3 Clinical Pharmacology p.11-13; 14 Clinical Studies p.14-15 (hypertension trough/peak passages); SPL product data p.22. Page images: p.1 (identity), p.13 (12.3 renal passage, scrambled in the text layer)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Atacand (candesartan cilexetil) prescribing information, DailyMed, revised 8/2016

## Study
Not a study: the US Atacand label (AstraZeneca, NDA020838, initial approval 1998) as repackaged by Carilion
Materials Management (8 mg tablet, NDC 68151-4146). Indications hypertension (adults, children 1 to < 17 y) and
heart failure NYHA II-IV. The file is what its name says (image p.1). A newer Atacand label may exist.

## Model and parameters
No model. Label facts (12.3, p.12-13):
- Candesartan cilexetil is a prodrug, rapidly and completely hydrolysed to candesartan during absorption.
- Absolute bioavailability of candesartan from the tablet about 15%; Cmax 3-4 h after the tablet; high-fat food
  has no effect. (This label version gives no oral-solution figure.)
- Volume of distribution 0.13 L/kg; protein binding > 99%.
- Total plasma clearance 0.37 mL/min/kg, renal clearance 0.19 mL/min/kg; about 26% of an oral dose excreted
  unchanged in urine. 14C oral: 33% urine, 67% faeces; 14C IV candesartan: 59% urine, 36% faeces.
- Elimination half-life approximately 9 h; linear up to 32 mg; no accumulation once daily.
- Derived check: t1/2 = ln2 x 0.13 / (0.37 x 60/1000) = 4.1 h, shorter than the stated 9 h, so the stated
  half-life is a terminal value and 0.13 L/kg is not the terminal volume (derived; the label does not say).
- Elderly (>= 65 y): Cmax about 50% and AUC about 80% higher. Severe renal impairment (CrCl < 30
  mL/min/1.73 m2): AUC and Cmax about doubled (image p.13); heart-failure patients with mild/moderate renal
  impairment AUC0-72h +36%/+65%, Cmax +15%/+55%. Hepatic: AUC +30% (Child-Pugh A), +145% (B); Cmax +56%, +73%.
  Heart failure: AUC about doubled. Not removed by hemodialysis.
- Children 1-17 y: peak about 4 h, with levels more than 10-fold higher at peak than at 24 h; 0.2 mg/kg in 1 to
  < 6 y gives exposure like 8 mg in adults.
- Molecular weight of candesartan cilexetil 610.67 (11, p.10).
Dosage (2, p.4-5): adult hypertension start 16 mg once daily, 8-32 mg/day in one or two doses; moderate hepatic
impairment start 8 mg; heart failure 4 mg up to 32 mg once daily, doubling about every 2 weeks; children 1 to
< 6 y 0.05-0.4 mg/kg/day (start 0.20 mg/kg), 6 to < 17 y by weight.

```yaml
parameters:
  - {name: F_candesartan_tablet, value: 0.15, unit: fraction, source: "12.3 p.12", note: "absolute, estimated"}
  - {name: tmax_tablet, value: "3-4", unit: h, source: "12.3 p.12"}
  - {name: t_half, value: 9, unit: h, source: "12.3 p.12", note: approximately}
  - {name: V, value: 0.13, unit: L/kg, source: "12.3 p.12"}
  - {name: CL_total, value: 0.37, unit: mL/min/kg, source: "12.3 p.12"}
  - {name: CL_renal, value: 0.19, unit: mL/min/kg, source: "12.3 p.12"}
  - {name: protein_binding, value: ">0.99", unit: fraction, source: "12.3 p.12"}
  - {name: MW_candesartan_cilexetil, value: 610.67, unit: g/mol, source: "11 p.10"}
```

## Pharmacodynamics and end points
12.2 (p.11): after 1 week of 8 mg once daily, the angiotensin II pressor effect was about 90% inhibited at peak
and about 50% at 24 h. 2.1: most of the antihypertensive effect within 2 weeks, maximal at 4-6 weeks. 14:
trough (24 h) BP reductions with 8-32 mg; trough-to-peak ratios reported for once-daily dosing (not tabulated
here). No therapeutic plasma range. Not an opioid.

## Covariates
Age, renal function, hepatic impairment (Child-Pugh), heart failure; no sex difference; children 1-17 y not
modified by age, sex or weight.

## Caveats
Descriptive values without n; the half-life and V/CL in 12.3 are not mutually consistent as a one-compartment
description (derived above). Revised 8/2016 repackager copy.

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:19: catalog record `candesartan__cilexetil_tablet__oral__catalog__none__label`,
  analyte candesartan, plasma, official_label, descriptive only; limitations "Presystemic conversion; label
  tablet active F about 15%, Tmax 3-4 h, half-life about 9 h. No tablet-specific active fit, no BP
  coefficients." Built at data-raw/antihypertensiveRegistry.R:195-199.

## Verification against stanpumpR
- **Presystemic conversion**: complete hydrolysis during absorption (11, 12.3). **matches**.
- **Tablet active F about 15%**: 12.3. **matches**.
- **Tmax 3-4 h**: 12.3, after tablet ingestion. **matches**.
- **Half-life about 9 h**: 12.3. **matches**.
- **No tablet-specific active fit, no BP coefficients**: the label has none. **matches**.
