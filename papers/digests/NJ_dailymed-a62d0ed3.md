---
file: NJ_dailymed-a62d0ed3.pdf
pmid: null
doi: null
citation: "Quinapril Tablets USP (quinapril hydrochloride, film coated), 10 mg; ANDA202725, repackaged by Bryant Ranch Prepack (NDC 63629-7934, source NDC 65862-618). DailyMed SPL setid a62d0ed3-8fcc-45fb-9246-b57b08a11f60, label revised 5/2024; retrieved 2026-10-21."
kind: label
drugs: [quinapril]
source_read: "text layer, all 20 PDF pages: boxed warning, Description, Clinical Pharmacology incl. Pharmacokinetics and Metabolism and Pharmacodynamics (p. 1-4), Indications (p. 4-5), Warnings and Precautions incl. Pregnancy, Pediatric and Geriatric Use (p. 5-13), Overdosage and Dosage and Administration incl. renal-impairment table (p. 16-18), How Supplied and SPL product/marketing data (p. 18-20). Old-format label; clean prose; the renal dose table read from the text layer (simple two-column layout)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Quinapril tablets (ANDA202725, Bryant Ranch repackager), DailyMed SPL

Old-format (non-PLR) generic label; boxed warning for fetal toxicity. This SPL covers only the repackaged 10 mg tablet (30-count bottle); the Description lists 5, 10, 20 and 40 mg strengths (5 mg quinapril = 5.416 mg quinapril hydrochloride). Quinapril is the ethyl ester prodrug of quinaprilat (Description, p. 1).

## Study
Not a study.

## Model and parameters
No population model. Pharmacokinetics and Metabolism (PDF p. 2):

```yaml
parameters:
  - {name: tmax_quinapril, value: 1, unit: h, source: "Clinical Pharmacology, PK (p. 2)", note: "peak plasma quinapril within one hour"}
  - {name: fraction_absorbed, value: 0.60, unit: fraction, source: "PK (p. 2)", note: "at least 60%, from urinary recovery"}
  - {name: high_fat_meal_effect, value: "25-30", unit: "% reduction", source: "PK (p. 2)", note: "rate and extent of absorption"}
  - {name: fraction_dose_to_quinaprilat, value: 0.38, unit: fraction, source: "PK (p. 2)", note: "about 38% of the oral dose"}
  - {name: tmax_quinaprilat, value: 2, unit: h, source: "PK (p. 2)", note: "multiple dosing"}
  - {name: t_half_accumulation_quinaprilat, value: 3, unit: h, source: "PK (p. 2)", note: "effective accumulation half-life, multiple dosing"}
  - {name: t_half_quinaprilat, value: 2, unit: h, source: "PK (p. 2)", note: "plasma elimination half-life"}
  - {name: t_half_terminal_quinaprilat, value: 25, unit: h, source: "PK (p. 2)", note: "prolonged terminal phase"}
  - {name: renal_fraction_quinaprilat_IV, value: 0.96, unit: fraction, source: "PK (p. 2)", note: "up to 96% of an IV dose"}
  - {name: protein_binding, value: 97, unit: "%", source: "PK (p. 2)", note: "quinapril and quinaprilat"}
```

- Linear over 5-80 mg single and 40-160 mg multiple daily doses. Quinaprilat clearance correlates linearly with creatinine clearance; elimination reduced in the elderly and in heart failure through renal function; dialysis has little effect; quinaprilat reduced in alcoholic cirrhosis (impaired de-esterification) (p. 2). Elderly have higher quinaprilat AUC and peak, attributed to renal function (Geriatric Use, p. 13).

## Pharmacodynamics and end points
Pharmacodynamics (p. 3): a single 20 mg dose gives > 80% plasma ACE inhibition for 24 h; inhibition of the angiotensin I pressor response 75% at about 4 h, 50% at about 8 h, 20% at 24 h. BP effect starts within 1 h, peaks at 2-4 h; most of a dose's effect in 1-2 weeks; trough effect about 5-11 / 3-7 mm Hg, about 50% of peak. Potassium rises about 0.07 mmol/L on monotherapy (p. 1-2). No therapeutic concentration range.

## Covariates
Creatinine clearance (descriptive); age through renal function; hepatic cirrhosis.

## Caveats
Dosing (p. 16-17): hypertension 10 or 20 mg once daily without diuretic, titrate at >= 2-week intervals, usual 20-80 mg/day once or in two doses; 5 mg if a diuretic cannot be stopped; elderly 10 mg. Renal impairment maximum initial dose: CrCl > 60 mL/min 10 mg, 30-60 5 mg, 10-30 2.5 mg, < 10 insufficient data. Heart failure 5 mg twice daily, usually to 20-40 mg/day; with renal impairment 5 mg (CrCl > 30) or 2.5 mg (10-30).

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:110 (8baa733): record quinapril__tablet__oral__catalog__none__label, active_moiety quinaprilat, analyte and matrix NA, official_label_descriptive, descriptive_only; limitations "Prodrug identity only; parent and active-moiety PK and BP are missing."

## Verification against stanpumpR
- Prodrug identity (quinapril, active moiety quinaprilat): Description and Mechanism (p. 1). **matches**.
- "parent and active-moiety PK and BP are missing": the label does give descriptive PK for both (tmax, extent of absorption, fraction converted, half-lives, renal elimination, binding) and descriptive BP time course, but no clearance, volume or model. Read as "no model parameters", the record **matches**; read literally ("PK missing") it understates what the label carries. Wording only; nothing numeric is taken.
- The SPL is a repackager's 10 mg-only listing of ANDA202725; product identity is correct for the record (oral tablet).
