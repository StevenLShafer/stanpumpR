---
file: NJ_dailymed-39631f1f.pdf
pmid: none
doi: none
citation: "VASOTEC (enalapril maleate) tablets, Bausch Health US LLC; NDA018998; prescribing information (non-PLR format), Revised 1/2026, DailyMed SPL setid 39631f1f-5d19-43c1-b504-bf56d991ed97, archived 2026-10-21. https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=39631f1f-5d19-43c1-b504-bf56d991ed97"
kind: label
drugs: [enalapril, enalaprilat]
source_read: "text layer, all 26 pages (one form-feed chunk per page). Old-format label with no numbered sections. Read: boxed warning and Description p.1; Clinical Pharmacology (mechanism, Pharmacokinetics and Metabolism, pharmacodynamics, heart-failure trials, Clinical Pharmacology in Pediatric Patients) p.1-5; Indications p.5; Warnings incl. Fetal Toxicity p.6-8; Precautions incl. Pregnancy, Nursing Mothers, Pediatric Use p.8-13; Overdosage p.16-17; Dosage and Administration p.17-19; How Supplied and SPL data p.19-26 skimmed. Page images: p.17 (renal dosing table)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Vasotec (enalapril maleate) prescribing information, DailyMed SPL, revised 1/2026

## Study
A label. Enalapril maleate (MW 492.53; tablets 2.5, 5, 10, 20 mg), the ethyl-ester prodrug of enalaprilat
(Description, p.1). Boxed warning: fetal toxicity. Indications: hypertension, heart failure, asymptomatic LV
dysfunction.

Dosing (p.17-19, renal table p.17 rendered): hypertension 5 mg once daily initially (2.5 mg if a diuretic cannot be
stopped), usual 10-40 mg/day in one or two doses; creatinine clearance <= 30 mL/min (serum creatinine >= 3 mg/dL)
2.5 mg once daily, dialysis 2.5 mg on dialysis days, titrated to at most 40 mg/day. Heart failure 2.5 mg, range
2.5-20 mg twice daily (max 40 mg/day); start at 2.5 mg daily with Na < 130 mEq/L or creatinine > 1.6 mg/dL.
Paediatric 0.08 mg/kg (up to 5 mg) once daily; above 0.58 mg/kg (or 40 mg) not studied; not recommended in
neonates or with GFR < 30 mL/min/1.73 m2.

## Model and parameters
No model. Pharmacokinetics and Metabolism (p.2):

```yaml
parameters:
  - {name: enalapril tmax, value: 1, unit: h, source: "Pharmacokinetics and Metabolism, p.2", note: "peak serum concentration within about one hour"}
  - {name: extent of absorption of enalapril, value: 0.60, unit: fraction, source: "p.2", note: "approximately 60%, based on urinary recovery; not affected by food"}
  - {name: enalaprilat tmax, value: "3-4", unit: h, source: "p.2", note: "after an oral dose of enalapril maleate"}
  - {name: enalaprilat effective accumulation half-life, value: 11, unit: h, source: "p.2", note: "multiple oral doses of enalapril; longer at GFR <= 30 mL/min"}
  - {name: enalaprilat in urine, value: 40, unit: "% of dose", source: "p.2", note: "about 40%; total urine + faeces recovery about 94% as enalaprilat or enalapril"}
  - {name: enalaprilat dialysis clearance, value: 62, unit: mL/min, source: "p.2"}
  - {name: paediatric effective accumulation half-life, value: 14, unit: h, source: "Clinical Pharmacology in Pediatric Patients, p.5", note: "n = 40, 2 months to 16 y, 0.07-0.14 mg/kg daily; conversion 63-76%; 24 h urinary recovery 68%"}
```

The enalaprilat profile has a prolonged terminal phase attributed to a small, saturable fraction bound to ACE
(p.2). No other metabolites. Disposition unchanged until GFR <= 30 mL/min, then higher peak and trough and
later peak.

## Pharmacodynamics and end points
Single oral dose: antihypertensive onset at about 1 h, peak blood-pressure reduction at 4 to 6 h; effect lasts at
least 24 h at recommended doses, sometimes waning at the end of the interval (p.3). Mean serum K+ rise about 0.2
mEq/L on monotherapy (p.2). No concentration-effect relation or therapeutic range. SOLVD and CONSENSUS outcomes
(p.4) are clinical, not PK.

## Covariates
Renal function (above); paediatric PK consistent with adult historic data (p.5). No hepatic or age data given in
the PK section.

## Caveats
Old-format label; no Cmax, AUC, volume or clearance values; the 11 h figure is an accumulation half-life from
repeated oral dosing, not an elimination half-life.

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:43: `enalapril__ir_tablet__oral__catalog__none__label`,
  `official_label_descriptive`, source_url this setid (also data-raw/antihypertensiveRegistry.R:159).

## Verification against stanpumpR
- Registry line 43: "Prodrug with an active metabolite; label Tmax about 1 h, accumulation half-life about 11 h,
  BP peak 4-6 h." Tmax about 1 h is the parent's (p.2); the metabolite's is 3-4 h, which the record does not
  separate, but the statement is as the label gives it. Accumulation half-life 11 h (p.2) and BP peak 4-6 h (p.3).
  **matches**. Its Ajayi 1985 remark is not from this label (**cannot verify** here).
- Not a listed location, noted for consistency: the label's "approximately 60 percent" absorption (p.2) is the
  "literature's 60%" that docs/antihypertensives.md:1418 compares with Faisal's F1 0.606.
