---
file: NJ_dailymed-5ddba454.pdf
pmid: none
doi: none
citation: "DIOVAN (valsartan) tablets, Novartis Pharmaceuticals Corporation; NDA021283; prescribing information (Highlights Revised 7/2025; SPL Revised 8/2026), DailyMed SPL setid 5ddba454-f3e6-43c2-a7a6-58365d297213, archived 2026-10-21. https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=5ddba454-f3e6-43c2-a7a6-58365d297213"
kind: label
drugs: [valsartan]
source_read: "text layer, all 38 pages (one form-feed chunk per page). Read: Highlights and contents p.1-3; 2 Dosage and Administration p.4-6; 8 Use in Specific Populations p.11-13; 10 Overdosage p.13-14; 11 Description p.14; 12.1-12.3 Clinical Pharmacology p.14-17; SPL product data p.31-38 skimmed. Page images: p.15 (12.2-12.3)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Diovan (valsartan) prescribing information, DailyMed SPL, revised 8/2026

## Study
A label. Same product and NDA as NJ_label-diovan_021283_2026-05-12; sections 2, 8 and 12.3 read the same in both.
Tablets 40, 80, 160, 320 mg (molecular weight 435.5, section 11 p.14). Boxed warning for fetal toxicity (p.1).

Dosing (section 2, p.4-6): adult hypertension 80 or 160 mg once daily (80-320 mg; effect largely present by 2
weeks, maximal by 4); paediatric 1-16 y 1 mg/kg once daily up to 40 mg, maximum 4 mg/kg or 160 mg daily, not
recommended under 1 y or with GFR < 30 mL/min/1.73 m2; heart failure 40 mg twice daily to 160 mg twice daily; post-MI
20 mg twice daily to a target of 160 mg twice daily. Suspension AUC 60% higher than tablets (2.1).

## Model and parameters
No model. Section 12.3 (p.15-17, p.15 rendered):

```yaml
parameters:
  - {name: tmax, value: "2-4", unit: h, source: "12.3 Absorption, p.15", note: "healthy volunteers"}
  - {name: elimination half-life, value: 6, unit: h, source: "12.3, p.15", note: "about 6 h, average, IV, bi-exponential"}
  - {name: absolute bioavailability, value: 0.25, unit: fraction, source: "12.3, p.15", note: "range 0.10-0.35"}
  - {name: food effect on AUC, value: -40, unit: "%", source: "12.3 Effect of Food, p.15-16", note: "tablet; Cmax about -50%"}
  - {name: Vss, value: 17, unit: L, source: "12.3 Distribution, p.16", note: "IV"}
  - {name: protein binding, value: 95, unit: "%", source: "12.3, p.16"}
  - {name: CL (IV), value: 2, unit: L/h, source: "12.3 Excretion, p.16", note: "renal 0.62 L/h, about 30%"}
  - {name: CL/F in heart failure, value: 4.5, unit: L/h, source: "12.3, p.16"}
  - {name: accumulation factor in heart failure, value: 1.7, unit: ratio, source: "12.3, p.15", note: "160 mg twice daily"}
  - {name: geriatric AUC increase, value: 70, unit: "%", source: "12.3 p.16; 8.5 p.13", note: "half-life 35% longer"}
```

Dose-proportional over 80-320 mg; recovery about 83% faeces and 13% urine; primary metabolite valeryl
4-hydroxy valsartan (about 9% of dose, CYP2C9).

## Pharmacodynamics and end points
12.2 (p.15): 80 mg orally blocks about 80% of the angiotensin II pressor response at peak, about 30% at 24 h.
No concentration-response or therapeutic range.

## Covariates
No relation of AUC to creatinine clearance down to 10 mL/min (12.3 p.16; 8.6 p.13); not dialysed; mild-moderate
liver disease about 2-fold AUC; no sex effect; paediatric weight-normalised clearance similar to adults.

## Caveats
Descriptive label values; intravenous half-life and clearance, oral tmax and bioavailability.

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:133: `valsartan__tablet__oral__catalog__none__label`,
  `official_label_descriptive`, `descriptive_only`, source_url this setid.

## Verification against stanpumpR
- Registry line 133 limitations: "Label Tmax 2-4 h, F about 25%, IV terminal half-life about 6 h, food lowers
  tablet AUC by about 40%. No matched tablet fit and no BP coefficients." All four numbers are in 12.3 (p.15-16)
  as stated, and the label has no BP model. **matches**.
