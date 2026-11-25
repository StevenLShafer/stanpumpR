---
file: NJ_label-glumetza_021748_2026-09-21.pdf
doi: ""
citation: "GLUMETZA (metformin hydrochloride) extended-release tablets, 500 mg and 1,000 mg, Santarus, Inc. (Salix); NDA 021748; FDA prescribing information revised 8/2026, DailyMed set id fb832474-88d9-4e29-95cd-fbc446944cc4 (archived 2026-09-21)."
kind: label
drugs: [metforminER]
source_read: "text layer only (clean, single column), sections: Boxed Warning, 2 Dosage and Administration, 5.1, 7 Drug Interactions, 8 Use in Specific Populations, 10 Overdosage, 11 Description, 12.1, 12.3 Pharmacokinetics (Tables 3 and 4). No page images rendered; no number below is used by the repo code."
digest_by: Claude (unattended digest_run.py), 2026-11-25
checked_against: stanpumpR master 94262c07
verification: partly-verified
open_questions:
  - "[decision] Label 12.3 gives a plasma elimination half-life of about 6.2 h (blood 17.6 h) and V/F 654 +/- 358 L (850 mg IR, single dose); the metforminER model (R/drugs_metforminER.R:86-92, Duong 2013) gives, at CLcr 100 mL/min and 70 kg, a derived terminal half-life of 12.3 h and V1+V2 = 331 L. Decide whether the label values are an acceptable independent check or a reason to revisit the model; the label does not say what dose, population or sampling window its half-life and V/F come from."
---

# Glumetza label (NDA 021748, 8/2026)

## Study
Not a study: the FDA label for an extended-release metformin HCl tablet (500 and 1,000 mg; 389.93 and 779.86 mg metformin base). Indication: adults with type 2 diabetes. Numbers come from healthy-volunteer PK studies and trials cited in section 12.3; the label gives no n, assay, or sampling detail.

## Model and parameters
No model. Label PK facts (section 12.3):

```yaml
parameters:
  - {name: Tmax, value: "7-8", unit: h, source: "12.3 Absorption", note: "single 1,000 mg (2 x 500 mg) after a meal"}
  - {name: Cmax_vs_IR, value: "up to 35% higher", unit: "", source: "12.3", note: "1,000 mg ER once daily vs IR 500 mg twice daily; AUC equivalent"}
  - {name: steady_state_conc, value: "<1", unit: mcg/mL, source: "12.3", note: "generally, usual doses; steady state reached within 24-48 h"}
  - {name: dose_proportionality, value: "less than proportional", unit: "", source: "12.3", note: "single 500-2,500 mg: AUC and Cmax"}
  - {name: food_AUC_increase, value: "38% low-fat; 73% high-fat", unit: "", source: "12.3", note: "vs fasting; Tmax +~3 h; Cmax unchanged"}
  - {name: V/F, value: "654 +/- 358", unit: L, source: "12.3 Distribution", note: "single 850 mg metformin HCl (immediate release), mean +/- SD"}
  - {name: protein_binding, value: negligible, unit: "", source: "12.3"}
  - {name: renal_clearance_vs_CLcr, value: 3.5, unit: "x", source: "12.3 Excretion", note: "tubular secretion; ~90% of absorbed dose renally eliminated in 24 h"}
  - {name: t_half_plasma, value: 6.2, unit: h, source: "12.3 Excretion", note: "approximate"}
  - {name: t_half_blood, value: 17.6, unit: h, source: "12.3 Excretion", note: "approximate; erythrocytes a distribution compartment"}
  - {name: mild_renal_impairment, value: "oral CL -33%; renal CL -16%; Cmax +27%; AUC +61%", unit: "", source: "12.3 Renal impairment", note: "single GLUMETZA 500 mg vs healthy; the label text lists the pairs as '33% and 50% and 16% and 53%' for mild and moderate"}
  - {name: moderate_renal_impairment, value: "oral CL -50%; renal CL -53%; Cmax +74%; AUC 2.36-fold", unit: "", source: "12.3 Renal impairment"}
  - {name: geriatric, value: "CL -35%; t1/2 +64%; Cmax +76%", unit: "", source: "12.3", note: "limited data, healthy elderly vs young; attributed to renal function"}
  - {name: female_Cmax, value: "+40%", unit: "", source: "12.3 Gender", note: "AUC and t1/2 not different"}
  - {name: Asian_Cmax_AUC, value: "~+10%", unit: "", source: "12.3 Race", note: "trend"}
  - {name: dialysis_clearance, value: "up to 170", unit: mL/min, source: "5.1, 10"}
```
Drug interactions (Table 3, 12.3; single doses of IR 850 mg unless noted): cimetidine 400 mg raises metformin AUC x1.40, Cmax x1.61; topiramate 100 mg q12h steady state AUC x1.25 (500 mg q12h); glyburide, furosemide, nifedipine, propranolol, ibuprofen 0.90-1.16. Table 4 (metformin's effect on the other drug) not needed for stanpumpR.

## Pharmacodynamics and end points
No ke0, EC50 or tPeak. Not an opioid. Concentration thresholds: metformin-associated lactic acidosis occurred with plasma levels generally >5 mcg/mL (Boxed Warning, 5.1) with blood lactate >5 mmol/L; usual-dose steady state generally <1 mcg/mL (12.3). Matrix is plasma; total vs free not stated (protein binding negligible). No therapeutic range is given for glycemic effect.

## Covariates
Renal function (eGFR) is the dominant one: contraindicated below 30 mL/min/1.73 m2; initiation not recommended at 30-45 (2.2). Age (via renal function), sex (Cmax), food (AUC), race: all above.

## Caveats
Population, n and doses behind each number are not stated in the label. Dosing: start 500 mg once daily with evening meal, titrate by 500 mg every 1-2 weeks to a maximum of 2,000 mg once daily (2.1). The V/F and half-life come from IR metformin, not Glumetza.

## Used by stanpumpR
`inst/extdata/fdaLabels.csv:200` registers this PDF (status label, NDA 021748, dailymed) against drug `metforminER`. No R code cites it. R/drugs_metforminER.R (Duong 2013, PMID 23475568) is the model of this product; help text inst/app-help/drugs/metforminER.md lists Glumetza.

## Verification against stanpumpR
Compared with R/drugs_metforminER.R (model; no citation of the label):
- **Dose range**: model's relative F table covers 500-2000 mg (R/drugs_metforminER.R:93-94); label maximum 2,000 mg once daily, tablets 500 and 1,000 mg. **Matches.**
- **Input shape**: zero-order input over 9.55 h (line 92) with relative F falling with dose (1.26, 1.16, 1.06, 1). Label: less-than-proportional AUC and Cmax from 500 to 2,500 mg, Tmax 7-8 h after 1,000 mg with food. Direction **matches**; the model has no food term, and the label says food raises AUC 38-73% (the model's Duong data are not food-labelled here, so not checked).
- **Renal dependence**: model CL/F proportional to Cockcroft-Gault CLcr with no non-renal part (lines 24-50, 148). Label: oral CL falls 33% (mild) and 50% (moderate) with mild/moderate impairment; AUC +61% and 2.36-fold. Directionally **consistent**; no numeric check, since the label does not give the CLcr ranges.
- **Steady-state concentration**: label says generally <1 mcg/mL at usual doses. Model at 70 kg, CLcr 84 mL/min gives CL/F 61.1 L/h (header line 70-72), so 2000 mg/day gives Css,avg about 1.4 mg/L (derived: 2000/(61.1 x 24)). That is above the label's "generally <1" but the label's phrase is not a dose-specific bound. **Cannot verify.**
- **Half-life and V/F**: derived from the model's header parameters (CL 72.7 L/h, V1 149, Q 12.18 L/h, V2 182): terminal t1/2 12.3 h, V1+V2 331 L, against the label's 6.2 h and 654 +/- 358 L. Both **differ** in the sense of a longer terminal half-life and smaller volume in the model. The label's values are IR-based and undefined in design; see the open question.
- **Pregnancy**: the label gives no pregnancy PK, so the model's pregnancy factors (lines 59-67) are not checkable here.
- **Therapeutic range**: inst/extdata/therapeuticRanges.csv:112 gives none established for metforminER; the label offers only the >5 mcg/mL lactic-acidosis level. No conflict.
