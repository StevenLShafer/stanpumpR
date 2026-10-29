---
file: NJ_label-invokana_204042_2026-06-09.pdf
pmid: null
doi: null
citation: "Janssen Pharmaceuticals. INVOKANA (canagliflozin) tablets, for oral use: full prescribing information. NDA 204042 (label 204042s044), revised 06/2026, Reference ID 5809303. https://www.accessdata.fda.gov/drugsatfda_docs/label/2026/204042s044lbl.pdf"
kind: label
drugs: [canagliflozin]
source_read: "text layer, 53 pages; read the Highlights and contents (pp. 1-2), 1 and 2 Dosage and Administration (pp. 3-5), 7 Drug Interactions (pp. 17-18), 8 Use in Specific Populations 8.1-8.7 (pp. 18-21), 10 and 11 (pp. 21-22), 12.1-12.3 Clinical Pharmacology including Tables 8-9 (pp. 22-27). Page 23 (12.2, start of 12.3) confirmed on the rendered image. Adverse reactions, clinical studies and the Medication Guide not read."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# INVOKANA (canagliflozin) label, revised 06/2026

Document identity: the Highlights and every page footer give INVOKANA, Initial U.S. Approval 2013, Revised 06/2026,
Reference ID 5809303, consistent with the file name (NDA 204042, 2026-06-09). Recent major changes: 2.6 and 5.4, 06/2026.

## Study
Not a study: the US prescribing information. Indications (section 1, p. 3): glycaemic control in adults and children
>= 10 y with T2DM; MACE reduction in T2DM with CVD; renal/CV risk reduction in diabetic nephropathy with albuminuria
> 300 mg/day. Not for type 1 diabetes, not for glycaemic control at eGFR < 30.

## Model and parameters
Non-compartmental summary values only (12.3, pp. 23-24); no compartmental model.

```yaml
parameters:
  - {name: Tmax, value: "1 to 2", unit: h, source: "12.3, p. 23", note: "median, single 100 and 300 mg doses"}
  - {name: apparent terminal t1/2 at 100 mg, value: 10.6, unit: h, source: "12.3, p. 23", note: "single dose"}
  - {name: apparent terminal t1/2 at 300 mg, value: 13.1, unit: h, source: "12.3, p. 23", note: "single dose"}
  - {name: time to steady state, value: "4 to 5", unit: days, source: "12.3, p. 23", note: "once daily 100-300 mg"}
  - {name: accumulation, value: 36, unit: "%", source: "12.3, p. 23", note: "up to; multiple 100 and 300 mg doses"}
  - {name: absolute oral bioavailability, value: 65, unit: "%", source: "12.3 Absorption, p. 24", note: "mean, approximately"}
  - {name: Vss after IV infusion, value: 83.5, unit: L, source: "12.3 Distribution, p. 24", note: "mean, healthy subjects"}
  - {name: plasma protein binding, value: 99, unit: "%", source: "12.3 Distribution, p. 24", note: "mainly albumin; concentration independent"}
  - {name: systemic clearance after IV, value: 192, unit: mL/min, source: "12.3 Excretion, p. 24", note: "mean, approximately, healthy subjects"}
  - {name: renal clearance, value: "1.30 to 1.55", unit: mL/min, source: "12.3 Excretion, p. 24", note: "100 and 300 mg"}
```

Dose proportionality of Cmax and AUC from 50 to 300 mg; no time-dependent PK; high-fat meal no effect on PK (p. 24).
Elimination: O-glucuronidation by UGT1A9 and UGT2B4 to two inactive metabolites; CYP3A4 ~7%. [14C] dose: faeces 41.5%
canagliflozin, 7.0% hydroxylated metabolite, 3.2% O-glucuronide; urine ~33%, mainly O-glucuronides (30.5%), < 1%
unchanged (p. 24).

Derived for comparison: 192 mL/min = 11.5 L/h systemic; / 0.65 = 17.7 L/h apparent oral clearance in healthy
subjects (derived, not a label number).

## Pharmacodynamics and end points
12.2 (p. 23): dose-dependent fall in the renal threshold for glucose (RTG) from about 240 mg/dL, to about 70-90 mg/dL at
300 mg daily (phase 1, T2DM); UGE rise about 100 g/day at 100 or 300 mg; UGE returns towards baseline by about 3 days
after single doses up to 300 mg. No QTc effect at 300 or 1200 mg (60 healthy adults). No concentration-effect model or
target concentration is given.

## Covariates
- Dosing (2.2-2.4, pp. 3-4): 100 mg once daily before the first meal, up to 300 mg if eGFR >= 60. eGFR 30 to < 60:
  maximum 100 mg; eGFR < 30: initiation not recommended (may continue 100 mg for the renal indication). UGT inducers
  (rifampin, phenytoin, phenobarbital, ritonavir): increase to 200 mg (Table 8: rifampin AUC ratio 0.49, Cmax 0.72).
- Renal impairment (12.3, p. 25): single 200 mg; AUC +15%, +29%, +53% in mild (N = 10), moderate (N = 9), severe
  (N = 10) vs healthy (N = 3); similar in ESKD (N = 8); Cmax unchanged; negligible removal by haemodialysis.
- Hepatic (p. 25): Child-Pugh A Cmax/AUC 107%/110%, B 96%/111%; C not studied.
- Population PK (p. 25, 1,526 adults): age, BMI/weight, sex and race not clinically meaningful.
- Paediatric (p. 24-25): 10 y and older, exposure consistent with adults at 100 and 300 mg.

## Caveats
- The half-lives are single-dose apparent values; the label does not name the method (non-compartmental is the usual
  basis but is not stated) or the sampling duration.
- The renal impairment AUC increases here (+15/29/53%, a dedicated single-dose study) differ from the population
  estimates in Hoeben 2016 (+11/40/29%); different designs, both reported.

## Used by stanpumpR
From the context file: R/drugs_canagliflozin.R:87 and inst/help/drugs/canagliflozin.md:22, the non-compartmental
terminal half-life (10.6 and 13.1 h at 100 and 300 mg) compared with the model's 16 h.

## Verification against stanpumpR
- **R/drugs_canagliflozin.R:87-89** (at 8baa733): "the label's 10.6 and 13.1 h at 100 and 300 mg are
  non-compartmental, from the first days after a dose". The numbers and dose pairing **match** 12.3 (p. 23, rendered).
  The label calls them apparent terminal half-lives after single doses; "non-compartmental" and "from the first days"
  are the repo's characterisation, not label wording.
- **inst/help/drugs/canagliflozin.md:22**: "the label's 10.6 to 13.1 hours, which come from the first days after a
  dose", against the model's 16 h. **Matches** the numbers; same characterisation note.
- Not cited by the repo but consistent with it: F 65% (the model's parameters are apparent, bioavailability carried as
  1), the 100/300 mg units, MDRD-based renal categories.
- The repo does not name the label edition (context note); this file is the 06/2026 edition.
