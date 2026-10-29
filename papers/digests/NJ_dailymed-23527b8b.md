---
file: NJ_dailymed-23527b8b.pdf
pmid: null
doi: null
citation: "FRAGMIN (dalteparin sodium) injection, for subcutaneous use. US prescribing information, Pfizer Laboratories Div Pfizer Inc. DailyMed setid 23527b8b-9b28-4e6d-9751-33b143975ac7; Highlights revised 10/2024, SPL revised 9/2025; downloaded 2026-10-21."
kind: label
drugs: []
source_read: "text layer, 72 PDF pages (form-feed chunks = pages). Read Highlights (pp1-3), 2 Dosage and Administration (pp6-11), 5.1 neuraxial timing (p11), 8 Use in Specific Populations (pp19-22), 10 Overdosage, 11 Description (p22), 12.1-12.3 Clinical Pharmacology (pp23-24), 14 pediatric and neonatal studies (pp31-32). Rendered pages 1, 23, 24, 31 and 32; Table 12 (p24) and Table 20 (p32) confirmed from the images."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# FRAGMIN (dalteparin) US label, 2024/2025

## Study
This is the current US prescribing information for dalteparin sodium, a low-molecular-weight heparin given SC
(Pfizer; initial US approval 1994). The Highlights were revised 10/2024, adding pediatric VTE treatment. PDF
page 1 is the Fragmin label as named, so there is no source file problem.

**Dosage (section 2, pp6-10):**
- Unstable angina or non-Q-wave MI: 120 units/kg SC q12h, at most 10,000 units, with aspirin.
- DVT prophylaxis: 2,500-5,000 units SC once daily, with pre- and postoperative schedules for hip replacement and
  abdominal surgery in Table 2. Medical patients: 5,000 units once daily.
- Cancer VTE: 200 units/kg once daily in month 1, then about 150 units/kg in months 2-6, at most 18,000 units/day.
- Pediatric starting doses: 150 units/kg BID from birth (GA ≥35 wk) to <2 y, 125 units/kg BID from 2 to <8 y, and
  100 units/kg BID from 8 to <17 y. Titrate in 25 units/kg steps to an anti-Xa of 0.5-1 units/mL, drawn 4 h
  post-dose before the 4th dose.
- Severe renal impairment (CrCl <30 mL/min) in cancer VTE: monitor anti-Xa, target 0.5-1.5 units/mL, sampled
  4-6 h after a dose and after 3-4 doses.

**Strength basis (section 11, p22):** units are anti-Xa international units (WHO First International LMWH
Reference Standard). 2,500 units = 16 mg dalteparin sodium. Mean MW is 5,000.

## Model and parameters
Section 12.3 (PDF pp23-24) gives the adults' parameters from noncompartmental results in healthy volunteers,
measured as anti-Xa activity. It gives no population model.
```yaml
parameters:
  - {name: Cmax after SC 2500 units, value: 0.19, unit: units/mL, source: "12.3, p23", note: "mean ± 0.04; anti-Xa"}
  - {name: Cmax after SC 5000 units, value: 0.41, unit: units/mL, source: "12.3, p23", note: "mean ± 0.07"}
  - {name: Cmax after SC 10000 units, value: 0.82, unit: units/mL, source: "12.3, p23", note: "mean ± 0.10"}
  - {name: Tmax SC, value: 4, unit: h, source: "12.3, p23", note: "about 4 h in most subjects"}
  - {name: F (SC, absolute), value: 0.87, unit: fraction, source: "12.3, p23", note: "87 ± 6%, healthy volunteers, anti-Xa"}
  - {name: V, value: "40-60", unit: mL/kg, source: "12.3, p23", note: "range as printed"}
  - {name: CL (IV 30 units/kg), value: 24.6, unit: mL/h/kg, source: "12.3, p23", note: "mean ± 5.4; disposition t1/2 1.47 ± 0.3 h"}
  - {name: CL (IV 120 units/kg), value: 15.6, unit: mL/h/kg, source: "12.3, p23", note: "mean ± 2.4; disposition t1/2 2.5 ± 0.3 h"}
  - {name: terminal t1/2 (IV 40 units/kg), value: 2.1, unit: h, source: "12.3, p24", note: "± 0.3"}
  - {name: terminal t1/2 (IV 60 units/kg), value: 2.3, unit: h, source: "12.3, p24", note: "± 0.4"}
  - {name: apparent terminal t1/2 (SC), value: "3-5", unit: h, source: "12.3, p24", note: "attributed to delayed absorption"}
  - {name: terminal t1/2 (IV 5000 units, haemodialysis CRF), value: 5.7, unit: h, source: "12.3, p24", note: "± 2.0"}
  - {name: "CL/F, 3 wk to <8 wk", value: 55.8, unit: mL/h/kg, source: "Table 12, p24", note: "derived mean (SD 3.91); Vd/F 181 (15.3) mL/kg; t1/2 2.25 (0.173) h"}
  - {name: "CL/F, 8 wk to <2 y", value: 40.4, unit: mL/h/kg, source: "Table 12, p24", note: "SD 8.49; Vd/F 175 (55.3); t1/2 3.02 (0.688)"}
  - {name: "CL/F, 2 to <8 y", value: 26.7, unit: mL/h/kg, source: "Table 12, p24", note: "SD 4.75; Vd/F 160 (25.6); t1/2 4.27 (1.05)"}
  - {name: "CL/F, 8 to <12 y", value: 22.4, unit: mL/h/kg, source: "Table 12, p24", note: "SD 3.40; Vd/F 165 (27.3); t1/2 5.11 (0.509)"}
  - {name: "CL/F, 12 to <20 y", value: 18.8, unit: mL/h/kg, source: "Table 12, p24", note: "SD 3.01; Vd/F 171 (38.9); t1/2 6.28 (0.937)"}
```
**Nonlinearity:**
- From 2,500 to 10,000 units SC, anti-Xa AUC rose more than dose-proportionally, by about one-third. Peak rose
  roughly linearly.
- IV clearance falls with dose, from 24.6 to 15.6 mL/h/kg between 30 and 120 units/kg.
- There was no appreciable accumulation at 100 units/kg SC BID for 7 days.

**Pediatric model (Table 12, p24):** a one-compartment model with linear absorption and elimination, fitted in 89
patients from two trials and one observational study. The label gives derived means by age band and no ka.
Weight-normalised CL/F falls with age; Vd/F stays near 160-181 mL/kg.

**Arithmetic checks (derived):**
- Adults: 0.693 × 50 mL/kg / 24.6 = 1.4 h, against the printed 1.47 h. 0.693 × 50 / 15.6 = 2.2 h, against
  2.5 h, which would need V of about 56 mL/kg, inside 40-60.
- Pediatrics: 0.693 × 181 / 55.8 = 2.25 h and 0.693 × 171 / 18.8 = 6.3 h, both matching Table 12.

## Pharmacodynamics and end points
- **Activity:** measured and dosed as anti-Xa activity, in units/mL. Dalteparin inhibits factor Xa preferentially
  and barely affects APTT (12.1). Single doses up to 10,000 units SC did not significantly change PT, TT or APTT
  in healthy subjects (12.2).
- **Targets:** therapeutic anti-Xa is 0.5-1 units/mL in children (2.4; Table 20) and 0.5-1.5 units/mL in severe
  renal impairment (2.6).
- **Neuraxial timing (5.1, p11):**
  - Delay catheter placement or removal at least 12 h after 2,500 units once daily, 15 h after 5,000 units, and
    24 h after 200 units/kg daily or 120 units/kg BID.
  - Double these times when CrCl <30 mL/min.
  - After catheter removal, consider waiting at least 4 h before the next dose.
- **Pediatric trial (14, p32):**
  - Therapeutic anti-Xa was reached in a mean of 2.6 days.
  - Table 20 gives the median doses at target: 208 units/kg (n=2, 8 wk to <2 y), 128 (n=8, 2 to <8 y), 125 (n=7,
    8 to <12 y) and 117 (n=17, 12 to ≤18 y).
  - In neonates (Table 21), the median starting dose was 154.5 units/kg BID (range 100-475) and the median
    maintenance dose 345 units/kg BID (range 96-723).
- **Protamine (10):** 1 mg per 100 anti-Xa units, which neutralises at most about 60-75% of the anti-Xa activity.

## Covariates
- Age in children: CL/F per kg falls with age (Table 12).
- Renal failure: t1/2 is 5.7 h on haemodialysis.
- Weight: all dosing is per kg or by weight band.
The label gives no covariate equations.

## Caveats
- The adult values are noncompartmental means from healthy volunteers, from undated studies. Clearance is
  dose-dependent, and the SC terminal phase is absorption-limited.
- The label gives no adult ka, no peripheral compartment and no population variability.
- Source typo: the pediatric efficacy text (p32) gives the therapeutic anti-Xa level as "0.5 mg per day to 1
  units/mL". Table 20's title on the same page gives 0.5 to 1 units/mL.

## Used by stanpumpR
- `inst/extdata/anticoagulantRegistry.csv:20` (line confirmed at 8baa733): `dalteparin_label`, low molecular
  weight heparin, SC, analyte "anti-Xa activity (IU/mL)", assay "LMWH anti-Xa", readiness `pending`, no
  stanpumpR drug. Blocking issue: "No general adult population ka/CL/V/Q vector; label PK is a validation envelope
  only."
- The row is generated by `data-raw/anticoagulantRegistry.R:136-140`.
- `tests/testthat/test-anticoagulant-registry.R:58` requires `dalteparin_label` to be catalogued.

## Verification against stanpumpR
- Analyte and route: the label measures and doses dalteparin as anti-Xa international units, given SC. The
  registry's "anti-Xa activity (IU/mL)" and SC are correct. **matches**
- Blocking issue: the label gives no adult population model and no ka. It gives only adult noncompartmental V
  (40-60 mL/kg), dose-dependent IV CL and SC F of 87%. Its only compartmental model is the pediatric
  one-compartment fit, reported as derived CL/F, Vd/F and t1/2 by age band, again without ka. "Validation
  envelope only" is a fair description: the Cmax at 2,500, 5,000 and 10,000 units, Tmax about 4 h and t1/2
  3-5 h SC could check a model, but they cannot define one. **matches**
- Readiness `pending` with no stanpumpR drug: no dalteparin drug file exists at 8baa733. That is consistent.
  **matches**
