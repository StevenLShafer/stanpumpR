---
file: NJ_label-lopressorinjection_018704_2020-08-25.pdf
pmid: none
doi: none
citation: "LOPRESSOR (metoprolol tartrate) injection, for intravenous use, NDA 018704 (Novartis Pharmaceuticals Corporation), prescribing information revised 08/2020, Reference ID 4660893, signed 08/24/2020, posted on Drugs@FDA 2020-08-25. https://www.accessdata.fda.gov/drugsatfda_docs/label/2020/018704s027,028lbl.pdf"
kind: label
drugs: [metoprolol]
source_read: "text layer, all 11 pages: highlights and contents (p1), sections 1-5 including 2 Dosage and Administration (p2), 6-7 (pp3-5), 8 Use in Specific Populations (pp5-7), 10-11 (p7), 12.1-12.5 Clinical Pharmacology (pp8-9), 13-17 (pp9-10), FDA signature page (p11)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Lopressor injection label (2020): metoprolol tartrate 5 mg/5 mL

## Study
US prescribing information for Lopressor injection, 5 mg metoprolol tartrate in 5 mL ampoules (1 mg/mL, with
45 mg NaCl), Novartis; "Initial U.S. Approval: 1978" (p1). Not a study.

## Model and parameters
No model. Label facts:
```yaml
parameters:
  - {name: iv_regimen, value: "3 x 5", unit: mg, source: "section 2 p2", note: "three bolus injections about 2 min apart in acute MI, then oral"}
  - {name: time_to_max_beta_blockade, value: 20, unit: min, source: "12.2 p8", note: "10-min infusion, normal volunteers"}
  - {name: oral_iv_equipotent_ratio, value: 2.5, unit: ratio, source: "12.2 p8"}
  - {name: albumin_binding, value: 12, unit: "%", source: "12.3 p8"}
  - {name: CSF_plasma, value: 0.78, unit: ratio, source: "12.3 p8"}
  - {name: t_half, value: "3-7", unit: h, source: "12.3 p8", note: "plasma"}
  - {name: urine_unchanged_iv, value: 10, unit: "%", source: "12.3 p8", note: "approximately; oral under 5%"}
  - {name: CL_dialytic, value: 87, unit: mL/min, source: "12.3 p9", note: "high-flux haemodialysis, after oral dosing"}
  - {name: CL_iv_renal_failure, value: 1, unit: L/min, source: "12.3 p9", note: "mean total systemic clearance, chronic renal failure"}
```
Contraindications include heart rate < 45/min, PR >= 0.24 s, systolic BP < 100 mmHg (p2). CYP2D6 absent in about
8% of Caucasians; strong inhibitors double concentrations; quinidine tripled S-metoprolol (7.3, 12.3, 12.5).
Hepatic impairment not studied (8.6). Metoprolol tartrate Mw 684.82 (11).

## Pharmacodynamics and end points
Maximum beta-blockade about 20 min after a 10-min infusion; log plasma level linear with reduction in exercise heart
rate (12.2, p8). Not an opioid; no therapeutic range.

## Covariates
CYP2D6 phenotype and inhibitors; no renal dose change stated.

## Caveats
No population-PK parameters. The 3-7 h half-life range here differs from the tablet label's 3-4 h (7-9 h in poor
metabolisers).

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:86 (metoprolol__tartrate_injection__iv__catalog__none__label) and
  tests/testthat/test-antihypertensive-registry.R:181-182 (record ID only).
- Implicitly the dose basis "metoprolol tartrate as labelled" for intravenous mg (R/drugs_metoprolol.R:38-39;
  inst/help/drugs/metoprolol.md:11).

## Verification against stanpumpR
- Registry :86 "Label half-life 3-7 h and CYP2D6-dependent. Log concentration tracks EXERCISE heart-rate
  reduction": **matches** 12.2-12.3 (p8). The record's URL is a DailyMed setid (8dae76a2...), not this FDA file; I
  cannot confirm they are the same version.
- Intravenous doses entered as the tartrate (drugs_metoprolol.R:38-39; help :11): **matches** this product, labelled
  as metoprolol tartrate 5 mg/5 mL.
- The "20 minutes after a 10-minute infusion" statement the repo attributes to the Lopressor label
  (drugs_metoprolol.R:58-60) also appears here (p8): **matches**.
- Test :181-182 checks the record ID only: **not applicable**.
