---
file: NJ_label-zepbound_217806_2026-09-02.pdf
pmid:
doi:
citation: "ZEPBOUND (tirzepatide) injection, for subcutaneous use (single-dose pen or vial, multi-dose vial, KwikPen), Eli Lilly and Company; US prescribing information, NDA 217806, revised 8/2026 (DailyMed file dated 2026-09-02)."
kind: label
drugs: [tirzepatide]
source_read: "text layer for the highlights, Dosage and Administration (2.1-2.4), Dosage Forms (3), Use in Specific Populations (8.2-8.7), Overdosage, Description and Clinical Pharmacology 12.1-12.3; page images read for 12.3 (PDF pages 17-18). Sections 5, 6, 7, 13 (partly), 14 and 16 skimmed only by heading, not read for PK."
digest_by: Claude (unattended digest_run.py), 2026-11-25
checked_against: stanpumpR master 4414bd01
verification: partly-verified
open_questions:
  - "[decision] inst/extdata/antidiabeticRegistry.csv (row tirzepatide_sc_zepbound) keeps Zepbound as a separate alias awaiting formulation and population evidence, and inst/extdata/fdaLabels.csv lists only the Mounjaro label (NJ_label-mounjaro_215866). This label (PDF p17, 12.3) reports the same PK as the Mounjaro model in numbers (80% bioavailability, CL ~0.06 L/h, Vss ~9.7 L, t1/2 5-6 d). Whether to let the Zepbound alias share R/drugs_tirzepatide.R is Shafer's call; the label alone does not distinguish the two formulations."
  - "[source] The label's 80% is the mean ABSOLUTE bioavailability (PDF p17); R/drugs_tirzepatide.R:56 uses 0.8*(1-0.181) = 0.655, justified by Schneck 2024 and its supplement, not by the label. The label gives no between-study difference, so the label cannot confirm the 0.655."
---

# Zepbound label 2026-09: tirzepatide pharmacokinetics

## Study
US prescribing information (NDA 217806, Eli Lilly), revised 8/2026, 136 PDF pages. GIP and GLP-1 receptor agonist, subcutaneous, once weekly, for weight reduction/maintenance in adults with obesity or overweight with a comorbidity, and for moderate to severe obstructive sleep apnea (OSA) with obesity. Doses are of tirzepatide in mg per injection.

## Model and parameters
No model is given; the label reports summary PK (12.3, PDF p17-18):
```yaml
parameters:
  - {name: tmax, value: 24, unit: h, source: "12.3 Absorption, PDF p17", note: "median; range 8 to 72 h"}
  - {name: F_absolute, value: 0.80, unit: fraction, source: "12.3 Absorption, PDF p17", note: "mean absolute SC bioavailability; abdomen, thigh, upper arm similar"}
  - {name: Vss_apparent_obesity, value: 9.7, unit: L, source: "12.3 Distribution, PDF p17", note: "mean, CV 29%, overweight or obesity"}
  - {name: Vss_apparent_OSA, value: 11.8, unit: L, source: "12.3 Distribution, PDF p17", note: "mean, CV 37%, OSA with obesity"}
  - {name: CL_apparent, value: 0.06, unit: L/h, source: "12.3 Elimination, PDF p17", note: "population mean, CV ~20%, obesity and OSA"}
  - {name: half_life, value: "5-6", unit: days, source: "12.3 Elimination, PDF p17", note: "overweight/obesity and OSA; Overdosage section says approximately 5 days"}
  - {name: protein_binding, value: 99, unit: "%", source: "12.3 Distribution, PDF p17", note: "plasma albumin"}
```
Steady state after 4 weeks of weekly dosing; exposure dose-proportional; PK similar between healthy subjects, obesity and OSA (p17). Metabolism: proteolytic cleavage, beta-oxidation of the C20 fatty diacid, amide hydrolysis; intact drug not found in urine or feces (p17). Dosing (2.1-2.2, PDF p4): start 2.5 mg SC weekly x4 weeks, raise in 2.5 mg steps after >=4 weeks; maintenance 5, 10 or 15 mg weekly (OSA 10 or 15 mg); maximum 15 mg weekly. Missed dose: give within 4 days (96 h); dose interval at least 3 days (72 h) (2.3).

## Pharmacodynamics and end points
Weight loss, reduced calorie intake, glucose-dependent insulin secretion, reduced glucagon, delayed gastric emptying (largest after the first dose, diminishing over time) (12.2, PDF p17). No ke0, tPeak, EC50, MEAC or concentration-effect values are given; no therapeutic concentration range. Not an opioid; opioid audit not applicable.

## Covariates
Age 18-84 y, sex, race, ethnicity and body weight: no clinically relevant effect on PK (PDF p18). Renal impairment (mild to ESRD, single 5 mg) and hepatic impairment (mild to severe): no PK change (p18, 8.6-8.7). Gastric emptying: after a first 5 mg dose, acetaminophen Cmax fell 55% (tmax 1 h later); oral contraceptive Cmax fell 55-66%, AUC 20-23%, tmax delayed 2.5-4.5 h (p18). Lactation: 5 mg SC single dose in 11 women, milk concentration undetectable (LOD 4 ng/mL) in 164 of 171 samples (8.2).

## Caveats
A label summarises; no per-parameter derivations, no covariate equations. The statement that body weight has no clinically relevant effect on PK is a labelled summary and does not contradict the population model's weight scaling (see below), since the model's allometric effect is modest across the label population. Doses in the 5 to 15 mg range; apparent V and CL are per F.

## Used by stanpumpR
The context file lists no repo locations (cited_in_repo false). The library models tirzepatide in R/drugs_tirzepatide.R (Schneck and Urva 2024 population PK), registered as Mounjaro and as a separate Zepbound alias in inst/extdata/antidiabeticRegistry.csv. Only the Mounjaro label is listed in inst/extdata/fdaLabels.csv.

## Verification against stanpumpR
Independent check of the tirzepatide model against this label (90 kg, 170 cm woman; the model's own check subject):
- Apparent CL: model 0.0329*(90/70)^0.8/0.655 = 0.061 L/h; label ~0.06 L/h. **matches**.
- Apparent Vss: model (Vc 2.47 + Vp 3.98)*scale, scale = (FFM 50.9 + 0.482*FM 39.1)/70 = 1.0 (derived), so V = 6.4 L true, /0.655 = 9.8 L apparent; label 9.7 L (obesity). **matches**.
- Half-life: Euler integration of the model's two compartments (derived) gives a terminal half-life of ~5.1 d; label 5-6 d. **matches**.
- tmax: the same simulation puts the peak at ~29 h (derived); label median 24 h, range 8-72 h. **matches** (within the range).
- Bioavailability: model F 0.655 versus label mean absolute 80% (model's 0.8 is the absolute value; the 0.819 factor is a Schneck between-study term for non-phase-1 studies). Label cannot confirm; **cannot verify** (open question).
- Units: model uses L/min and ng/mL with ka 0.0373/h converted to /min; dose mg SC weekly in drugDefaults_global.csv:130 matches the label's weekly mg dosing. No discrepancy found.
