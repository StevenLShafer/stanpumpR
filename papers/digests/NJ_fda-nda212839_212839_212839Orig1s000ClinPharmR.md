---
file: NJ_fda-nda212839_212839_212839Orig1s000ClinPharmR.pdf
pmid: none
doi: none
citation: "FDA Office of Clinical Pharmacology. Integrated Review, NDA 212839 (XCOPRI, cenobamate tablets; SK Life Science), Clinical Pharmacology and Biopharmaceutics Review(s), 212839Orig1s000ClinPharmR, signed 09/09/2019, Reference ID 4488162. https://www.accessdata.fda.gov/drugsatfda_docs/nda/2019/212839Orig1s000ClinPharmR.pdf"
kind: review
drugs: [cenobamate, phenytoin, phenobarbital, carbamazepine, valproate]
source_read: "text layer, whole document (46 PDF pages; PDF page = printed page + 1). Read: TOC (p3), 1 Executive summary (p4-6), 2 Summary incl. 2.2 dosing/individualization (p6-8), 3.2 PK characteristics table (p9-11), 3.3.1-3.3.5 incl. hepatic/renal/age/sex/race, DDI tables, food, BE (p11-25), 4.1 bioanalysis (p26-27), 4.2 population PK incl. 4.2.3 monotherapy simulations (p28-36), 4.3 exposure-response (p37-41), 4.4-4.5 (p41-45). Rendered and read as images: PDF p17, 18, 19, 20, 22, 23, 29, 30, 31, 35, 36, 42."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# FDA 2019: NDA 212839 (XCOPRI) clinical pharmacology review

## Study
Regulatory review of 19 clinical pharmacology studies for cenobamate (YKP3089) in partial-onset seizures in adults
(Executive summary, PDF p4, printed 3). Single doses studied up to 750 mg, multiple doses up to 500 mg/day for 35 days. Assay:
chiral LC-MS/MS in heparin plasma, calibration 0.02-25 ug/mL (p9; validation 0.0200-10.0 ug/mL, LLOQ 0.0200 ug/mL,
p26). Healthy-subject DDI studies were run mostly at cenobamate 200 mg/day; the reviewers note DDIs at 400 mg/day
were not evaluated (PDF p9, p20).

The sponsor's population PK model (report SK16007, "sk16007-pop-pk.pdf"): 8700 observations from 960 subjects, single
doses 5-750 mg and daily doses 50-600 mg; 14.2% of subjects at 100 mg/day, 67.5% at 200 mg/day, 9.3% at 400 mg/day;
8 phase 1, 2 phase 2 and 1 phase 3 studies (Section 4.2.1, PDF p28; study list Table 1, PDF p29). Structural model:
two compartments, first-order absorption with lag time (Figure 6, PDF p30; from SK16007 p53). IIV exponential;
residual error proportional + additive.

## Model and parameters
Table 2 (PDF p31, printed 30; source line "sk16007-pop-pk.pdf page 54 of 157"), rendered and read. Estimates (RSE%):

```yaml
parameters:
  - {name: ka, value: 2.74, unit: 1/h, source: "Table 2, PDF p31", note: "RSE 6.61%; 95% CI 2.38-3.09"}
  - {name: CL/F, value: 0.533, unit: L/h, source: "Table 2, PDF p31", note: "RSE 1.38%; 95% CI 0.518-0.547"}
  - {name: V1/F, value: 42.4, unit: L, source: "Table 2, PDF p31", note: "RSE 1.79%"}
  - {name: tlag, value: 0.240, unit: h, source: "Table 2, PDF p31", note: "RSE 0.893%"}
  - {name: Q/F, value: 1.84, unit: L/h, source: "Table 2, PDF p31", note: "RSE 9.74%"}
  - {name: V2/F, value: 7.27, unit: L, source: "Table 2, PDF p31", note: "RSE 7.78%"}
  - {name: theta7 dose on CL/F, value: -0.0280, unit: "not given", source: "Table 2, PDF p31", note: "labelled 'Exponential dose effect on CL/F'; no unit in this table"}
  - {name: theta8 BSA on CL/F, value: 0.611, unit: m^-2, source: "Table 2, PDF p31", note: linear}
  - {name: theta9 clobazam on CL/F, value: -0.190, unit: fraction, source: "Table 2, PDF p31"}
  - {name: theta10 bilirubin on CL/F, value: -0.0119, unit: "IU (as printed)", source: "Table 2, PDF p31", note: "RSE printed as -9.69"}
  - {name: theta11 weight on V1/F, value: 0.0120, unit: kg^-1, source: "Table 2, PDF p31", note: linear}
  - {name: theta12 carbamazepine on CL/F, value: 0.150, unit: fraction, source: "Table 2, PDF p31"}
  - {name: theta13 African American on CL/F, value: -0.147, unit: fraction, source: "Table 2, PDF p31"}
  - {name: theta14 African American on V1/F, value: -0.0935, unit: fraction, source: "Table 2, PDF p31"}
  - {name: theta15 Hispanic on CL/F, value: -0.133, unit: fraction, source: "Table 2, PDF p31"}
  - {name: omega2 ka, value: 1.10, unit: variance, source: "Table 2, PDF p31"}
  - {name: omega2 CL, value: 0.106, unit: variance, source: "Table 2, PDF p31"}
  - {name: omega2 V2, value: 0.0138, unit: variance, source: "Table 2, PDF p31"}
  - {name: sigma2 proportional, value: 0.0177, unit: variance, source: "Table 2, PDF p31"}
  - {name: sigma2 additive, value: 0.00147, unit: variance, source: "Table 2, PDF p31"}
  - {name: median BSA (reviewer simulation), value: 1.9, unit: m^2, source: "Section 4.4, PDF p42 (printed 41)"}
  - {name: median bilirubin (reviewer simulation), value: 5.13, unit: umol/L, source: "Section 4.4, PDF p42"}
  - {name: median weight (reviewer simulation), value: 77, unit: kg, source: "Section 4.4, PDF p42"}
  - {name: Vd/F, value: "40-50", unit: L, source: "Section 2.1, PDF p6; 3.2 table, PDF p10", note: estimated}
  - {name: terminal half-life, value: "50-60", unit: h, source: "Section 2.1, PDF p6", note: "single doses 100-400 mg"}
  - {name: AUCtau steady state healthy 200 mg/day, value: 482, unit: ug*h/mL, source: "3.2 table, PDF p10"}
  - {name: AUCtau steady state healthy 400 mg/day, value: 861, unit: ug*h/mL, source: "3.2 table, PDF p10"}
```

Covariate text (PDF p30) says CL/F "increases with increasing cenobamate dose", but theta7 is negative (-0.0280) and the
functional form of the dose term is not written out anywhere in the review; the sign and the text cannot both hold
for a simple power or exponential term. Unresolved from this document.

Other PK facts: absorption >88% (mass balance); median Tmax 1-4 h (10-400 mg, fasted); protein binding 60%,
blood/plasma 0.6; metabolism mainly UGT2B7 (less UGT2B4), CYP2E1, 2A6, 2B6 (less 2C19, 3A4/5); unchanged drug 6.8% of
the dose (6.4% urine); radioactivity 93.0% recovered (87.8% urine, 5.2% faeces) (PDF p6, p10-11). Single-dose AUC
greater than dose-proportional (5-750 mg), multiple-dose PK essentially dose-proportional (50-500 mg/day);
accumulation about 5-fold; inter-subject variability up to 25% (Cmax) and 35% (AUC0-t) (PDF p10). High-fat food:
GMR AUC0-inf 0.90 (0.85-0.95) in the 3.2 table (PDF p10); tablet food study (n=13) Cmax ratio 0.902, AUC ratios 0.982
and 0.999 (PDF p24-25). Capsule and tablet bioequivalent (PDF p25).

Applicant's monotherapy simulations (Section 4.2.3, Tables 3-4, PDF p35-36, rendered): 1000 replicates x 1000 virtual
subjects, covariates resampled from the analysis set. Geometric mean steady-state AUC (ug*h/mL), monotherapy:
186.61 (100 mg/day), 382.78 (200), 769.74 (400); with carbamazepine GMR 0.86996 (0.81603-0.92496); with clobazam GMR
1.2357 (1.1466-1.3386) at every dose. Arithmetic means, monotherapy: 199.42, 409.30, 822.23 (derived: 200/382.78 =
CL/F 0.52 L/h at 200 mg/day).

## Pharmacodynamics and end points
No ke0, no effect-site model. Exposure-efficacy (Section 4.3.1, Table 5 is an image not rendered): generalized
Poisson model of daily seizure count, 655 subjects (c013 and c017), treatment effect a power function of exposure.
Applicant simulations: seizure rate reduced on average 30%, 48% and 63% at 100, 200 and 400 mg/day; observed in c017
35.5%, 55%, 55% (PDF p11). Slower label titration reaches about half the maximum effect at 5 weeks and the maximum at
14-18 weeks (reviewer simulation, PDF p15). No therapeutic or toxic concentration range is given. QTcF shortening:
-10.8 ms (90% CI -13.4 to -8.2) day 35 and -18.4 ms (-21.5 to -15.2) day 63 (PDF p9). Exposure-safety for ALT/AST
and DRESS not analysed (too few events).

## Covariates
Population PK: dose, BSA (+), bilirubin (-), carbamazepine (+15%), clobazam (-19%), African American (-14.7% CL/F,
-9.35% V1/F), Hispanic (-13.3%, labelled on CL/F in Table 2, in the Figure 6 text and in Section 3.3.3, PDF p18),
weight (+, linear) on V1/F. Other ASMs tested and not retained: lacosamide, lamotrigine, levetiracetam, oxcarbazepine,
topiramate, valproate (PDF p23). The review does not mention creatinine clearance as a tested covariate.

Special populations (rendered PDF p17): hepatic, single 200 mg, 8 per group, GLSM ratio AUC0-inf 1.93 (1.51-2.49)
mild and 2.30 (1.79-2.95) moderate, Cmax 1.23 and 0.90; titration doses halved and maximum 200 mg/day in mild and
moderate; severe not studied (post-marketing requirement). Renal (Table 2 on PDF p18, rendered): 200 mg in normal, mild,
moderate; 100 mg in severe. AUC0-inf ratio 1.37 (1.11-1.70) mild, 1.48 (1.17-1.86) moderate, 0.88 (0.71-1.09) severe;
severe result called unexplained; "use with caution", no dose cap stated; not recommended on haemodialysis. Age: AUC
6% higher in elderly, not clinically meaningful. Sex: no difference at 150 mg/day.

DDI (rendered PDF p20, p23). Cenobamate (mostly 200 mg/day) on other drugs, AED+cenobamate/AED alone, Cmax and
AUC0-tau: divalproex 1.0477, 1.0968; phenobarbital 1.3380, 1.3745; phenytoin 1.6700, 1.8417; carbamazepine study
C011 0.6634, 0.6547; carbamazepine study C014 0.7691, 0.7646. Probes at 200 mg/day: midazolam AUC -72%, Cmax -61%;
omeprazole AUC +83%, Cmax +107%; bupropion total AUC -39%, Cmax -23% (PDF p7, p21-22). Other drugs on cenobamate,
ratio Cmax and AUC0-tau: divalproex 1.0000, 1.0897; phenobarbital 0.9027, 0.8450 (CI 0.7739-0.9227); phenytoin
0.7306, 0.7170; carbamazepine 0.9733, 0.9736. The text calls only phenytoin significant (27-28% decrease), although
the phenobarbital AUC CI lies wholly below 1. Recommendation in this review: phenytoin dose decreased by "at least
50%" (PDF p5, p7).

## Caveats
A regulatory review of an unpublished sponsor report: no equations for the covariate model, no reference (centring)
dose, and the dose-term sign conflicts with its own text. The medians on PDF p42 are the reviewer's simulation inputs,
called medians; the review does not say they are the centring values of the covariate terms. DDI magnitudes at
400 mg/day are unknown. The population analysis concluded healthy subjects and patients had comparable PK
(PDF p10).

## Used by stanpumpR
Context file: reproduces SK16007's parameter table (pp 53-54), stated identical to Vashi 2023 Supplemental Table 2.
Locations at 8baa733: R/drugs_cenobamate.R:17-23 (source and parameters), :28-36 (no medians anywhere; dose-term
unit), :57-62 (Hispanic term), :63 (clobazam/carbamazepine), :223-224 (reference); inst/help/drugs/cenobamate.md:3,
:9; tests/testthat/test-drugs-cenobamate.R:1-3 and :6-16 (pins of Table 2 values).

## Verification against stanpumpR
- **Parameters** (R/drugs_cenobamate.R:21-22, :267-271; test :8-12): ka 2.74/h, lag 0.240 h, CL/F 0.533 L/h,
  V1/F 42.4 L, Q/F 1.84 L/h, V2/F 7.27 L; code converts to per-minute (cl/60, ka/60, lag x 60 = 14.4 min). All match
  Table 2. **matches**.
- **Source description** (R:11-19, help md:3): 960 subjects, 8700 concentrations, 8 phase 1 / 2 phase 2 / 1 phase 3,
  5-750 mg single, 50-600 mg/day multiple, two thirds at 200 mg/day (67.5%), report pages 53-54 (Figure 6 p53,
  Table 2 p54). **matches**.
- **"No median is printed anywhere in the paper, its supplement, the FDA review or the EMA assessment report"**
  (R/drugs_cenobamate.R:30-33; help md:9 "Neither the paper nor the FDA or EMA assessment reports print the
  medians"). The FDA review, Section 4.4 (PDF p42, printed 41, rendered), prints median BSA 1.9 m^2, median bilirubin
  5.13 umol/L and median weight 77 kg, used by the reviewer with the typical parameters to simulate the mean profile.
  **discrepancy** for the claim as worded. Whether these are the centring values X~ is not stated, and the reference
  dose is still not printed. (derived, only if they are the centring values: a 70 kg, 1.81 m^2 man would get
  V1/F 42.4 x (1 + 0.012 x (70 - 77)) = 38.8 L and CL/F 0.533 x (1 + 0.611 x (1.81 - 1.9)) = 0.50 L/h.)
- **Dose-term unit** (R:34-36: Supplemental Table 2 gives the dose term's unit as m^-2): in this review's Table 2 the
  dose row has no unit; m^-2 is on the BSA row. The repo also says the two tables are identical (R:17-19, help md:3);
  the numbers are identical as far as this table goes, the unit cell is not. Supplement not read here. **cannot
  verify** the supplement; note only.
- **Hispanic term** (R:57-62, help md:13): repo says the CL/F label is probably wrong (from Vashi Supplemental
  Table 1). This review places it on CL/F in three places (Table 2, PDF p30 text, PDF p18 text). **cannot
  verify** the Vashi argument from this document; not contradicted, but the FDA reproduction does not help it.
- **Clobazam -19%, carbamazepine +15%** (R:63, help md:14): Table 2 and PDF p23. **matches**.
- **Test pins** (test :6-16): match Table 2. **matches**.
- Corroboration (derived): the applicant's simulated monotherapy geometric mean AUC at 200 mg/day, 382.78 ug*h/mL
  (Table 4), against the model's 200/0.533 = 375 ug*h/mL.
