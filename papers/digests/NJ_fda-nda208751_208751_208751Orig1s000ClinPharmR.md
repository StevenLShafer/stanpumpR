---
file: NJ_fda-nda208751_208751_208751Orig1s000ClinPharmR.pdf
pmid: none
doi: none
citation: "US FDA CDER, Clinical Pharmacology Review, NDA 208751 (Fiasp, insulin aspart injection 100 U/mL, Novo Nordisk), resubmission of 29 March 2017 (original NDA 9 December 2015), Reference ID 4149259; reviewers S. Wickramaratne Senarath Yapa and M. Khurana. https://www.accessdata.fda.gov/drugsatfda_docs/nda/2017/208751Orig1s000ClinPharmR.pdf"
kind: review
drugs: [insulinAspart, insulinAspartFiasp]
source_read: "Text layer for the executive summary and clinical-pharmacology summary (printed pp 4-23), the dose-exposure, intrinsic-factor and BMI/age/sex sections (printed pp 36-45), and Appendix 5.3-5.5 (printed pp 84-106). Page images read for PDF pp 16, 19 (printed 15, 18: Figure 3, Table 3), 85 (printed 84: Table 1), 88 and 89 (printed 87, 88: PK/PD parameterisation, Table 4), 91 (printed 90: Table 5), 92 and 93 (printed 91, 92: Figure 3, Table 6), 94 (printed 93: Figure 4). NOT read: printed pp 24-35 and 46-83 (remaining NCA, meal-challenge, phase 3, extrinsic factors, bioanalytical validation, labeling) and the bootstrap/qualification figures (printed pp 96-101). PDF page = printed page + 1."
digest_by: Claude (unattended digest_run.py), 2026-11-25
checked_against: stanpumpR master 8c1ea1bd
verification: partly-verified
open_questions:
  - "[source] Reference weight of Table 5's CL/F and V/F (PDF p 91, printed p 90) is not printed; the repo takes 75 kg (Table 1 mean, PDF p 85) at R/insulinAspart.R:223. The review's NCA 'adjusted for body weight (75 kg)' (Figure 20 caption, printed p 46) supports it but the Applicant's statistical-methods report (Appendix 5.4 of the Summary of Clinical Pharmacology Studies) would settle it."
  - "[source] Reference categories and units for the covariate terms (age group 0.905 on CL/F, sex 0.914, dose -0.198, BMI -1.18 on ka, antibody -0.183) are not printed (Table 5, PDF p 91; Table 4 PDF p 89 lists only the categories 'geriatric >=65 y, younger 18-64 y' and 'male, female'). The repo applies 0.905 to age >=65 at R/insulinAspart.R:225-227,247-248 assuming the younger group is the reference; the Applicant's model code would settle it."
  - "[decision] The repo takes the effect-compartment rate for insulin from regular insulin i.v. (0.876/h, R/insulinEffectSite.R:80) rather than this review's P2 = 0.996/h (Table 6, PDF p 93); the repo comment says the two bracket each other. Whether to keep it is a modelling choice."
---

# FDA 2017: Clinical Pharmacology Review of Fiasp (insulin aspart), NDA 208751

## Study
A regulatory review, not a single study. Clinical pharmacology programme: 8 phase 1 studies (euglycaemic clamps and meal challenge; mostly type 1 diabetes) plus 2 phase 3a trials (NN1218-3852 T1DM, -3853 T2DM) giving PD only (PDF p 5-7, printed 4-6). The 2015 submission was refused in part because the original assay was unacceptable; the resubmission uses **total insulin aspart** (free plus anti-insulin-aspart-antibody-bound), in serum, in pmol/L (printed p 4, 14).

Key PK studies (printed pp 14-18): NN1218-3978 (51 patients, clamp, 0.2 U/kg), NN1218-3891 (clamp, 0.2 U/kg; 22 younger + 22 geriatric), NN1218-3922 (meal test, 40 patients, individualised 0.06-0.28 U/kg, concentrations adjusted to 0.2 U/kg). Each subject received both Fiasp and NovoLog.

Population PK-PD data set (Appendix 5.3, Table 1, PDF p 85, printed p 84; image read): 135 adults with T1DM, 113 younger (18-64) and 22 geriatric (>=65); 89 men, 46 women; age mean 43.2 (SD 15.6) y, range 21-73; **body weight mean 75.0 (SD 10.4) kg, range 50-100**; BMI mean 24.3 (SD 2.2), range 18.9-28.7; antibody binding (%B/T) mean 11.2 (SD 12.5), range 0.3-66.9. NONMEM 7.3, FOCE+I for PK; PK and PD estimated sequentially (printed p 87-88).

## Model and parameters
Structure (printed p 86 and Figure 1): first-order absorption from a dose depot through 3 transit compartments (rate kt each) into a one-compartment disposition with first-order elimination. Different transit rate (kt) and relative bioavailability (F) for Fiasp and NovoLog; disposition identical by assumption (printed p 89). Between-occasion variability on F, kt, ka; between-subject on CL/F and V/F; combined proportional + additive residual error (PDF p 88, printed p 87).

Table 5 (PDF p 91, printed p 90; read on the image):

```yaml
parameters:
  - {name: KA, value: 1.53, unit: "1/h", source: "Table 5 (printed p 90)", note: "absorption rate constant, both products; IOV 47.8% CV"}
  - {name: KT_Fiasp, value: 55.8, unit: "1/h", source: "Table 5", note: "transit rate constant, each of 3 transits; IOV 67.7% CV"}
  - {name: KT_NovoRapid, value: 16.1, unit: "1/h", source: "Table 5", note: "NovoLog (NovoRapid) transit rate constant"}
  - {name: CL/F, value: 67.8, unit: "L/h", source: "Table 5", note: "IIV 25.6% CV; weight exponent 1 (fixed); reference weight not printed"}
  - {name: V/F, value: 78.6, unit: L, source: "Table 5", note: "IIV 39.4% CV; weight exponent 1 (fixed); reference weight not printed"}
  - {name: BA_Fiasp, value: 1, unit: "-", source: "Table 5", note: "relative bioavailability, fixed; IOV 19.6% CV"}
  - {name: BA_NovoRapid, value: 0.99, unit: "-", source: "Table 5", note: "relative bioavailability"}
  - {name: BWonCL, value: 1, unit: "-", source: "Table 5", note: fixed}
  - {name: BWonV, value: 1, unit: "-", source: "Table 5", note: fixed}
  - {name: ABonCL, value: -0.183, unit: "-", source: "Table 5", note: "antibody binding (%B/T) on CL/F; form not printed"}
  - {name: AGEGRPonCL, value: 0.905, unit: "-", source: "Table 5", note: "age group on CL/F; Table 4 categories geriatric >=65 y vs younger 18-64 y; reference not stated"}
  - {name: DOSEonCL, value: -0.198, unit: "-", source: "Table 5", note: "insulin dose on CL/F; reference dose and units not printed"}
  - {name: SEXonCL, value: 0.914, unit: "-", source: "Table 5", note: "sex on CL/F; reference sex not printed"}
  - {name: BMIonKA, value: -1.18, unit: "-", source: "Table 5", note: "BMI on ka; reference BMI not printed"}
  - {name: additive_error, value: 12.6, unit: "pmol/L", source: "Table 5", note: residual}
  - {name: proportional_error, value: 9.49, unit: "%", source: "Table 5", note: residual}
```

No standard errors or CIs are given for the PK estimates (RSE, CI columns "NA"): the covariance step failed, so uncertainty was by bootstrap (printed p 98, figure not read).

Derived (derived: CL/V = 67.8/78.6 = 0.863 /h, t1/2 = 48 min). The review's non-compartmental terminal half-life is longer: geometric mean 68.1 min (Fiasp, median 65.5) and 67.8 min (NovoLog, median 64.4) in NN1218-3978 (printed p 18). Unit conversion: serum values are pmol/L; 1 U insulin = 6 nmol is not stated in the review (the repo's assumption).

Non-compartmental results (printed pp 14-19; Fiasp vs NovoLog, 0.2 U/kg):
- Onset of appearance in NN1218-3978: mean 2.53 min vs 5.24 min (difference -2.71 min [-3.26; -2.16]). Time to 50% Cmax earlier by 9.41 min, tmax earlier by 10.42 min (printed p 15-16).
- Exposure ratio Fiasp/NovoLog in the first 15 min: 1.93 to 3.55 across studies (printed p 16).
- Total exposure ratio AUC0-tlast: 0.98 [0.89;1.09], 1.13 [0.88;1.43], 0.99 [0.95;1.03] (studies 3978, 3891, 3922); Cmax ratio 1.02, 1.06, 1.05 (printed p 18).
- Late exposure AUC2h-t pooled: 442.45 (Fiasp) vs 505.56 (NovoLog) pmol h/L, ratio 0.88 [0.82;0.93]; time to late 50% Cmax pooled 144.23 vs 158.75 min (Table 3, printed p 18, image read).
- Dose proportionality (study 3922, 0.06-0.28 U/kg, AUC0-8h): Fiasp slope 1.14 [0.99;1.29], Cmax 0.91 [0.65;1.17]; NovoLog AUC slope 1.20 [1.05;1.35], Cmax 0.96 [0.70;1.22] (printed p 38).

## Pharmacodynamics and end points
End point: glucose infusion rate (GIR, mg/kg/min) in euglycaemic clamps in T1DM (not an opioid or anaesthetic drug). PD model (printed p 86-92): effect compartment driven by the PK model's concentration, sigmoid Emax link to smoothed GIR; the PD model is treatment-independent (printed p 90).

Table 6 (PDF p 93, printed p 92; image read): turnover rate constant of the effect compartment **P2 = 0.996 /h** (95% CI 0.88-1.11; RSE 5.93%; IIV 31.9% CV); insulin sensitivity SI 183 mg/kg/min per nmol/L; X0 0.175 nmol/L; ALFA (nonlinearity) 20.1 L/nmol; Hill coefficient GAMMA 2.42; covariate terms ABonSI -0.101, AGEGRPonSI 0.872, BMIonSI -1.47, SEXonSI 1.08; additive error 0.449 mg/kg/min. (derived: ke0 = P2 = 0.996/h, t1/2ke0 = ln2/0.996 = 0.70 h = 42 min.) No tPeak is stated; Figure 4 (printed p 93) shows effect-compartment concentration peaking later than central concentration, with GIR in between.

Clamp GIR results, 0.2 U/kg (printed pp 19-21): AUC GIR 0-30 min ratio 1.48 [1.13;2.02] (3978); onset of action Fiasp 13 min (SD 8) vs 16 min (SD 10); time to GIRmax 125 vs 135 min; no MEAC, no analgesic concentration; no potency ratio other than the NovoLog comparisons above. Opioid audit: not applicable.

## Covariates
Table 4 (PDF p 89, printed p 88; image read): F and kt by treatment; ka by BMI; CL/F by age group (geriatric >=65 vs younger 18-64), antibody binding, BMI, body weight, insulin dose, sex; V/F by body weight; SI by antibody binding, age group, BMI, sex. Table 5 retains BWonCL, BWonV, ABonCL, AGEGRPonCL, DOSEonCL, SEXonCL, BMIonKA (a BMI term on CL/F listed in Table 4 has no Table 5 row).
NCA in T1DM (printed pp 40-45): geriatric/young total exposure 1.09 [0.86;1.38], Cmax 1.06 [0.88;1.28] (Fiasp); sexes comparable; lower BMI gave higher early exposure and Cmax; total exposure rose with antibody level.

## Caveats
PK covers T1DM adults only (50-100 kg, BMI 19-29). In study 3922 concentrations were below the LLOQ after 6 h (printed p 36-37), so the late tail is poorly sampled; the Applicant's transit model was also used only to fill the time before the first quantifiable sample (printed p 102). Model fit uses total (antibody-bound included) insulin. No renal/hepatic PK work was done for the model; the review says historical data show no effect (printed p 41). Reviewer states agreement with the Applicant's model (printed p 84) but did not re-derive it.

## Used by stanpumpR
The context file lists no locations (`cited_in_repo: false`), but the checkout cites this review widely:
- R/insulinAspart.R:14-190, 204-227, 237-293 (shared model: ka, kt, CL/F, V/F, relative F, weight scaling, age >=65 factor, header checks).
- R/drugs_insulinAspart.R and R/drugs_insulinAspartFiasp.R (NovoLog kt 16.1/F 0.99; Fiasp kt 55.8/F 1).
- R/insulinEffectSite.R:68-80 (compares P2 0.996/h, Table 6, with its own 0.876/h).
- data-raw/antidiabeticRegistry.R:49, 170-189, 440-456 (registry rows and provenance).

## Verification against stanpumpR
- **Table 5 transcription** (R/insulinAspart.R:208-213 and header 45-57; antidiabeticRegistry.R:443-456): KA 1.53, KT 55.8, KT.NovoRapid 16.1, CL 67.8 L/h, V 78.6 L, BA 1 / 0.99, BWonCL/BWonV 1, ABonCL -0.183, AGEGRPonCL 0.905, DOSEonCL -0.198, SEXonCL 0.914, BMIonKA -1.18 all **match** the rendered Table 5. Units (/h, L/h, L) correct; code converts to min (cl/60, ka/60, kt/60) at R/insulinAspart.R:254-255,289 (**matches**).
- **Table 1 demographics** quoted in the header (R/insulinAspart.R:26-33): 135 adults, 113/22 age split, 89/46 sex, weight 75.0 (SD 10.4), range 50-100, BMI 18.9-28.7: **match**. "21 to 73 years" matches.
- **Structure**: depot, three transits, one compartment: **matches** (printed p 86). Order reversal argument is the repo's own mathematics, not in the source.
- **Reference weight 75 kg**: not printed in Table 5; repo's inference (R/insulinAspart.R:81-84, 223) rests on Table 1's mean and the NCA weight adjustment (Figure 20 caption); **cannot verify** from this PDF alone (open question). Restating CL/F 67.8 x 70/75 = 63.28 L/h and V/F 78.6 x 70/75 = 73.36 L is arithmetic-correct (derived).
- **Figure 4 check** (header 96-108): rendered Figure 4 (PDF p 94) shows central peaks near 560-570 pmol/L (Fiasp, earlier) and a little lower and later for NovoLog, consistent with the repo's pixel readings 564 at 58 min and 556 at 67 min; **consistent**, though I did not remeasure pixels.
- **Age factor**: 0.905 on CL/F for >=65 matches the number; its reference (younger) is the repo's inference; the repo's NCA corroboration 1.09 (printed p 43) matches the review text. **Partly verified**.
- **Half-life check** (header 168-177): CL/V = 0.863/h gives 48 min (derived); review's NCA 68.1 and 67.8 min (printed p 18): **matches** the repo's statement. Dose-proportionality slope 1.14 quoted by the repo (printed p 38) is the Fiasp AUC0-8h slope; for NovoLog the slope is 1.20: **matches** for Fiasp, repo does not distinguish.
- **Terms not applied** (sex, dose, BMI, antibody): the repo's reasons (references not printed) are consistent with Table 4/5.
- **Effect-site rate** (R/insulinEffectSite.R:68-69): P2 0.996 /h, CI 0.88 to 1.11, Table 6 printed p 92: **matches** the rendered table. Repo uses 0.876/h (own derivation); not a transcription error.
- **Analyte**: total insulin aspart in pmol/L; 6 pmol/L per mcU/mL conversion is the repo's convention, not from this document; Figure 4 is consistent with it.
