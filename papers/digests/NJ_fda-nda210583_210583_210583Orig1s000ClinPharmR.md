---
file: NJ_fda-nda210583_210583_210583Orig1s000ClinPharmR.pdf
pmid: none
doi: none
citation: "US FDA CDER. Clinical Pharmacology and Biopharmaceutics Review(s), NDA 210583, ANJESO (meloxicam injection, N1539, 30 mg/mL IV nanocrystal dispersion; Recro Pharma; 505(b)(2), reference Mobic). Cycle-1 review D. Kwatra, Y. Xu, pharmacometrics K. Krudys, signed 04/24/2018 (submission 7/26/2017); cycle-2 review D. Kwatra, Y. Xu, signed 03/08/2019. Posted under drugsatfda_docs/nda/2020 (approval year). https://www.accessdata.fda.gov/drugsatfda_docs/nda/2020/210583Orig1s000ClinPharmR.pdf"
kind: review
drugs: [meloxicam]
source_read: "text layer for the whole document (43 PDF pp.: cycle-2 review; cycle-1 review sections 1-3 and appendix 4.1 pharmacometrics); page images (Flate-decoded) of PDF pp.39-41 for appendix 4.1.1 Tables 2, 3 and 4, which are images. Sponsor label, synopses and 28+5 pages withheld as (b)(4)."
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# FDA 2018/2019 (posted 2020): NDA 210583 ANJESO clinical pharmacology review

## Study
Seven pooled studies for the population PK (appendix 4.1.1, Table 1): Phase 1 N1539-01 (SAD 15/30/60 mg IV and PO,
25.5 mg/mL product), N1539-03 (MAD 30/60 mg daily, 25.5 mg/mL), REC-15-018 (renal impairment, 30 mg), REC-15-019
(TQT, 30/120/180 mg); Phase 2 REC-15-014 and Phase 3 REC-15-015, -016 (bunionectomy, abdominoplasty; 30 mg).
Final dataset 316 subjects, 3496 concentrations (26 samples of 2 subjects removed). Median (range) age 40 (18-75)
years, weight 71 (46-122) kg; 76.3% female, 66.1% White, 94.3% < 65 y; renal function 147 normal, 161 mild,
8 moderate (eGFR 45-59), 0 severe (Table 2). Phoenix NLME 7.3. Assay HPLC-MS/MS (method MLXHPP), range
10.0-10,000 ng/mL, K2EDTA plasma, total meloxicam. Dose 30 mg IV bolus over 15 s once daily.

## Model and parameters
Three compartments, weight power functions in the base model, mixed additive and proportional residual error;
stepwise covariate search; final covariates eGFR and study on CL. Table 3 (source RECR-PCS-102 Table 6), typical
values, page image p.31 of 66:

```yaml
parameters:
  - {name: CL, value: 0.416, unit: L/h, source: "Table 3", note: "x (WT/70)^0.761 x (eGFR/91)^0.554 x 0.64 if study N1539-01 or -03; BSV 37.6%, shrinkage 5.357%"}
  - {name: V (central), value: 4.16, unit: L, source: "Table 3", note: "x (WT/70)^0.776; BSV 24.7%, shrinkage 10.436%"}
  - {name: CL2, value: 6.171, unit: L/h, source: "Table 3", note: "x (WT/70)^0.761; 'peripheral clearance'"}
  - {name: CL3, value: 0.835, unit: L/h, source: "Table 3", note: "x (WT/70)^0.761; 'deep tissue clearance'"}
  - {name: V2, value: 2.06, unit: L, source: "Table 3", note: "x (WT/70)^0.776"}
  - {name: V3, value: 3.28, unit: L, source: "Table 3", note: "x (WT/70)^0.776"}
  - {name: additive_error, value: 13.3, unit: ng/mL, source: "Table 3"}
  - {name: proportional_error, value: 15.2, unit: "%", source: "Table 3"}
```
Vss = 9.50 L at 70 kg (derived: 4.16 + 2.06 + 3.28).

Table 4 (sponsor's predictions, 30 mg): eGFR 120 mL/min CL 0.485 L/h, t1/2gamma 15 h, AUCinf 61856 ng.h/mL;
eGFR 91 (population median) 0.416 L/h, 17 h, 72115; 75: 0.374, 19 h, 80214 (ratio 1.30); 45: 0.282, 24 h,
106383 (1.72); 22.5: 0.189, 36 h, 158730 (2.57). Footnote defines eGFR as creatinine clearance, units mL/min;
the equation is not named anywhere in the review.

Noncompartmental (cycle-1 Table 2.4.1, N1539-01, n = 7 per arm): 30 mg IV Cmax 5562.86 (SD 1114.86) ng/mL,
AUCinf 107508.7 ng.h/mL, t1/2 23.3 (9.36) h; Mobic 15 mg PO Cmax 1221.9 ng/mL, tmax 6.57 h, t1/2 26.4 h.
Relative BA IV:PO 1.03-1.07 (Mobic label absolute oral F 0.89). Multiple dosing (N1539-03): accumulation ratio
2.2 (30 mg) and 2.7 (60 mg); t1/2 median 22.8 h (Day 1), 24.6 h (Day 13) at 30 mg. Renal study REC-15-018:
mild impairment / normal GMR Cmax 105%, AUC 107%; t1/2 21.3 vs 17.5 h (mean). Dose proportional 15-180 mg.

## Pharmacodynamics and end points
No PD model, ke0 or effective concentration. End points were SPID24/SPID48 pain scores. The reviewer did not
accept the sponsor's exposure-response analysis (SPID24 does not address sustained 24 h efficacy; high rescue
use) and rejected an earlier PK/PD executive summary (poor fit, PK from one of three studies) (appendix 4.1.2).
Cycle 2: no adequate efficacy data for onset in the first 4 h after the first dose. Potency: meloxicam stated
10-fold more potent on COX-2 than COX-1 (p.11). Not an opioid; no MEAC.

## Covariates
CL: (WT/70)^0.761 x (eGFR/91)^0.554 x 0.64 for the two early studies (attributed by the sponsor to formulation
concentration 25.5 vs 30 mg/mL, injection rate and bioanalytical method). Volumes (WT/70)^0.776; Q2, Q3
(WT/70)^0.761. Age, sex, race, surgery type, liver function, dose not retained.

## Caveats
Reviewer: some misspecification at late times (CWRES trend), probably steady-state data of N1539-03 with a
different formulation and assay; caution extrapolating Table 4 to moderate/severe renal impairment (lowest
eGFR 45; five of eight "moderate" were > 55). The eGFR equation and whether it was indexed to 1.73 m2 are not
stated; Table 4 calls it creatinine clearance in mL/min. Note Table 4 predicts a 1.30 AUC ratio at eGFR 75
whereas the dedicated mild-impairment study found 1.07.

## Used by stanpumpR
- R/drugs_meloxicam.R:3, 47-72, 74-92, 113-115, 172-184, 194-196 — intravenous ANJESO three-compartment model,
  study factor, eGFR equation, Vss and half-life checks, rejection of exposure-response, reference strings.
- inst/help/drugs/meloxicam.md:15, 19 — help text of the IV model and its provenance.
- tests/testthat/test-drugs-meloxicam.R:2-3 — header naming the model.
- docs/nsaid-models.md:14, 85-96 — model summary and open checks; docs/adding-a-drug.md:151 — route-limited
  parallel system example.

## Verification against stanpumpR
- R/drugs_meloxicam.R:54-56 and 177-183: CL 0.416 x (WT/70)^0.761 x (eGFR/91)^0.554 L/h; Vc, V2, V3 4.16,
  2.06, 3.28 x (WT/70)^0.776 L; Q2, Q3 6.171, 0.835 x (WT/70)^0.761 L/h; /60 to L/min. Table 3 identical.
  **matches**.
- R/drugs_meloxicam.R:58-59: study factor 0.64 only for N1539-01/-03, 1 for the marketed formulation.
  **matches**.
- R/drugs_meloxicam.R:51-52: "section 4.1.1, Table 3: 316 subjects, 3496 concentrations, three compartments".
  **matches**.
- R/drugs_meloxicam.R:59-62: Vss 9.5 L (derived 9.50, matches); terminal half-life 17 h equals Table 4's
  t1/2gamma at eGFR 91; reviewer noted late-time misfit. **matches** (the label's 9.63 L and "about 24 h" are
  not in this review; the review's own NCA half-lives are 17.5-24.6 h, consistent).
- R/drugs_meloxicam.R:64-66, inst/help/drugs/meloxicam.md:19, docs/nsaid-models.md:89-91: "values ... await a
  check against its Table 3". Now checked: all values match. The pending-check notes are stale (not a value
  error).
- R/drugs_meloxicam.R:66-70, 174, 195; help md:15: eGFR by CKD-EPI 2009 (mL/min/1.73 m2). Review: equation not
  named; Table 4 footnote says "eGFR = creatinine clearance", mL/min. **cannot verify**; the review hints at a
  non-indexed creatinine clearance, so the assumption deserves a note.
- R/drugs_meloxicam.R:71-72: eight moderate, no severe. **matches**.
- R/drugs_meloxicam.R:91-92: reviewer did not accept sponsor's IV exposure-response. **matches** (4.1.2).
- Citation year "2020" (R/drugs_meloxicam.R:51, 114; help md:15; docs): the reviews were signed 2018 and 2019;
  2020 is the posting/approval folder. Minor citation imprecision, not a value error.
- docs/adding-a-drug.md:151: describes code structure only; consistent.
