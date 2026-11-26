---
file: NJ_fda-nda203952_203952_203952Orig1s000ClinPharmR.pdf
pmid: ""
doi: ""
citation: "FDA CDER Clinical Pharmacology and Biopharmaceutics Review(s), NDA 203952 (Duopa, levodopa-carbidopa enteral suspension, AbbVie), Reference ID 3463353, resubmission 2013, OCP briefing 27 Feb 2014; posted at accessdata.fda.gov/drugsatfda_docs/nda/2015/203952Orig1s000ClinPharmR.pdf"
kind: review
drugs: [levodopa, levodopaIntestinalGel]
source_read: "Page images read for PDF pp 18, 28, 29, 41 (printed 4, 14, 15, 27) and, from the earlier pass, p 27 (printed 13); text layer for the rest of the Clinical Pharmacology Review (PDF pp 15-17, 20-23, 25, 28-31, 37-44). PDF pp 2-11, 13, 16, 19, 24, 26, 32-36, 45-62 have no text layer (the biopharmaceutics/dissolution review and redacted pages) and were not read. Printed page = PDF page - 14 for PDF pp 15-44."
digest_by: Claude (unattended digest_run.py), 2026-11-25
checked_against: stanpumpR master 8c1ea1bd
verification: partly-verified
open_questions:
  - "[decision] The model's terminal half-life is about 10 h (derived from CL/F 24.8, Vc/F 58.5, Q/F 6.8, Vp/F 72.9: beta = 0.070 /h), while the review's observed harmonic-mean t1/2 after LCIG infusion stops is 1.5 h (Table 7, PDF p29, N = 14) and levodopa 'declined rapidly'. Vp/F is poorly estimated (RSE 49%, bootstrap 22.9-407.8 L, PDF p41). R/drugs_levodopaIntestinalGel.R:34-39 reports the 10 h tail openly; whether the long tail (2.06 mcg/mL 8 h into a 60 mg/h infusion) is wanted, against a 1.5 h observed fall, is for Shafer."
  - "[check] R/drugs_levodopaIntestinalGel.R:41 calls the median time to peak 2.5 h 'the label's'; in this document it is the review's Table 7 comment (PDF p29) and the Executive Summary (PDF p17), N = 18 patients on a 16-h infusion. The Duopa label (NJ_label-duopa) was not read here."
  - "[source] The repo's simulated typical peaks after a 200 mg dose (75 min tablets, 45 min gel; R/drugs_levodopa.R:41-47, R/drugs_levodopaIntestinalGel.R:34-35) are attributed to Othman & Dutta 2014, not to this review; this review states no simulated peak time, so they are unverified here."
---

# FDA 2015: Duopa (levodopa-carbidopa intestinal gel) Clinical Pharmacology Review, NDA 203952

## Study
FDA Office of Clinical Pharmacology review of NDA 203952 (505(b)(2), references Sinemet; Duopa 20 mg/mL levodopa with 5 mg/mL carbidopa monohydrate, given into the jejunum by a CADD-Legacy 1400 pump through a PEG-J tube). Original submission 11/16/12, resubmission 5/28/13 (PDF p15; the pharmacometric review gives "23 May 2014" for the resubmission, PDF p39, an internal inconsistency). Contains: executive summary, a question-based review (QBR), one individual study review (Phase 1 PK study S187-1-002) and the sponsor's population PK (PopPK) analysis, which the FDA reviewer reproduced and endorsed (PDF p42). The remaining pages are a biopharmaceutics (dissolution) review, mostly without text layer.

- Phase 1 PK study S187-1-002: N = 18 advanced-PD patients on stable individual LCIG dose, 16-h infusion on Day -1 and Day 1, up to 24 blood samples over 19 h (PDF p25). Morning dose in the study 4-11.5 mL (80-230 mg levodopa); continuous rate 2.7-6.1 mL/h (54-122 mg/h); extra doses given on the PK day to only 2 patients.
- Phase III: studies 3001/3002 (12-week, double-blind, double-dummy; 15+12 on LCIG, 15+8 on oral levodopa-carbidopa with PK) and open-label 3004 (n = 320 treated, 311 on LCIG with PK) (PDF p22).
- PopPK (PDF pp 39-42): NONMEM, FOCE-I, ADVAN6, 68 patients (45 LCIG, 23 oral levodopa-carbidopa) from S187-1-002 and 3001/3002; external evaluation on the 311 patients of study 3004. Covariates tested: weight, age, sex, entacapone, treatment.

## Model and parameters
Final PopPK model (PDF p40 text, p41 table; read on the page image): two compartments, one transit compartment for absorption, first-order elimination, relative bioavailability for LCIG vs oral tablets, separate KTR and residual variability for LCIG and oral. IIV on CL, Vc and KTR (exponential). Residual error combined additive plus proportional. Weight is a significant covariate on Vc only (exponent 1); CL not related to weight or sex (p > 0.01); entacapone had no significant effect on CL; age nearly significant on CL (p = 0.0057) and not retained (PDF p41).

PDF p41 table (point estimate, %RSE; bootstrap median [95% CI] of 977 converged runs of 1000). The table's labels for the residual-variance rows are partly lost in the image; the assignment below follows the text on PDF p42 (proportional error 15% LCIG vs 29% oral; additive SD 0.3 vs 0.59 mcg/mL).

```yaml
parameters:
  - {name: KTR_LCIG, value: 9.2, unit: 1/h, source: "PDF p41 table (printed 27)", note: "%RSE 19; bootstrap 9.7 [6.0-14.3]"}
  - {name: KTR_oral, value: 2.4, unit: 1/h, source: "PDF p41 table", note: "%RSE 30; bootstrap 2.2 [0.85-5.2]"}
  - {name: CL_F, value: 24.8, unit: L/h, source: "PDF p41 table", note: "%RSE 5; bootstrap 24.4 [20.4-26.8]; apparent, with carbidopa; no weight term"}
  - {name: Vc_F_at_70kg, value: 58.5, unit: L, source: "PDF p41 table", note: "x WT(kg)/70; %RSE 11; bootstrap 56.0 [36.6-71.0]"}
  - {name: Q_F, value: 6.8, unit: L/h, source: "PDF p41 table", note: "%RSE 22; bootstrap 7.9 [4.1-17.2]"}
  - {name: Vp_F, value: 72.9, unit: L, source: "PDF p41 table", note: "%RSE 49; bootstrap 80.3 [22.9-407.8]"}
  - {name: Frel_LCIG, value: 0.97, unit: ratio to oral tablets, source: "PDF p41 table", note: "%RSE 1; bootstrap 0.97 [0.95-0.98]; oral fixed at 1"}
  - {name: omega2_KTR, value: 0.78, unit: variance, source: "PDF p41 table", note: "%RSE 31; text gives IIV 88% (PDF p42)"}
  - {name: omega2_CL, value: 0.11, unit: variance, source: "PDF p41 table", note: "%RSE 17; text gives 33%"}
  - {name: omega2_Vc, value: 0.37, unit: variance, source: "PDF p41 table", note: "%RSE 40; text gives 60%"}
  - {name: omega_KTR_CL_cov, value: -0.14, unit: covariance, source: "PDF p41 table", note: "%RSE 31"}
  - {name: sigma_prop_LCIG_oral, value: "0.03 (LCIG), 0.09 (oral)", unit: variance, source: "PDF p41 table", note: "row labels partly lost; text: 15% vs 29%"}
  - {name: sigma_add_LCIG_oral, value: "0.09 (LCIG), 0.34 (oral)", unit: "variance of (mcg/mL)", source: "PDF p41 table", note: "text: SD 0.3 vs 0.59 mcg/mL"}
  - {name: Vss_F_70kg, value: 130, unit: L, source: "PDF p42 text", note: "approx; 58.5 + 72.9 = 131.4 (derived)"}
```

Non-compartmental results, LCIG 16-h infusion, Phase 1 study S187-1-002, N = 18 (PDF p29, Table 7, mean +/- SD, read on the image):

```yaml
parameters:
  - {name: total_LCIG_dose_day1, value: 1580, unit: mg levodopa, source: "PDF p29 Table 7", note: "SD 403; carbidopa 395 +/- 101"}
  - {name: Tmax_levodopa, value: 2.85, unit: h, source: "PDF p29 Table 7", note: "mean, SD 2.31; reviewer states median 2.5 h; carbidopa median 3.5 h"}
  - {name: Cmax_levodopa, value: 4.21, unit: mcg/mL, source: "PDF p29 Table 7", note: "SD 1.36"}
  - {name: Cmin_levodopa, value: 0.447, unit: mcg/mL, source: "PDF p29 Table 7", note: "SD 0.282"}
  - {name: Cavg_levodopa, value: 2.91, unit: mcg/mL, source: "PDF p29 Table 7", note: "SD 0.836"}
  - {name: AUC0_16_levodopa, value: 46.5, unit: mcg*h/mL, source: "PDF p29 Table 7", note: "SD 13.3"}
  - {name: AUC0_24_levodopa, value: 53.8, unit: mcg*h/mL, source: "PDF p29 Table 7", note: "SD 17.2; N = 14"}
  - {name: half_life_levodopa, value: 1.5, unit: h, source: "PDF p29 Table 7", note: "harmonic mean, pseudo-SD 0.19; N = 14; after the infusion stops"}
  - {name: CL_F_levodopa_NCA, value: 30.7, unit: L/h, source: "PDF p29 Table 7", note: "SD 7.52; N = 14; not tested statistically"}
  - {name: Cmax_3OMD, value: 19.0, unit: mcg/mL, source: "PDF p29 Table 7", note: "SD 5.66; Tmax 8.38 +/- 5.77 h; AUC0-16 273 mcg*h/mL"}
  - {name: Cmax_carbidopa, value: 0.371, unit: mcg/mL, source: "PDF p29 Table 7", note: "SD 0.149; Tmax 5.70 +/- 5.22 h; AUC0-16 3.54 mcg*h/mL"}
  - {name: variability_levodopa_CV, value: "Cmax 32, Cmin 63, Cavg 29, AUC0-16 29", unit: "% CV", source: "PDF p29", note: "N = 18"}
```

Other facts: relative bioavailability of levodopa from LCIG vs oral levodopa-carbidopa tablets 97% (95% CI 95-98%) from PopPK (PDF p17); within-patient variability of levodopa concentration 8.6% (LCIG) vs 15.5% (oral) (PDF p17), consistent with the PopPK residual errors; LCIG absorbed faster than oral, consistent with delivery past the stomach (PDF p42). Dosing: morning dose 5-10 mL (100-200 mg levodopa) over 10-30 min, not to exceed 15 mL (300 mg); continuous 1-10 mL/h (20-200 mg/h), expected 2-6 mL/h (40-120 mg/h); extra doses 0.5-2 mL (PDF pp 25, 31). Hydrazine (a carbidopa degradation product, a known toxicant) detectable in one of 11 LCIG patients (PDF p23).

## Pharmacodynamics and end points
The Phase 1 study had no PD assessment (PDF p27: "PD Assessment: none"). The Phase III primary end point was daily normalised OFF time (PD diary); the review judges that PK and efficacy data from the controlled studies (27 LCIG and 23 oral patients with PK) are too few for an exposure-response analysis and that the PK-efficacy link is not adequately characterised (PDF pp 21-23). No therapeutic or minimum effective concentration is given. No ke0, tPeak or hysteresis. Lower PK variability with LCIG is linked qualitatively to less OFF time (PDF p20-21 text). Not an opioid; no MEAC or potency ratio.

## Covariates
Weight scales Vc linearly (Vc/F 58.5 L x WT/70). No weight or sex effect on CL; no entacapone effect on CL; age (p = 0.0057) not retained. Demographics of the 68 PopPK patients are not given in the pages read.

## Caveats
- Parameters are apparent (/F); oral reference bioavailability is fixed at 1; the review states the same for LCIG as relative, 0.97.
- Vp/F is imprecise (RSE 49%, bootstrap 22.9-407.8 L), so the terminal half-life of the model is poorly determined, whereas observed post-infusion t1/2 is 1.5 h (NCA, N = 14, sampled only about 3 h after the infusion stops) (derived: Figure 2 shows sampling to 19-20 h from start of a 16-h infusion).
- NCA CL/F 30.7 L/h (N = 14) is about 24% above the PopPK CL/F 24.8 L/h (derived: 30.7/24.8 = 1.24); different data and methods, not discussed in the review.
- Phase III data are sparse (30 and 20 completers with PK); only 18 patients in the dedicated PK study.
- Carbidopa is fixed at 4:1 in the gel; the model is not valid for other carbidopa ratios (the repo says this, `R/drugs_levodopa.R:92`; the review does not address it in the pages read).
- Large parts of the document (CMC/dissolution) have redacted or no text and were not read.

## Used by stanpumpR
The context file lists no locations; a search of the checkout shows the review cited at:
- `R/drugs_levodopa.R:15-34, 121-128` and `R/drugs_levodopaIntestinalGel.R:12-17, 45-48`: the levodopa two-compartment disposition (CL/F, Vc/F x WT/70, Q/F, Vp/F), KTR 2.4 and 9.2 /h and Frel 0.97, "confirmed" against Othman & Dutta 2014 by this review ("pp. 25-29", the printed pages).
- `R/drugs_levodopaIntestinalGel.R:41`: median time to peak 2.5 h.
- `data-raw/parkinsonsRegistry.R:318-333` (the registry row giving the PopPK source).
The effect-site (ke0 1.80 /h, EC50 0.812 mcg/mL) and the 1.0-3.5 mcg/mL band come from Mao 2013 and the AGNP guideline, not this review.

## Verification against stanpumpR
- `LEVODOPA_CL` 24.8 L/h, `LEVODOPA_VC` 58.5 L at 70 kg x WT/70, `LEVODOPA_Q` 6.8 L/h, `LEVODOPA_VP` 72.9 L (`R/drugs_levodopa.R:122-125`): **matches** the PDF p41 table (confirmed on the page image). Unit conversions: `cl1 = 24.8/60` L/min and `cl2 = 6.8/60`, `v1 = 58.5 x w/70` L, `v2 = 72.9 x size$volume` (`:146-152`) are correct; with the fat-free-mass switch off the size factors are 1 (the published model), as the header says (`:102-106`).
- `LEVODOPA_KTR_ORAL` 2.4 /h, `LEVODOPA_KTR_GEL` 9.2 /h, `LEVODOPA_FREL_GEL` 0.97 (`:126-128`): **matches** the table; `ka_PO = KTR/60` per minute (`R/drugs_levodopaIntestinalGel.R:65`). The help text's "four times as fast" (9.2/2.4 = 3.8, derived) and interval 0.95-0.98 match.
- Header quote "with a transit compartment for absorption" attributed to review p. 26 (`R/drugs_levodopa.R:23-24`): **matches** (printed p 26 = PDF p40). "age almost reached significance (p = 0.0057)": matches. The header says "1182 concentrations" and "68 patients (45 gel, 23 tablets)": 68/45/23 match (PDF p39); 1182 and the demographics (age 64, weight 73 kg) are from Othman & Dutta, not seen in this review.
- Doses: morning dose 5-10 mL = 100-200 mg over 10-30 min, continuous 2-6 mL/h (40-120 mg/h), 16 h (`R/drugs_levodopaIntestinalGel.R:19-25`): **matches** PDF pp 25, 31 (20 mg/mL gel). Check that 60 mg/h x 0.97 / 24.8 = 2.35 mcg/mL (`:36-37`): arithmetic correct (2.347), but the observed Cavg after a 1580 mg day was 2.91 mcg/mL (PDF p29), i.e. 98.8 mg/h average including the morning dose (derived); the model's steady state at that rate would be 3.9 mcg/mL (derived), so a typical-patient curve at that average rate would sit about a third above the observed Cavg of this study (3.86 vs 2.91; derived), in line with the NCA CL/F being 24% above the PopPK value. (Different patients and a crude average-rate comparison, not a model failure.)
- Median time to peak 2.5 h (`:41`): **matches** the review's median Tmax 2.5 h (PDF p17, p29), but this is the review's, not the label's (open question).
- Terminal behaviour (`:36-39`, half-life near 10 h): the arithmetic from the table parameters is right, but it disagrees with the observed 1.5 h post-infusion half-life in Table 7 (open question, decision).
- Peaks of 75 and 45 min after 200 mg (`R/drugs_levodopa.R:41-48`): **cannot verify** here; the review gives no simulated peak times.
- Verdict: parameter transcription matches everywhere; verdict is partly-verified because of the open questions above.
