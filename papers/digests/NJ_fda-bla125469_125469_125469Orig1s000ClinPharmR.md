---
file: NJ_fda-bla125469_125469_125469Orig1s000ClinPharmR.pdf
pmid: null
doi: null
citation: "US FDA CDER. Clinical Pharmacology and Biopharmaceutics Review(s), BLA 125469 (dulaglutide [rDNA origin] injection, Trulicity; Eli Lilly). Reviewers Sang M. Chung, Lian Ma (pharmacometrics); team leaders Lokesh Jain, Nitin Mehrotra. Submissions 18 Sep 2013 and 22 Apr 2014; signed 06 Jun 2014. Reference ID 3520514. accessdata.fda.gov/drugsatfda_docs/nda/2014/125469Orig1s000ClinPharmR.pdf"
kind: review
drugs: [dulaglutide]
source_read: "text layer plus rendered page images; 116 PDF pages, printed 'Page N of 114' = PDF page N+1. Read: cover and TOC (PDF 1-8); 1 Executive summary (PDF 9-14); 2.1-2.2 general attributes, formulation, dosage (PDF 15-17); 2.3 exposure-response incl. Tables 4-6 (PDF 19-26); 2.4 PK characteristics incl. Tables 8-11 (PDF 27-30); 2.4.3 PD (PDF 30-33); 2.5 intrinsic factors incl. renal Table 14 and hepatic Table 17 (PDF 34-39); 2.6 DDI Tables 18, 20 (PDF 39-45); 2.8 bioanalytics (PDF 45); 3 labeling (PDF 47, a pointer only); appendices GBCB, GBCL, GBDR, GBDT tables (PDF 51-57, 90); 4.2 pharmacometric review in full (PDF 91-111); filing memo and signatures (PDF 112-116). Many pages are image-only: PDF 10, 16, 20, 29, 38, 92, 99, 100, 102, 103, 104 rendered and read. Appendix pages 48-89 only skimmed (many tables are images not rendered)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# FDA 2014: dulaglutide (Trulicity) BLA 125469 clinical pharmacology review

## Study
Not a single study: the FDA Office of Clinical Pharmacology review of Eli Lilly's BLA for once-weekly SC dulaglutide
0.75 and 1.5 mg (prefilled syringe or single-use pen; 1.5 mg in 0.5 mL, PDF 16) in adults with type 2 diabetes.
21 clinical pharmacology studies were reviewed (PDF 113). Assay: validated radioimmunoassay, LLOQ 5.00 ng/mL, ULOQ
50.0 ng/mL (PDF 45). Recommendation: approve both doses; start 0.75 mg and titrate to 1.5 mg after 4 weeks for GI
tolerability, or start at either (PDF 9, 98). Section 3 (labeling) only points to a separate file (PDF 47).
Several passages are redacted (b)(4), including the brand name, the sponsor's proposed dosing text (PDF 16) and the
bioavailability-by-dose function (PDF 103).

Population PK model (sponsor's Phase 3 combined analysis, reviewed in 4.2.3.3, PDF 103): 1067 patients from Phase 3
GBDA and GBDC plus the 0.75 and 1.5 mg arms of Phase 2/3 GBCF; two compartments, first-order absorption, structure
chosen from the absolute bioavailability study GBDR; informative priors from the GBDR model on KA, CL, V2, Q, V3;
IIV on KA fixed to the GBDR value; IIV on CL and V2 with a covariance block; proportional residual error; FOCE with
interaction. Covariates tested: age, weight, BMI, sex, race/ethnicity, serum creatinine, dose, smoking, DPP-4
co-administration, renal function, macroalbuminuria; only baseline weight on F was retained (PDF 103).

## Model and parameters
Final population PK model, **Table 63** "PK and covariate parameters in the base and final model", PDF 104 (printed
p103), source Sponsor's Phase 3 Combined PopPK/PD Report Table 9.2. Confirmed on the rendered page. The list of tables
(PDF 8) numbers this table 64; the caption on the page says 63. Typical values (%SEE); IIV as %CV:

| Parameter | Final base | Final model | IIV final |
|---|---|---|---|
| F1, 0.75 mg dose | 0.474 (2.57) | 0.476 (2.33) | - |
| F1, 1.5 mg dose | 0.470 fixed | 0.470 fixed | - |
| weight on F1 (per kg, about 92.5 kg) | - | -0.00877 (6.61) | - |
| Ka (1/h) | 0.00705 (15.6) | 0.00769 (18.3) | 40.5% |
| CL (L/h) | 0.0604 (1.90) | 0.0593 (1.74) | 33.8% (base 39.4%) |
| V2 central (L) | 2.12 (16.9) | 2.25 (19.2) | 55.6% (base 68.7%) |
| Q (L/h) | 0.0145 (42.8) | 0.0201 (38.4) | - |
| V3 peripheral (L) | 3.18 (14.2) | 3.75 (13.5) | - |
| CL-V2 covariance | 0.220 | 0.155 | |
| proportional residual | 28.5% | 28.7% | |

Footnote b: F1 x exp(theta_WT-F1 x (Weight - 92.5 kg)). The table heading calls F1 "Relative Bioavailability", while
the text (PDF 103) says the 1.5 mg bioavailability was fixed to 47%, "the final estimate from Study GBDR" (the
absolute bioavailability study), and that the 0.75 mg value was estimated with a continuous dose function (FMAX,
FD50, F0; the equation itself is redacted).

Derived checks (mine): CL/F at 92.5 kg 0.0593 / 0.47 = 0.126 L/h; Vss/F (2.25 + 3.75) / 0.47 = 12.8 L; terminal
half-life of the disposition model 7.5 days, distribution 18.8 h; absorption half-time ln 2 / 0.00769 = 90 h
(3.8 days). Typical Cavg,ss = F x dose / (CL x 168 h) at 120 kg is 55.6 ng/mL (1.5 mg, F 0.470) and 28.2 ng/mL
(0.75 mg, F 0.476), which reproduce exactly the review's predicted 55.6 and 28.2 ng/mL for a 120 kg patient
(PDF 106). So 0.476 does act as F for the 0.75 mg dose in the final model.

Noncompartmental results (all geometric means unless stated):
- GBDR absolute bioavailability, healthy volunteers, Table 10 (PDF 29, rendered): 1.5 mg SC (n=16) AUC0-inf 15400
  ng.h/mL, Cmax 80.7 ng/mL, tmax median 48 h (24-72), t1/2 107 h, CL/F 0.0975 L/h, Vz/F 15.1 L, F 0.443 (on
  AUC0-inf) or 0.371 (AUC0-168). 0.1 mg IV over 30 min (n=22): CL 0.0426 L/h, Vz 5.32 L, t1/2 86.6 h, with AUC
  extrapolation over 20% in every subject and t1/2 estimated over less than 2 half-lives (footnotes). SC 0.75 mg vs IM
  relative F 95.8% (PDF 30).
- The executive summary (PDF 12) gives absolute F 44.3%, tmax 48 h, mean V 5.3 L, mean terminal t1/2 4.5 days,
  accumulation about 1.6-fold; section 2.2.1 (PDF 17) instead quotes t1/2 4.7 days and CL 0.107 L/hr "as reported in
  section 2.4"; neither value was found in section 2.4's text or its Table 10 (Table 11, an image on PDF 30, was not
  rendered). The 4.6 day SC vs 3.6 day IV half-lives are on PDF 30. Unresolved within this document; 4.7 days and
  0.107 L/h are the values the repo attributes to Geiser 2016.
- SAD healthy (GBCA, n=22, 0.1-12 mg): tmax about 48 h, t1/2 about 3.7 days; power model Cmax 64.7 x Dose^0.9,
  AUC0-inf 10908 x Dose^0.9 (Table 8, PDF 27). SAD Japanese T2D (GBCB, n=30, 0.3-6 mg): exponent 0.7 for both
  (Table 8); appendix Table 27 (PDF 51-52) CL/F 0.0586-0.116 L/h by dose. MAD T2D (GBCD, n=28, 0.05-8 mg weekly x 5):
  tmax 48 h, t1/2 4.0 days, accumulation 1.43; Cmax 59.9 x Dose^0.8, AUC0-168 6820.9 x Dose^0.9 (Table 9, PDF 28).
  MAD Japanese T2D (GBCL, n=14, 1 and 1.5 mg): t1/2 4.5 days, accumulation 1.51 and 1.57 (PDF 28, 56).
- Device bridging GBDT, 1.5 mg single dose (Table 59, PDF 90): Cmax 91.1 / 88.2 ng/mL, tmax 48 h, t1/2 94.2 / 95.7 h,
  CL/F 0.0913 / 0.0936 L/h, Vz/F 12.4 / 12.9 L (auto-injector / manual syringe).
- Phase 3 combined PK/PD: Cavg,ss 74.0 ng/mL (1.5 mg) and 37.5 ng/mL (0.75 mg) in the population (PDF 106).

```yaml
parameters:
  - {name: F1_1.5mg, value: 0.470, unit: fraction, source: "Table 63, PDF 104", note: "fixed; text PDF 103 says final GBDR estimate"}
  - {name: F1_0.75mg, value: 0.476, unit: fraction, source: "Table 63, PDF 104", note: "estimated, %SEE 2.33; dose function redacted"}
  - {name: theta_WT_on_F1, value: -0.00877, unit: per kg, source: "Table 63 footnote b, PDF 104", note: "F1 x exp(theta x (WT - 92.5))"}
  - {name: ka, value: 0.00769, unit: 1/h, source: "Table 63, PDF 104", note: "final model; IIV 40.5%"}
  - {name: CL, value: 0.0593, unit: L/h, source: "Table 63, PDF 104", note: "final model; IIV 33.8%"}
  - {name: V2, value: 2.25, unit: L, source: "Table 63, PDF 104", note: "central; IIV 55.6%"}
  - {name: Q, value: 0.0201, unit: L/h, source: "Table 63, PDF 104", note: final model}
  - {name: V3, value: 3.75, unit: L, source: "Table 63, PDF 104", note: peripheral}
  - {name: residual_prop, value: 0.287, unit: fraction, source: "Table 63, PDF 104", note: proportional}
  - {name: F_abs_NCA_GBDR, value: 0.443, unit: fraction, source: "Table 10, PDF 29", note: "1.5 mg SC vs 0.1 mg IV, healthy, n=16, AUC0-inf"}
  - {name: CL_IV_NCA_GBDR, value: 0.0426, unit: L/h, source: "Table 10, PDF 29", note: "0.1 mg IV, n=22; AUC extrapolation >20%"}
  - {name: Vz_IV_NCA_GBDR, value: 5.32, unit: L, source: "Table 10, PDF 29", note: "0.1 mg IV"}
  - {name: tmax_SC, value: 48, unit: h, source: "Table 10, PDF 29", note: "median, 1.5 mg SC, range 24-72"}
  - {name: Cavg_ss_1.5mg, value: 74.0, unit: ng/mL, source: "PDF 106", note: "Phase 3 combined PK/PD population"}
  - {name: Cavg_ss_0.75mg, value: 37.5, unit: ng/mL, source: "PDF 106", note: "Phase 3 combined PK/PD population"}
```

## Pharmacodynamics and end points
No effect compartment or ke0 for glycaemia. The sponsor's population PK/PD models (4.2.3.1, PDF 100-102) covered
FPG, HbA1c, body weight and heart rate from GBCF, GBDA and GBDC (0.75 and 1.5 mg, 1-2 years), estimated by Monte Carlo
importance sampling. The FPG-HbA1c model is described only in outline: no structure, equations or parameter values
are printed. Its covariates were baseline FPG and TZD co-medication; the reviewer reports an additional 0.17% HbA1c
fall per 10 mg/dL higher baseline FPG, and at 1 year FPG -35.9 vs -28.1 mg/dL and HbA1c -1.22% vs -0.93% with vs
without a TZD (PDF 102). The reviewer calls these the sponsor's analyses and judges them adequate.

Exposure-response at 52 weeks (Figure 5 / Figure 50, PDF 20 and 92): Phase 2 model-estimated HbA1c and FPG change
vs dulaglutide concentration, saturating above roughly 50 ng/mL (read from the figure), baseline assumptions HbA1c 8.0%
and weight 93 kg. Minimal clinically significant HbA1c fall (-0.4%) needs about 10 ng/mL (PDF 106). Body weight and
heart rate had their own E-R models; the heart-rate effect was about 2.1 bpm at 52 weeks (PDF 107).

Nausea and vomiting: Markov-chain models with a 1 h interval (PDF 101); concentration raised nausea and vomiting
transition probabilities, with tolerance lagging concentration with half-lives of about 990 h (nausea) and 5.2 h
(vomiting). Simulated per-week incidence on starting 1.5 mg vs titration: about 11% nausea and 7% vomiting in week 1
without titration (Figure 57, PDF 99).

Observed PD: in elderly T2D (GBCT, 1.5 mg weekly for 6 weeks) FPG -26.6 mg/dL, 2 h PPG -59.5 mg/dL and PPG AUC
-197 vs placebo (PDF 31); gastric emptying t50 delayed from 1.72 to 3.77 h after the first 1.5 mg dose (GBDM,
PDF 42). QTc: no effect at 4 and 7 mg (PDF 12).

## Covariates
- Weight on F only (above). Reviewer: weight lowered IIV on CL only from 39% to 34%; no dose adjustment by weight
  (PDF 108).
- Renal (GBCM, 1.5 mg single dose, Table 14, PDF 36): AUC0-inf ratio vs control 1.20 mild, 1.28 moderate, 1.14
  severe, 1.12 ESRD; Cmax 1.13, 1.23, 1.20, 1.11; no relation to CrCL (r squared 0.0686 AUC). No dose adjustment;
  caution in severe impairment because long-term safety data are sparse (PDF 9, 37).
- Hepatic (GBDO, Table 17, PDF 38, rendered): AUC0-inf ratio 0.774 mild (n=6), 0.669 moderate (n=5), 0.791 severe
  (n=3); no dose adjustment.
- Age: no difference under vs over 65 (GBCT, PDF 35). BMI 30-45 vs 18-27: AUC0-inf ratio 0.761-0.806 by site
  (GBCN, PDF 34). Injection site (abdomen, arm, thigh) comparable.
- Interactions (Table 18, PDF 41): sitagliptin raised dulaglutide AUC 1.38-fold and Cmax 1.27-fold; dulaglutide
  lowered atorvastatin Cmax to 0.296 and delayed the tmax of several oral drugs.

## Caveats
- The PK parameter table is the sponsor's, reproduced by the FDA (Table 63, source Lilly's report Table 9.2); the
  disease and E-R models are also the sponsor's, with no equations given.
- The dose function for F is redacted, so how F changes between doses other than 0.75 and 1.5 mg cannot be read here.
- F 0.470 (model, GBDR) and 0.443 (NCA, GBDR, AUC0-inf) differ; the IV NCA in GBDR is weak (all AUC extrapolations
  over 20%, short half-life windows), so the IV CL 0.0426 L/h and Vz 5.32 L are not comparable to the model's 0.0593
  L/h and 6.0 L without care.
- Internal inconsistencies: half-life 4.5 days (PDF 12) vs 4.7 days (PDF 17); table numbering in the list of tables
  vs the captions (pharmacometric section, e.g. Table 63 vs 64).
- The phase 3 doses were 0.75 and 1.5 mg; nothing in this review covers 3 or 4.5 mg.

## Used by stanpumpR
- `R/drugs_dulaglutide.R:5-6` (at 8baa733): concentration only; "the FDA's fasting glucose and HbA1c disease model is
  not plotted".
- `R/drugs_dulaglutide.R:23-24`: the Geiser 2016 Table 6 values "are the values of the FDA review's Table 5 that the
  specification cited".
- `R/drugs_dulaglutide.R:29-35`: F 0.47 used for every dose; "the phase 3 table gives F for 1.5 mg only".
- `inst/help/drugs/dulaglutide.md:3`: parameter values, true scale "because an absolute bioavailability study fixed
  the bioavailability"; "These are the values the FDA's review tabulates".
- `inst/help/drugs/dulaglutide.md:9`: "the final model gives it for 1.5 mg only".
- `inst/help/drugs/dulaglutide.md:13`: "The FDA's model of fasting glucose and HbA1c over months is not drawn."
- `inst/help/models/antidiabetics.md:41` (context gave 39; the text is on 41 at 8baa733): HbA1c and fasting glucose
  for "tirzepatide and dulaglutide (FDA disease models)".
- Also, not in the context file: `inst/extdata/antidiabeticRegistry.csv:9` and `data-raw/antidiabeticRegistry.R:81-83`
  ("the FDA review's Table 5 values"; PD end point "FPG/HbA1c (FDA)").

## Verification against stanpumpR
- **Parameter values** (`R/drugs_dulaglutide.R:19-21, 58-60, 78-81`; help line 3): ka 0.00769 /h, CL 0.0593 L/h,
  Q 0.0201 L/h, Vc 2.25 L, Vp 3.75 L, F 0.470 x exp(-0.00877 x (WT - 92.5)) are exactly the final-model column of this
  review's Table 63 (PDF 104). Unit conversion in the code (`/ 60` for L/h to L/min and 1/h to 1/min) is correct.
  Apparent clearance 0.126 L/h at 92.5 kg is right (derived). **matches**.
- **"the FDA review's Table 5"** (`R/drugs_dulaglutide.R:23`; `antidiabeticRegistry.csv:9`;
  `data-raw/antidiabeticRegistry.R:83`): in this review Table 5 (PDF 23) is dose-dependent GI adverse events, and
  Table 4 is GBCF dose finding. The PK table is Table 63 (PDF 104; listed as Table 64 in the list of tables).
  **discrepancy** (table number), unless "the specification" meant some other FDA document, which the repo does not
  name.
- **"the phase 3 table gives F for 1.5 mg only" / "the final model gives it for 1.5 mg only"**
  (`R/drugs_dulaglutide.R:30-31`; `inst/help/drugs/dulaglutide.md:9`): this review's Table 63 also lists F1 for the
  0.75 mg dose, 0.476 (%SEE 2.33), and with it the model reproduces the review's 28.2 ng/mL Cavg,ss for 0.75 mg at
  120 kg (derived). Geiser 2016 Table 6 itself was not checked here. **discrepancy** as a statement about the source;
  the code's choice is still sound and is, if anything, supported: 0.476 is only 1.3% above 0.470, far from the 11%
  the header attributes to the phase 2 Emax, and the sponsor concluded PK was dose-proportional from 0.75 to 1.5 mg
  (PDF 100).
- **"an absolute bioavailability study fixed the bioavailability"** (help line 3; code line 17): the review says the
  1.5 mg F was fixed to 47%, the final GBDR estimate (PDF 103). **matches**. Note the NCA absolute F in GBDR was 0.443,
  and Table 63 labels F1 "relative".
- **Weight acting on F because it had similar effects on CL and V** (code lines 24-26; help line 3): the review says
  only that baseline weight on F was the sole retained covariate (PDF 103); the rationale is Geiser's, not stated here.
  **cannot verify** from this document.
- **"the FDA's fasting glucose and HbA1c disease model"** (code line 5; help line 13; `antidiabetics.md:41`; registry
  PD end point "FPG/HbA1c (FDA)"): the review describes an FPG-HbA1c PK/PD model developed by the sponsor (4.2.3,
  "Results of Sponsor's Analysis", PDF 100-102), with no structure or parameters printed; the FDA reviewer's own
  analysis was of heart rate in GBCJ only (PDF 108-109). It is the sponsor's model as reported by the FDA.
  **discrepancy** (attribution wording only; nothing is plotted, so no numeric consequence).
- **Help line 5 checks** (12.9 mg.h/L, 92 ng/mL, 58 h vs 14.0, 114, 48 h): taken from Geiser 2016 Table 4, not from
  this review; the review's own single-dose tmax (median 48 h, GBDR and GBDT) agrees with the 48 h quoted. **cannot
  verify** the 14.0 and 114 here.
