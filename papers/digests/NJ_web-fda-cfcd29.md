---
file: NJ_web-fda-cfcd29.pdf
pmid: null
doi: null
citation: "FDA Office of Clinical Pharmacology. Clinical Pharmacology Review, NDA 022200 (BYDUREON; Sequences 573, 588, 597) and NDA 209210 (BYDUREON BCISE; Sequence 544), exenatide extended-release injectable suspension 2 mg, pediatric Study BCB114 (D5551C00002). Reviewers S.W. Johnny Lau, Yangbing Li; signed 2021-06-08/09; Reference ID 4808563. https://www.fda.gov/media/152135/download"
kind: review
drugs: [exenatide]
source_read: "text layer, all 21 PDF pages (Executive Summary pp1-2, Question Based Review 2.1-2.3 pp2-6, Labeling Recommendations p7, Pharmacometric Review appendix 4.1.1 population PK and 4.1.2 exposure-response pp8-20, signature page p21); pages 8, 9, 10, 13, 14, 15 rendered (Tables 3-7, 9-10, model equations)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: partly-verified
---

# FDA 2021: Clinical pharmacology review of Bydureon / Bydureon BCise in adolescents (Study BCB114)

Document identity: this is the pediatric (PREA PMR 1860-1) supplement review for both extended-release exenatide
products, not the original BCise approval review. It cites, but does not contain, the BCise-vs-Bydureon comparison
(Clinical Pharmacology review of NDA 209210 dated 2018-12-04, DARRTS 4358137; p4).

## Study
- Study BCB114 (D5551C00002): multicentre, randomised, double-blind, placebo-controlled Phase 3 in adolescents
  (11-17 y) with type 2 diabetes; about 77 randomised 5:2 to Bydureon 2 mg once weekly (EQW) or placebo for 24 weeks,
  then 28-week open-label extension (all on EQW) and 10-week follow-up (pp2-3). Formulation identical to commercial
  Bydureon (p2).
- PK sampling predose Day 1 and weeks 4, 8, 12, 24, 52 (Table 3, p8). Assay: two-site sandwich immunoenzymetric
  (ELISA) assay in K2EDTA plasma, LLOQ 20 pg/mL, ULOQ 500 pg/mL (Table 1, p5).
- Population PK: the sponsor did **not** refit; it evaluated the previously developed adult Phase 3 model
  (>1000 adults; Table 5 n = 1064) externally on the adolescents (NONMEM 7.3.0, FOCE; p9). Data used: 123 of 365
  samples from 47 of 58 subjects (excluded 209 non-steady-state, before week 8 or after week 30; 8 BLQ; 25 with
  antibody titre > 625) (Table 4, p8).
- Demographics (Table 5, p9; median [range]): adolescents n 47, age 15 [11, 17] y, BMI 37 [18, 71], weight
  102 [48, 201] kg, eGFR 108 [68, 150] mL/min/1.73 m2, IBW 59 [34, 83] kg, 53.2% female; adults n 1064, age
  56 [23, 83], weight 90 [47, 181], eGFR 89 [34, 217], IBW 62 [34, 102]. (The prose on p9 gives eGFR 109 and 88,
  Table 5 gives 108 and 89.)

## Model and parameters
Steady-state average-concentration model (not a time-course model), p9 equation, Table 6 (p10):
Css,avg,ij = ((θ1 · (baseline eGFR_i / 81.6)^θ2 + θ3 · (IBW − 64.1) + θ4 · T125,625,ij) · Frel) · exp(η_i);
log-transformed concentrations, additive residual error on the log scale. Frel/θ5 is EQWS (the BCise suspension,
autoinjector) relative to EQW. Table 6 mislabels the eGFR exponent as θ3 and the IBW slope as θ4 in its description
column; the p9 equation gives θ2 and θ3 (source typo).

```yaml
parameters:
  - {name: theta1_Css_avg, value: 161, unit: pg/mL, source: "Table 6 p10", note: "typical Css,avg for 2 mg weekly at eGFR 81.6 and IBW 64.1 kg; RSE 2.8%"}
  - {name: theta2_eGFR_exponent, value: -0.85, unit: none, source: "Table 6 p10", note: "power on eGFR/81.6; RSE 9.2%"}
  - {name: theta3_IBW_shift, value: -1.38, unit: "pg/mL per kg", source: "Table 6 p10", note: "additive x (IBW - 64.1); RSE 14.8%"}
  - {name: theta4_antibody_shift, value: 39, unit: pg/mL, source: "Table 6 p10", note: "additive when titre >= 125; RSE 11.1%"}
  - {name: theta5_Frel_EQWS, value: 1.01, unit: ratio, source: "Table 6 p10", note: "BCise (EQWS, autoinjector) vs Bydureon EQW, proportional on Css,avg; RSE 3.8%"}
  - {name: omega2_Css_avg, value: 0.285, unit: variance, source: "Table 6 p10", note: "IIV on Css,avg; RSE 7.5%"}
  - {name: sigma2, value: 0.284, unit: "variance (log scale)", source: "Table 6 p10", note: "residual, Tandem assay; RSE 4.6%"}
  - {name: Css_avg_adolescent_median, value: 149.8, unit: pg/mL, source: "Table 7 p13", note: "model-predicted; IQR 121.7-215, 5th-95th 75.9-359"}
  - {name: Css_avg_adult_median, value: 172.1, unit: pg/mL, source: "Table 7 p13", note: "model-predicted; IQR 121.7-236.3, 5th-95th 74.3-414.3"}
  - {name: LLOQ, value: 20, unit: pg/mL, source: "Table 1 p5", note: "exenatide ELISA"}
```

No clearance, volume, absorption rate, release-phase constants or half-life are given anywhere in the document.

## Pharmacodynamics and end points
Exposure-response Emax model for HbA1c at steady state (p14 equations, Table 10 p15):
HbA1c_ij = baseline HbA1c_i − Emax_i · Css,avg / (EC50_i + Css,avg); EC50 = (θ2 + θ4 · [titre ≥ 125]) · exp(η);
Emax = (θ1 + θ3 · (baseline HbA1c − 8.2)) · exp(η).
- θ1 Emax 1.89 % (HbA1c units) at baseline 8.2% (RSE 6.5%); θ2 EC50 52.1 pg/mL for titre < 125 (RSE 31.3%);
  θ3 0.67 change in Emax per 1% baseline deviation (7.7%); θ4 +33.3 pg/mL EC50 for titre ≥ 125 (33.7%);
  IIV variances Emax 0.039, EC50 2.9; residual 0.0039 (Table 10).
- Data: 70 of 154 HbA1c values from 39 of 47 subjects (Table 9, p14). Predicted steady-state HbA1c median 7.1%
  (5.8-9) adolescents vs 6.9% (5.9-9) adults (p16). Reviewer: change from baseline was smaller in adolescents
  (lower baseline; more titre ≥ 125) (p18).
- Immunogenicity: BLQ samples 17% adolescents vs 9% adults; titre > 625 in 16% vs 7% of samples; BLQ in 72% / 52%
  of titre > 625 samples vs 6% at titre ≤ 625 (Table 8, p13). Proposed label 6.2: maximum titre high (≥ 625) in
  about 63% of pediatric patients; higher titres attenuated HbA1c response (p7).

## Covariates
eGFR (power, −0.85), ideal body weight (additive shift), antibody titre ≥ 125 (additive shift on Css,avg and on
EC50), formulation (Frel 1.01). Css,avg rises with age through falling eGFR (p12). Age itself is not in the model.

## Caveats
- Steady-state, measurable, titre ≤ 625 samples only: the model describes a subset and gives no time course.
- Adolescent model evaluation was external (no re-estimation); few adolescent samples (123).
- Redactions ((b)(4)) remove Table 2 (assay specifications across studies), manufacturing-scale details and parts
  of the proposed label text.
- Typographical inconsistencies: Table 6 parameter labels vs the p9 equation; Table 5 vs p9 prose eGFR medians.

## Used by stanpumpR
- `inst/extdata/antidiabeticRegistry.csv:15` (row `exenatide_er_bcise`, Bydureon BCise, SC): source_url for a
  registry-only row, pk_status and pd_status CONSTRAINT, not implemented. Same row is generated at
  `data-raw/antidiabeticRegistry.R:104-107` (8baa733).

## Verification against stanpumpR
- `antidiabeticRegistry.csv:15` at 8baa733: `PK-CONSTRAINT` / `PD-CONSTRAINT`, pd_endpoint `FPG/HbA1c`, stanpumpr_drug
  NA, structure_verified FALSE, note "MCT vehicle; cannot inherit the microsphere release constants without
  comparability audit."
  - Constraint-only status: **matches**. The review gives only a steady-state average-concentration model and an
    HbA1c Emax model; no release or disposition constants that could drive a curve.
  - pd_endpoint FPG/HbA1c: HbA1c **matches** (Table 10); fasting plasma glucose is not analysed in this document
    (**cannot verify** FPG from it).
  - "MCT vehicle": the review does not describe the BCise vehicle (no mention of medium-chain triglyceride)
    (**cannot verify** from this source).
  - "cannot inherit the microsphere release constants without comparability audit": the document reports
    Frel 1.01 for BCise vs Bydureon on steady-state average concentration and says a 2018 review found the two
    similar in adults, but nothing on release kinetics, so the caution is consistent with, not contradicted by, it.
  - Note for the registry: the linked document is the 2021 pediatric supplement review; the BCise-specific adult
    comparability review it cites (NDA 209210, 2018-12-04) would be the more direct source.
