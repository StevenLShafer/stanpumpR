---
file: DOI_10.1007_bf03257449.pdf
pmid: none
doi: 10.1007/BF03257449
citation: "de Groot ANJA, Vree TB, Hekster YA, van den Biggelaar-Martea M, van Dongen PWJM, van Roosmalen J. Variation in bioavailability of oral methylergometrine in healthy male volunteers. Drug Investigation 1994;8(6):345-351."
kind: pk-model
drugs: [methylergonovine]
source_read: "page images (PDF pages 2, 4, 5 rendered and read: Tables I-IV, Fig 1, Table II; remaining pages text layer)"
edition: published
digest_by: Claude (unattended digest_run.py), 2026-11-26
checked_against: stanpumpR master 8e491545
verification: not-applicable
open_questions:
  - "[check] The abstract (PDF p1) gives CL 34.1 ± 9.7 L/h after IV, but Table II (PDF p4, printed 348) and the Results text (p5) give 32.2 ± 11.80 L/h, and the mean of the six Table III values is 32.2. Which is printed in error cannot be settled from this PDF; use the table value."
  - "[check] Results text (p4) says subject 1 Cmax was 1.2 ug/L at 59 min, but Table IV gives Cmax 1.00 ug/L and tmax 0.99 h for subject 1; Fig 1 caption is for the same subject. Source of the 1.2 is unclear."
  - "[decision] The repo has no methylergonovine model on master 8e491545 (no R/drugs_methylergonovine.R, no drugDefaults row); papers/pending-work.csv lines 79-81 say one is being built from FDA labels. If this paper is to be cited, the V1 is not reported (only Vss 71.5 L, CL 32.2 L/h, t1/2alpha 0.19 h, t1/2beta 1.85 h as means), so a 2-compartment model needs a decision on how to close it; per-subject weights are in Table I, so per-kg scaling is possible but no per-kg values are given."
---

# de Groot 1994: oral methylergometrine bioavailability in men

## Study
Six healthy men (Table I, PDF p2 / printed 346): age 24, 53, 27, 49, 26, 35 y; weight 75, 79, 79, 73, 72, 70 kg; height 173-194 cm; normal liver and renal function. Randomised crossover, 2 weeks apart: oral 0.125 mg methylergometrine maleate tablet (Methergin, = 0.095 mg base) after a standard breakfast, and IV 0.200 mg maleate (= 0.152 mg base) over 1 min after a similar breakfast. Venous plasma (lithium heparin), samples at 0, 10, 20, 30, 60, 90, 180, 270, 360, 450, 540 min, plus 3 and 5 min after IV. Assay: HPLC with fluorescence detection (ex 315 / em 430 nm); quantitation limit 150 ng/L in plasma (120 ng/L in water); intraday CV 3%, interday 5%. Software MW\Pharm: 2-compartment after IV, 1-compartment with lag time after oral. IV solution content measured at 93.3 ± 6.5% of claimed (n=20); the dose used in the analysis is the nominal base dose.

## Model and parameters
Table II (PDF p4 / printed 348), mean ± SD, all doses as free base; concentrations are plasma (total, no protein-binding data).

```yaml
parameters:
  - {name: CL_IV, value: 32.2, unit: L/h, source: "Table II", note: "mean, SD 11.80, absolute (not per kg); abstract says 34.1 ± 9.7"}
  - {name: Vss_IV, value: 71.5, unit: L, source: "Table II", note: "mean, SD 25.9, absolute; model-independent (AUMC)"}
  - {name: t_half_alpha_IV, value: 0.19, unit: h, source: "Table II", note: "mean, SD 0.27, range 0.04-0.74 per Table III; 142% CV"}
  - {name: t_half_beta_IV, value: 1.85, unit: h, source: "Table II", note: "mean, SD 0.28"}
  - {name: MRT_IV, value: 2.25, unit: h, source: "Table II", note: "mean, SD 0.44"}
  - {name: AUC_IV, value: 4.82, unit: "ug/L*h", source: "Table II", note: "mean, SD 1.82, for 152 ug base"}
  - {name: F_oral, value: 84.9, unit: "%", source: "Table II", note: "mean, SD 37.2, 43.8% CV; individual range 22.2-136.0 (Table IV)"}
  - {name: tlag_oral, value: 0.33, unit: h, source: "Table II", note: "mean, SD 0.09, range 0.24-0.50"}
  - {name: t_half_abs_oral, value: 0.08, unit: h, source: "Table II", note: "mean, SD 0.08, range 0.010-0.23"}
  - {name: t_half_beta_oral, value: 2.08, unit: h, source: "Table II", note: "mean, SD 0.43, range 1.62-2.69"}
  - {name: Cmax_oral, value: 0.77, unit: "ug/L", source: "Table II", note: "mean, SD 0.34, for 95 ug base"}
  - {name: tmax_oral, value: 0.68, unit: h, source: "Table II", note: "mean, SD 0.22"}
  - {name: MAT_oral, value: 0.87, unit: h, source: "Table II", note: "mean, SD 0.72, model-independent"}
  - {name: CL_oral, value: 31.1, unit: L/h, source: "Table II", note: "mean, SD 10.3, calculated assuming CL equal after IV and oral (F derived)"}
  - {name: Vss_oral, value: 94.4, unit: L, source: "Table II", note: "mean, SD 38.9"}
```

Individual IV values (Table III, PDF p5 / printed 349), subjects 1-6: t1/2alpha 0.076, 0.74, 0.04, 0.09, 0.14, 0.041 h; t1/2beta 1.84, 1.96, 1.63, 2.31, 1.52, 1.88 h; CL 30.4, 17.65, 49.7, 33.9, 21.5, 39.8 L/h; Vss 69.9, 44.8, 92.7, 99.1, 36.5, 86.1 L. Individual oral values are in Table IV (F 103.6, 22.2, 81.9, 80.0, 85.4, 136 %). Not reported: V1, V2, rate constants k10/k12/k21 or intercepts; clearance or volume per kg. Population approach not used (individual fits, then means). The IV vs PO t1/2beta, CL and Vss differences are not significant (p 0.31, 0.50, 0.11).

## Pharmacodynamics and end points
None. No effect measure (uterine pressure, blood pressure change) was modelled; blood pressure did not change after either route. No ke0, t1/2ke0, tPeak, EC50, MEAC or effective concentration, and no potency ratio. Not an opioid. Side effects after IV: nausea in 1 subject, a "weird" sensation in 3. Pharmacodynamics in non-pregnant women is announced as future work.

## Covariates
None analysed. Only men; weights in Table I only. No pregnancy or sex comparison here (a companion women's study is cited as in preparation).

## Caveats
n = 6 healthy men, none pregnant or postpartum (the clinical target). Very large IIV in oral F (22-136%, with F above 100% in one subject, attributed to assuming equal CL for IV and oral) and in CL (17.7-49.7 L/h) and Vss (36.5-99.1 L). The distribution phase is very fast (t1/2alpha 0.04-0.74 h) with sampling at 3 and 5 min only early on, so the alpha phase and V1 are poorly determined; alpha was not detected after oral dosing. Plasma assay quantitation limit 150 ng/L. Oral doses taken after breakfast (food effect not tested). The abstract and Table II disagree on CL (34.1 vs 32.2 L/h); the oral sampling-time sentence reads "after ergometrine administration" (apparent slip). The authors state that earlier reports of rapid, complete oral absorption were not confirmed.

## Used by stanpumpR
The context file lists no repo locations, `cited_in_repo` is false, and no drug or model uses it.

## Verification against stanpumpR
No R/drugs_methylergonovine.R, drugDefaults_global.csv row or help text mentioning methylergonovine/methylergometrine/ergometrine exists in master 8e491545 (searched whole checkout by name and keyword). Only papers/pending-work.csv lines 79-81 and docs/literature-search-2026-11-16.md line 623 mention it: they list this paper as wanted for V1/Vss, clearance per kg and weights, and oral ka/lag/F. Findings relevant to that request: Vss 71.5 L (the unverified figure mentioned there is confirmed, Table II), no V1 reported, CL and Vss are absolute (L/h, L) with subject weights 70-79 kg in Table I (mean of six weights 74.7 kg, derived), oral F 84.9 ± 37.2% with a 22-136% range, lag 0.33 h, t1/2abs 0.08 h. Verdict: not-applicable (nothing in the repo to compare), with the open questions above.
