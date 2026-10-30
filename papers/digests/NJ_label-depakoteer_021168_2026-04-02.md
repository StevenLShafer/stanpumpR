---
file: NJ_label-depakoteer_021168_2026-04-02.pdf
pmid: none
doi: none
citation: "DEPAKOTE ER (divalproex sodium) extended-release tablets, US prescribing information, AbbVie; NDA 021168 (s052), revised 3/2026 (FDA label dated 2026-04-02). https://www.accessdata.fda.gov/drugsatfda_docs/label/2026/021168s052lbl.pdf"
kind: label
drugs: [valproate]
source_read: text layer; read Highlights, contents, 2.1-2.5 (dosing, 2.4 Conversion from Depakote with Table 1), 12.2 Pharmacodynamics, 12.3 Pharmacokinetics (Table 8, distribution, metabolism, elimination, specific populations)
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# Depakote ER label 2026: divalproex extended-release prescribing information

## Study
Regulatory label. Product: divalproex sodium extended-release tablets 250 and 500 mg, each strength stated as
"equivalent to" that many mg of valproic acid (Section 11/16). Once-daily. Summaries of bioavailability studies:
five multiple-dose studies (healthy n = 82, epilepsy n = 86) for relative F; two randomised crossover conversion
studies (Table 8).

## Model and parameters
No compartmental model. Values (12.3 unless noted):

```yaml
parameters:
  - {name: F_absolute_ER, value: "approximately 0.90", unit: fraction, source: "12.3 Absorption", note: "single dose after a meal, vs IV infusion"}
  - {name: F_relative_ER_vs_DR, value: 0.89, unit: fraction, source: "12.3 Absorption", note: "average, ER once daily vs equal total daily dose of Depakote DR BID/TID/QID; 5 studies, N=82 healthy + 86 epilepsy, fasting and nonfasting"}
  - {name: tmax_ER, value: "4 to 17", unit: h, source: "12.3", note: "range of median tmax"}
  - {name: fluctuation, value: "10-20% lower", source: "12.3", note: "peak-trough fluctuation, ER qd vs DR BID/TID/QID"}
  - {name: conversion, value: "ER dose 8 to 20% higher than total daily DR dose", source: "2.4 and Table 1; 12.3"}
  - {name: Table8_healthy_AUC24_ratio, value: 1.059, source: "Table 8", note: "ER 1000 & 1500 mg vs DR 875 & 1250 mg, N=35; Cmax 0.882, Cmin 1.173"}
  - {name: Table8_epilepsy_AUC24_ratio, value: 1.008, source: "Table 8", note: "ER 1000-5000 mg vs DR 875-4250 mg, on enzyme-inducing AEDs, N=64; Cmax 0.899, Cmin 1.022"}
  - {name: CL_total, value: 0.56, unit: "L/h/1.73 m2", source: "12.3 Elimination", note: "total valproate"}
  - {name: V_total, value: 11, unit: "L/1.73 m2", source: "12.3 Elimination"}
  - {name: CL_free, value: 4.6, unit: "L/h/1.73 m2", source: "12.3 Elimination"}
  - {name: V_free, value: 92, unit: "L/1.73 m2", source: "12.3 Elimination"}
  - {name: t_half, value: "9 to 16", unit: h, source: "12.3 Elimination", note: "monotherapy, oral 250-1000 mg"}
  - {name: free_fraction, value: "approx 10% at 40 mcg/mL to 18.5% at 130 mcg/mL", source: "12.2 and 12.3 Protein binding"}
```

Elderly (68-89 y vs 22-26 y): intrinsic clearance 39% lower, free fraction 44% higher. Sex: BSA-adjusted unbound
clearance 4.8 vs 4.7 L/h/1.73 m2 (men vs women). Cirrhosis: free clearance down 50% (n = 7), acute hepatitis 16%;
t½ 12 to 18 h. Renal failure (CrCl <10): unbound clearance down 27%; haemodialysis lowers levels ~20%. CSF
approximates unbound plasma (~10% of total). Total concentration rises less than proportionally with dose
(saturable binding); unbound kinetics linear. Enzyme inducers raise clearance (no figure).

## Pharmacodynamics and end points
Therapeutic range in epilepsy commonly 50-100 mcg/mL total valproate (12.2; 2.2). Mania trials dosed to trough
85-125 mcg/mL (2.1, 12.2). Thrombocytopenia probability rises at total trough >110 mcg/mL (women) or >135 mcg/mL
(men) (2.2, 5.x). Max dose 60 mg/kg/day. Not an opioid: no MEAC, ke0 or potency ratio.

## Covariates
Age, hepatic and renal disease, inducers; no equations.

## Caveats
Conversion equivalence (Table 8) was shown with ER doses 8-20% higher; Cmin on average equivalent after
conversion but may vary between patients. Patients above 3125 mg/day DR lack a conversion recommendation.

## Used by stanpumpR
- R/drugs_valproate.R:55: ER bioavailability 0.89 relative to DR, "the Depakote ER label's basis for giving
  8-20% more drug on conversion".
- inst/help/drugs/valproate.md:7: ER 0.89 as available, basis for 8-20% more on conversion (label not named).

## Verification against stanpumpR
- **ER relative F 0.89 (valproate.R:52-56, code line 121 `bioavailability_PO = 0.89`)**: label 12.3 gives an
  average 89% for ER vs an equal daily dose of DR. **Matches** (the repo's primary source is Dutta and Zhang 2004;
  the label value is identical).
- **8-20% more drug on conversion (valproate.R:55; valproate.md:7)**: label 2.4 and 12.3. **Matches.**
- **ER ka 0.38/h**: not in the label; label median tmax 4-17 h for ER is compatible with slow absorption.
  **Cannot verify** the ka from the label.
- Context: label absolute F of ER ~90% vs IV is consistent with the code's DR F = 1 and ER 0.89 (absolute
  0.89); label CL 0.56 L/h/1.73 m2 and V 11 L/1.73 m2 (derived for 1.83 m2: ~0.59 L/h, ~11.6 L) are close to
  the model's 0.646 L/h and 14 L at 70 kg; label t½ 9-16 h brackets the model's 15 h. Band 50-100 mcg/mL in
  R/drugs_valproate.R:112 and the CSV row matches the label's epilepsy range. **Matches.**
