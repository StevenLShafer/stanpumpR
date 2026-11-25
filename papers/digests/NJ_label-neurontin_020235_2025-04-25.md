---
file: NJ_label-neurontin_020235_2025-04-25.pdf
pmid: none
doi: none
citation: NEURONTIN (gabapentin) capsules, tablets and oral solution, US prescribing information, NDA 020235 (with 020882, 021129), revised 4/2025; FDA label file dated 2025-04-25 (https://www.accessdata.fda.gov/drugsatfda_docs/label/2025/020235s079,020882s060,021129s059lbl.pdf). Reference ID 5578563 (Viatris).
kind: label
drugs: [gabapentin]
source_read: text layer, partial (Highlights/TOC; sections 2.1-2.5, 8.4-8.6, 9-11, 12.1, 12.3 including Specific Populations and drug interactions, 14.2). The Cockcroft-Gault equation in 2.3 is an image not in the text layer.
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
revised: 2026-10-27 (added the adjunctive partial-seizure responder rates and response ratios by dose from 14.2, and the label's statement that within-study response was not consistently dose-ordered)
---

# Neurontin label 2025: gabapentin dosing and pharmacokinetics

## Study
US prescribing information. Capsules 100/300/400 mg, tablets 600/800 mg, solution 50 mg/mL; MW 171.24 (11).

Dosing (2.1-2.4): postherpetic neuralgia 300 mg day 1, 600 mg/day day 2, 900 mg/day day 3, titrate to
1800 mg/day (600 mg tid); 1800-3600 mg/day studied, no added benefit above 1800. Epilepsy ≥12 y: start 300 mg
tid, maintenance 300-600 mg tid, up to 2400 mg/day long term (3600 mg/day briefly); max 12 h between doses.
Children 3-11 y: start 10-15 mg/kg/day; maintenance 40 mg/kg/day (3-4 y), 25-35 mg/kg/day (5-11 y).
Renal adjustment by CrCL (Table 1, ≥12 y): ≥60 mL/min 900-3600 mg/day; >30-59: 400-1400; >15-29: 200-700;
15: 100-300 mg/day; <15 reduce in proportion to CrCL; hemodialysis supplemental 125-350 mg after each 4 h of
dialysis. CrCL by Cockcroft-Gault (2.3). Elderly: adjust by CrCL (2.4).

## Model and parameters
No compartmental model. Section 12.3:

| Quantity | Value | Source |
|---|---|---|
| Oral bioavailability (not dose-proportional) | ~60%, 47%, 34%, 33%, 27% at 900, 1200, 2400, 3600, 4800 mg/day in 3 divided doses | 12.3 Oral Bioavailability |
| Food | +14% AUC and Cmax | 12.3 |
| Protein binding | <3% | 12.3 Distribution |
| V after 150 mg IV | 58 ± 6 L (mean ± SD) | 12.3 Distribution |
| CSF/plasma (steady-state Cmin, epilepsy) | ~20% | 12.3 |
| Elimination | renal, unchanged; not appreciably metabolised | 12.3 |
| t½ | 5-7 h, unaltered by dose or multiple dosing | 12.3 |
| Clearance, renal clearance, k | directly proportional to CrCL | 12.3 |
| Age (20-80 y) | CL/F ~225 mL/min (<30 y) to ~125 mL/min (>70 y) | 12.3 Age |
| Renal impairment (n=60, single 400 mg, CrCL 13-114) | t½ ~6.5 h (CrCL >60) to 52 h (CrCL <30); CLr ~90 to ~10 mL/min; CL/F ~190 to 20 mL/min | 12.3 |
| Anuric hemodialysis (n=11) | t½ ~132 h off dialysis, 3.8 h on dialysis | 12.3 |
| Pediatric (n=48, 1 mo-12 y, ~10 mg/kg) | peak 2-3 h; t½ mean 4.7 h; <5 y ~30% lower AUC | 12.3 Pediatric |
| Cmax at 3600 mg/day | ~171/15 = 11.4 mcg/mL (derived from the CYP statement "approximately 15 times the Cmax") | 12.3 Drug Interactions |

Not in this edition: an adult steady-state table (Cmax, tmax, AUC at 300 mg q8h etc.), an adult tmax,
and any therapeutic range.

```yaml
parameters:
  - {name: V_IV, value: 58, unit: L, source: "12.3", note: "mean ± 6 SD after 150 mg IV"}
  - {name: F_900mg_day, value: 0.60, unit: fraction, source: "12.3", note: "300 mg tid"}
  - {name: F_1200mg_day, value: 0.47, unit: fraction, source: "12.3"}
  - {name: F_2400mg_day, value: 0.34, unit: fraction, source: "12.3"}
  - {name: F_3600mg_day, value: 0.33, unit: fraction, source: "12.3"}
  - {name: F_4800mg_day, value: 0.27, unit: fraction, source: "12.3"}
  - {name: t_half, value: "5-7", unit: h, source: "12.3", note: normal renal function}
  - {name: t_half_CrCL_lt30, value: 52, unit: h, source: "12.3", note: mean, single 400 mg}
  - {name: CL_F_young, value: 225, unit: mL/min, source: "12.3 Age", note: "<30 y, approximate"}
  - {name: responder_rate_1200mg_day_study1, value: 23, unit: "%", source: "14.2, PDF p.29", note: "14/61; placebo 9% (6/66); significant"}
  - {name: responder_rate_600mg_day_study2, value: 17, unit: "%", source: "14.2, PDF p.29", note: "N=53; placebo 8%; not significant"}
  - {name: responder_rate_1200mg_day_study2, value: 16, unit: "%", source: "14.2, PDF p.29", note: "N=101; placebo 8% (N=98); not significant"}
  - {name: responder_rate_1800mg_day_study2, value: 26, unit: "%", source: "14.2, PDF p.29", note: "N=54; placebo 8%; significant"}
  - {name: responder_rate_900mg_day_study3, value: 22, unit: "%", source: "14.2, PDF p.29", note: "N=111; placebo 10% (N=109); significant"}
```

## Pharmacodynamics and end points
No plasma concentration-response model, no therapeutic concentration range, no effect site. CSF ~20% of plasma
at trough.

Clinical dose-response, adjunctive therapy of refractory partial seizures (14.2, PDF pp.28-29; printed pp.28-29):
three placebo-controlled trials in 705 patients aged 12 y and over, drug added for 12 weeks after a 12-week
baseline, doses in three divided doses. Responder rate = % of patients with a 50% or greater fall in seizure
frequency; response ratio = (T - B)/(T + B), -0.33 being a 50% fall; intent-to-treat, all partial seizures.
- Study 1: 1200 mg/day 23% (14/61) vs placebo 9% (6/66), significant; response ratio -0.199 vs -0.044, significant.
- Study 2: 1200 mg/day (primary, N=101) 16% vs placebo (N=98) 8%, not significant (response ratio -0.103 vs
  -0.022, p = 0.224); 600 mg/day (N=53) 17%, not significant; 1800 mg/day (N=54) 26%, significant (response
  ratio -0.222, significant; 600 mg/day -0.105).
- Study 3: 900 mg/day (N=111) 22% vs placebo (N=109) 10%, significant; response ratio -0.119 vs -0.027,
  significant; 1200 mg/day (N=52) response ratio -0.184, significant vs placebo (its responder rate not printed).
- The label states that within each multi-dose study the response did not rise consistently with dose, while
  across studies efficacy trends upward with dose (Figure 4, responder rate minus placebo by dose and study;
  values not transcribed). These are dose-response observations, not a concentration-response relation.
- Pediatric (PDF p.30): 3-12 y, 25-35 mg/kg/day (N=118) vs placebo (N=127): response ratio -0.146 vs -0.079,
  significant; responder rate 21% vs 18%, not significant. 1 month-3 y, 40 mg/kg/day (N=38 each): no significant
  difference on either measure.

Not an opioid; no
MEAC/ke0. Interaction: morphine 60 mg CR 2 h before gabapentin raised gabapentin AUC 44%; hydrocodone raises
gabapentin AUC 14% (12.3 Drug Interactions).

## Covariates
CrCL (proportional, Cockcroft-Gault), age (via renal function), body weight in children; no sex or race
effect expected.

## Caveats
Summary values without study detail; bioavailability values are from multiple-dose regimens (daily totals,
three divided doses), not single-dose sizes.

## Used by stanpumpR
`R/drugs_gabapentin.R:31,33,44,50,62,79,131`; `inst/help/drugs/gabapentin.md:7,8`;
`tests/testthat/test-drugs-gabapentin.R:96,178` — proportional CrCL, 52 h half-life at CrCL <30, IV volume
58 L (re-anchoring), steady-state AUC 24.8 at 300 mg q8h, F 0.60 at 300 mg tid, tmax 2-4 h.

## Verification against stanpumpR
- `R/drugs_gabapentin.R:31` and `gabapentin.md:7`: clearance proportional to CrCL across renal impairment
  (label). Label 12.3: clearance and renal clearance directly proportional to CrCL. **matches**.
- `R/drugs_gabapentin.R:33`, `gabapentin.md:7`, `test-drugs-gabapentin.R:96`: label 52 h at CrCL <30. Label:
  52 h mean (CrCL <30 mL/min, single 400 mg). **matches**.
- `R/drugs_gabapentin.R:50`, `gabapentin.md:8`: IV volume 58 ± 6 L (Neurontin label). **matches** (150 mg IV).
  Derived 58/81 = 0.716 (code "about 72%") consistent.
- `R/drugs_gabapentin.R:62`: F 0.60 "for 300 mg three times daily in the label". Label: ~60% at 900 mg/day in
  3 divided doses. **matches**.
- `R/drugs_gabapentin.R:44` and `test-drugs-gabapentin.R:178`: "24.8 for 300 mg q8h at steady state in the
  label". This 2025 label has no steady-state AUC table and no 24.8 value. **cannot verify / discrepancy of
  source**: the number appears to come from an older Neurontin label edition; the citation should name that
  edition or another source.
- `R/drugs_gabapentin.R:79`: plasma peak at 3.0 h "inside the 2-4 h of the label". This label gives peak 2-3 h
  only for children (1 month-12 years); no adult tmax. **partly matches** (adult 2-4 h not in this edition).
- `R/drugs_gabapentin.R:131`: reference entry "section 12.3". **matches** (the content above is in 12.3),
  but no edition or date is given.
