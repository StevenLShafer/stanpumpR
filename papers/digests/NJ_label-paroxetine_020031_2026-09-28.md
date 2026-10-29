---
file: NJ_label-paroxetine_020031_2026-09-28.pdf
pmid: none
doi: none
citation: "PAXIL (paroxetine) tablets and oral suspension US prescribing information, Apotex (NDA 020031, supplement s081), Drugs@FDA label dated 2026-09-28 (\"Revised: 9/2026\"). https://www.accessdata.fda.gov/drugsatfda_docs/label/2026/020031s081lbl.pdf"
kind: label
drugs: [paroxetine]
source_read: text layer; read sections 2 (2.2-2.8, Table 1), 8.5-8.7, 12.3 (absorption, food, distribution, elimination, specific populations); Figures 1-3 are images and were not read
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: discrepancy
---

# Paxil (paroxetine) label (2026)

## Study
Not a study: US label for immediate-release paroxetine tablets (10-40 mg) and suspension. Main PK study
cited: 15 healthy men given 30 mg tablets daily for 30 days.

## Model and parameters
- Nonlinear PK with dose and duration, attributed to saturable CYP2D6 (12.3).
- Absorption: complete from the hydrochloride solution. Tablets and suspension similar exposure.
- Steady state (30 mg/day, n = 15 healthy men): reached by about 10 days in most subjects, substantially
  longer in some.
- At steady state: Cmax 61.7 ng/mL (CV 45%), Tmax 5.2 h (CV 10%), Cmin 30.7 ng/mL (CV 67%), half-life 21 h
  (CV 32%) (12.3, means). Steady-state Cmax and Cmin about 6 and 14 times, and AUC0-24 about 8 times, the
  single-dose predictions.
- Food (single dose): AUC +6%, Cmax +29%, Tmax from 6.4 h to 4.9 h.
- Distribution: only 1% remains in plasma; 95% / 93% bound at 100 / 400 ng/mL; clinical concentrations
  normally below 400 ng/mL.
- Elimination: mean half-life about 21 h (CV 32%) after 30 mg/day for 30 days. 40 mg/day Cmin only 2-3 times
  more than double the 20 mg/day Cmin. Metabolites have at most 1/50 the parent's SERT potency. Urine 64%
  (2% parent), faeces 36% over 10 days. Not studied in CYP2D6 poor metabolisers.
- Dosing (2.3-2.6): MDD start 20 mg, max 50; OCD 20/60; PD 10/60; PTSD 20/50; SAD 20/60; GAD 10 then 20, max
  50; elderly, severe renal or hepatic impairment start 10 mg, max 40 mg/day.

```yaml
parameters:
  - {name: t_half_ss, value: 21, unit: h, source: "12.3", note: "mean, CV 32%, 30 mg/day x 30 d, 15 healthy men"}
  - {name: time_to_ss, value: 10, unit: days, source: "12.3", note: "approximate, most subjects; longer in some"}
  - {name: Cmax_ss_30mg, value: 61.7, unit: ng/mL, source: "12.3", note: "mean, CV 45%"}
  - {name: Cmin_ss_30mg, value: 30.7, unit: ng/mL, source: "12.3", note: "mean, CV 67%"}
  - {name: Tmax_ss, value: 5.2, unit: h, source: "12.3", note: "mean, CV 10%"}
  - {name: Tmax_single_fasted, value: 6.4, unit: h, source: "12.3 food effect"}
```

## Pharmacodynamics and end points
No therapeutic concentration range. QTc: mean increase >20 ms unlikely at the maximum dose. Not an opioid.

## Covariates
Age (higher Cmin at >= 65 y), renal impairment (higher concentrations, graded), severe hepatic impairment,
CYP2D6 inhibitors; sex no significant Cmax/AUC difference (meta-analysis of 4 studies, 20-40 mg/day).

## Caveats
Figures 1-3 (interaction and specific-population forest plots) are images, not read. The 21 h half-life was
measured at steady state on 30 mg/day, i.e. in the saturated state, in young healthy men.

## Used by stanpumpR
- R/drugs_paroxetine.R:68 — "The label gives ~21 h."
- R/drugs_paroxetine.R:74 — the model approaches steady state too slowly: 7.5 days to 90% at 71 y, "against
  about 3 by the label".
- inst/help/drugs/paroxetine.md:17 — same two statements.
- inst/help/models/antidepressants.md:23 — paroxetine model's 54 h at 71 y against "the label's 21".

## Verification against stanpumpR
- R/drugs_paroxetine.R:68, paroxetine.md:17 (half-life ~21 h), antidepressants.md:23 (label's 21): label mean
  21 h. **matches**.
- R/drugs_paroxetine.R:73-74 and paroxetine.md:17 "about 3 [days] by the label": the label says steady state
  was reached by about 10 days in most subjects, longer in some, and explains excess accumulation by saturable
  metabolism. "About 3 days" appears to be derived from 21 h (3.3 half-lives = 2.9 days, derived), not stated
  by the label. **discrepancy**: by the label's own figure, the model's 7.5 days to 90% is if anything faster
  than observed, so the claim that the approach is "too slow" against the label is not supported.
- Context for the half-life comparison: the label's 21 h is in young healthy men at steady state on 30 mg/day,
  whereas the model's 54 h is at 71 y and 25 mg/day; the age difference alone accounts for part of the gap
  (the model's own age term gives CL/F 2.4x at 20 y). The "artefact of trough data" argument may still hold,
  but the comparison is not like for like. Noted.
