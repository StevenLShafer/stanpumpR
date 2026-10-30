---
file: NJ_web-page-meeting-70bcef.pdf
pmid: null
doi: null
citation: "Chae DW, Son M, Kim Y, Son H, Holford N, Park K. Mechanistic modeling of telmisartan blood pressure lowering effect in human. Poster, PAGE meeting 2015 (PAGE2015_POSTER_DW, page-meeting.org pdf_assets 8389); archived 2026-10-21."
kind: pkpd-model
drugs: [telmisartan]
source_read: "page images: the single-page poster rendered at scale 2 and read in four crops (objectives/methods, equations 8-16, Table 1, conclusions); the text layer scrambles the equations. The meeting and year come from the file URL only; the poster face shows neither."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Chae 2015 (PAGE poster): mechanistic telmisartan BP model

## Study
Secondary analysis of a randomized, open-label, multiple-dose 2x2 crossover rosuvastatin-telmisartan interaction study
in healthy volunteers (Severance Hospital, Seoul, 2012). Rosuvastatin 20 mg and telmisartan 80 mg alone and together,
once daily for 6 days; intensive blood sampling on the last dosing day; systolic and diastolic BP and heart rate by
digital cuff, recorded as safety measures. 20 subjects who completed telmisartan monotherapy were modelled. NONMEM 7.3,
PsN 4.2. Age, weight and sex of the subjects not given. Assay not described.

## Model and parameters
PK: two-compartment, first-order absorption with lag time; weight on CL as (WT/70)^0.75 and on V as WT/70 (fixed
allometry). No other covariate significant. **No PK parameter values are printed** (TVCL, TVV2, Q, V3, ka, lag absent).

Link (Eq 3): dCb/dt = ke0 (Cf - Cb), Cf and Cb the free and receptor-bound telmisartan concentrations; ke0 described as
an empirical receptor-interaction constant.

PD: MAP = CO x TPR (Eq 4), CO = SV x HR, C = SV/PP, so MAP = CTPR x PP x HR (Eq 7, CTPR = C x TPR). PP turnover with Imax
inhibition of kin by Cb ("slow" natriuretic effect, Eq 9); CTPR turnover with Imax inhibition by Cb ("rapid" AT1 effect via
cytosolic calcium, Eq 13); a two-harmonic circadian term (12 h and 24 h) on PP and HR (Eqs 10-11) and its mirror on CTPR
with a multiplier MULT (Eq 14); HR feedback driven by the fractional change in MAP with constant Ki (Eqs 15-16).
Sequential fitting (PP and HR, then CTPR, then all together). Table 1 (final PD model, population estimate with %RSE):

```yaml
parameters:
  - {name: CTPR0, value: 0.030, unit: "mmHg/(mmHg x beats/min)", source: "Table 1", note: "baseline; RSE 3.21%; unit not printed, implied by Eq 8"}
  - {name: HR0, value: 67.7, unit: /min, source: "Table 1", note: "RSE 2.84%"}
  - {name: PP0, value: 47.74, unit: mmHg, source: "Table 1", note: "RSE 1.99%"}
  - {name: IC50_CTPR, value: 1110, unit: ng/mL, source: "Table 1", note: "RSE 49.74%; against Cb"}
  - {name: IC50_PP, value: 3238, unit: ng/mL, source: "Table 1", note: "RSE 144.1%; against Cb"}
  - {name: ke0, value: 0.12, unit: 1/h, source: "Table 1", note: "FIXED, not estimated"}
  - {name: kout_PP, value: 0.005, unit: 1/h, source: "Table 1", note: "RSE 166.8%"}
  - {name: kout_CTPR, value: 0.127, unit: 1/h, source: "Table 1", note: "RSE 30.66%"}
  - {name: AMP1_24h, value: 0.059, unit: fraction, source: "Table 1", note: "RSE 17.61%"}
  - {name: PHASE1_24h, value: 14.38, unit: h, source: "Table 1", note: "RSE 7.12%"}
  - {name: AMP2_12h, value: 0.019, unit: fraction, source: "Table 1", note: "RSE 39.11%"}
  - {name: PHASE2_12h, value: 9.54, unit: h, source: "Table 1", note: "RSE 9.06%"}
  - {name: admission_effect, value: 0.035, unit: not stated, source: "Table 1", note: "RSE 67.26%; not defined on the poster"}
```
Variance (Table 1): CV of CTPR0 13.19% (RSE 20.56%), HR0 12.62% (14.34%), PP0 7.30% (25.78%), PHASE1 or 2 12.52% (34.28%);
correlations CTPR0-HR0 -0.77, CTPR0-PP0 -0.47, HR0-PP0 -0.02. Residual CV: HR 7.25%, PP 15.88%, CTPR 6.70%.
Not printed: kin values, MULT, Ki, MAP0. Figure 1 (PK VPC) and Figure 2 (PD VPC) only.

## Pharmacodynamics and end points
End points are cuff MAP (as CTPR x PP x HR), pulse pressure and heart rate in normotensive healthy volunteers, not a
hypertensive BP response. ke0 0.12 /h was fixed (t1/2ke0 5.8 h, derived: ln2/0.12), so it carries no estimated
information. IC50s are against the bound-concentration state Cb; whether Cf is free or total plasma telmisartan in
practice is not stated (the label gives >99.5% binding).

## Covariates
Weight by fixed allometry on PK only; none in the PD model.

## Caveats
A poster: no PK estimates, no subject demographics, no definition of the admission effect, several parameters
unreported. One dose level (80 mg), 20 healthy volunteers, BP measured only as a safety biomarker. PP turnover terms
very imprecise (RSE 144% and 167%). Sequential fixing of earlier-step fixed effects at their EBEs.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:118` (`data-raw/antihypertensiveRegistry.R:967-973`):
  `telmisartan__tablet__oral__healthy_volunteers__pulse_pressure_resistance__page2015`, `primary_fitted_partial` for PK
  and PD, `requires_source_extraction`, BP simulation off, source type conference poster.
- `inst/extdata/antihypertensiveParameters.csv:160` (IC50 pulse pressure 3238 ng/mL, fitted, RSE 144%) and `:161`
  (ke0 0.12 /h, fixed) (`data-raw/antihypertensiveRegistry.R:1842-1856`). The context file's lines 143-144 are stale;
  at 8baa733 the rows are 160-161.

## Verification against stanpumpR
- Structural output the product of resistance, pulse pressure and heart rate (Eq 7): **matches**. "Not a hypertensive
  BP surface": **matches** (healthy volunteers, MAP).
- ke0 FIXED at 0.12 /h: **matches** (Table 1, "0.12 fix", rendered).
- IC50_PP 3238 ng/mL, RSE 144%: **matches** (Table 1: 3,238, 144.1%). Note's resistance IC50 1110 ng/mL: **matches**.
- "144% and 167% relative standard error" for the PP parameters: **matches** (IC50_PP 144.1%, kout_PP 166.8%).
- "Bioavailability dose-dependent so F must not be held constant": not from this poster (it comes from the label,
  `NJ_dailymed-3bb7bc69.md`); consistent with it. **cannot verify** from this source, no conflict.
- `requires_source_extraction` and partial evidence: consistent with the poster printing no PK values, kin, MULT or Ki.
