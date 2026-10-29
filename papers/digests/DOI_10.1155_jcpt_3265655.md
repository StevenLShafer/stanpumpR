---
file: DOI_10.1155_jcpt_3265655.pdf
pmid: none (not in context)
doi: 10.1155/jcpt/3265655
citation: "Ghimire A, Tuey SM, Prebehalla L, Atilano-Roque A, Roda G, Thompson LE, Chonchol MB, Shah N, Wempe MF, Nolin TD, Joy MS. A Population Pharmacokinetic Model to Assess the Cytochrome P450 2B6 Metabolic Probe Bupropion and Hydroxybupropion in Vitamin D-Insufficient and -Replete Patients With Kidney Disease. J Clin Pharm Ther 2026;2026:3265655."
kind: pk-model
drugs: [bupropion, hydroxybupropion]
source_read: "text layer; PDF pp. 6 and 11 (Table 3, Section 3.7) checked on rendered page images"
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
revised: 2026-10-27 (citation given its printed title; added the vitamin D formation-clearance contrasts and the authors' interpretation)
---

# Ghimire 2026: population PK of bupropion SR and hydroxybupropion in CKD and controls

## Study
- NCT02360644; Universities of Colorado and Pittsburgh. Adults 18-70 y with 25-OH vitamin D < 30 ng/mL; CKD (n 13) and controls without kidney disease (n 6). Two phases: vitamin D-insufficient, then after cholecalciferol 5000 IU/day for 12 weeks (replete).
- Single oral **150 mg bupropion SR** in each phase, fasted; plasma pre-dose and 0.5, 1, 2, 4, 8, 12, 24, 48 h; urine to 48 h. LC-MS/MS; LLOQ 0.8 ng/mL (bupropion), 0.9 ng/mL (hydroxybupropion).
- Demographics (Table 2, median (range)): CKD age 61 (29-73) y, weight 99.2 (75-139.5) kg, eGFR 29 (11-48); controls age 52 (28-58) y, weight 100.8 (69.8-131) kg, eGFR 93 (89-118); BMI 34.85 (24.6-48.3) (column unclear); CKD 100% Caucasian; KDIGO G1 5, G2 1, G3 6, G4 6, G5 1.
- PopPK: 171 plasma samples, 19 subjects. Phoenix NLME 8.6.1.6, FOCE; dose entered as 150,000,000 ng (no molar conversion described). Two-step sequential model (parent fixed, then metabolite), since the simultaneous fit was not identifiable. Bootstrap 1000; VPC 200.

## Model and parameters
Two compartments for bupropion (first-order absorption), two for hydroxybupropion; formation CL_F = fm × CL/F with **fm fixed at 0.1** (chosen as ~50% of the 0.05-0.16 published maximum, because most subjects had CKD); non-forming CL_B = (1 − fm) × CL/F. Multiplicative residual error; IIV on bupropion V and CL (and hydroxybupropion central V and CL); IIV values not tabulated. Table 4 (estimate (95% CI); RSE %):

| Parameter | Estimate | RSE % |
|---|---|---|
| V/F (L) | 524.6 (371.2, 678) | 14.7 |
| CL/F (L/h) | 148.4 (115.9, 181) | 10.2 |
| Ka (h⁻¹) | 0.29 (0.26, 0.32) | 5.0 |
| V2/F (L) | 5006.9 (4073.5, 5940.2) | 9.5 |
| CL2/F (L/h) | 257 (197.4, 316.5) | 11.8 |
| Stdev1 (bupropion) | 0.48 | 7.3 |
| VHB1/F (L) | 2.2 (1.7, 2.6) | 11.1 |
| CLHB1/F (L/h) | 0.9 (0.7, 1.0) | 8.8 |
| CLHB2/F (L/h) | 3.1 (2.5, 3.5) | 9.8 |
| VHB2/F (L) | 17.7 (14.2, 20.9) | 9.6 |
| Stdev2 (hydroxybupropion) | 0.38 | 8.1 |
| fm | 0.1 (fixed) | |

```yaml
parameters:
  - {name: V/F, value: 524.6, unit: L, source: "Table 4"}
  - {name: CL/F, value: 148.4, unit: L/h, source: "Table 4"}
  - {name: ka, value: 0.29, unit: 1/h, source: "Table 4", note: "SR tablet"}
  - {name: V2/F, value: 5006.9, unit: L, source: "Table 4"}
  - {name: CL2/F, value: 257, unit: L/h, source: "Table 4"}
  - {name: VHB1/F, value: 2.2, unit: L, source: "Table 4", note: "conditional on fm 0.1"}
  - {name: CLHB1/F, value: 0.9, unit: L/h, source: "Table 4"}
  - {name: CLHB2/F, value: 3.1, unit: L/h, source: "Table 4"}
  - {name: VHB2/F, value: 17.7, unit: L, source: "Table 4"}
  - {name: fm, value: 0.1, unit: fraction, source: "Table 4; Methods 2.7", note: fixed}
  - {name: CLform_OHBUP_CKD_insufficient, value: 2.21, unit: L/h, source: "Table 3 (PDF p. 6)", note: "median, n 8; urinary post hoc NCA"}
  - {name: CLform_OHBUP_CKD_replete, value: 2.65, unit: L/h, source: "Table 3 (PDF p. 6)", note: "median, n 7; p = 0.39 vs insufficient"}
  - {name: CLform_OHBUP_control_insufficient, value: 2.96, unit: L/h, source: "Table 3 (PDF p. 6)", note: "median, n 3"}
  - {name: CLform_OHBUP_control_replete, value: 2.66, unit: L/h, source: "Table 3 (PDF p. 6)", note: "median, n 4; p = 0.85 vs insufficient"}
```
Derived: bupropion half-lives 0.86 h and 38.5 h; hydroxybupropion disposition half-lives 0.35 h and 18.9 h; formed-metabolite/parent AUC ratio fm·CL/F ÷ CLHB/F = 14.84/0.9 = 16.5.

NCA (whole group, mean ± SD, p. 5): CL/F 202.8 ± 89.4 L/h, Cmax 83.2 ± 40.3 ng/mL, t½ 23.1 ± 22.4 h, AUC0-∞ 934.5 ± 487.3 h·ng/mL, Tmax 1.8 ± 0.9 h. Table 3 (NCA medians by group/phase): bupropion Cmax 64-74 ng/mL, Tmax 1-2 h, t½ 17-21 h; hydroxybupropion Cmax 170-289 ng/mL, Tmax 4-8 h, AUC0-∞ 16,325-20,653 h·ng/mL. Discussion: observed bupropion t½ ~20 h; sampling truncated at 48 h.

## Pharmacodynamics and end points
None (CYP2B6 phenotyping study). No ke0, no therapeutic range, no EC50. Not an opioid: no MEAC or potency ratio.

CYP2B6 activity end point (the study's research question; a separate post hoc NCA, not part of the popPK model):
hydroxybupropion formation clearance = amount of hydroxybupropion in urine 0-48 h / bupropion AUC0-48 (Methods, PDF
p. 5), assuming the metabolite formed is mainly excreted in urine. Medians (n), insufficient vs replete (Table 3, PDF
p. 6; Section 3.7, PDF p. 11; abstract, PDF p. 1):
- CKD: 2.21 L/h (n 8) vs 2.65 L/h (n 7), p = 0.39 (a non-significant trend upward with repletion).
- Controls: 2.96 L/h (n 3) vs 2.66 L/h (n 4), p = 0.85 (slight non-significant fall).
- Whole group: 2.31 L/h (n 11) vs 2.65 L/h (n 11), p = 0.51 (Section 3.7).
- Between groups (Discussion, PDF p. 13): controls vs CKD 2.96 vs 2.21 L/h (p = 0.63) insufficient, 2.66 vs 2.65 L/h
  (p > 0.99) replete.
- Interpretation: Section 3.7 says neither group changed significantly, yet the abstract conclusion and Conclusions
  (PDF p. 14) speak of increased CYP2B6 function in CKD after repletion. The data show only a non-significant trend;
  the Discussion (p. 13) also allows that a higher formation clearance may reflect higher renal clearance of the
  metabolite rather than more CYP2B6 metabolism.

## Covariates
Tested: eGFR, CKD vs control, baseline 25-OH vitamin D, weight, BMI, age, sex, race, CYP2B6 rs3745274. **None retained** (forward p < 0.05, backward p < 0.01).
Vitamin D status therefore has no term in the popPK model; its only reported effect is the non-significant urinary formation-clearance contrast above (separate post hoc analysis).

## Caveats
- n = 19, single dose, 48-h sampling vs ~20 h half-life; early concentrations underpredicted; no external validation; IIV incomplete.
- fm fixed at 0.1 (CKD-motivated); hydroxybupropion parameters are conditional on fm and F (explains 2.2 L central volume).
- Cohort heavy (median ~100 kg), older, mostly CKD; model terminal half-life (38.5 h, derived) exceeds the observed ~20 h.
- Dose in mg of bupropion HCl, entered on a mass basis.

## Used by stanpumpR
- `R/drugs_bupropion.R:20-44,72-93,128-153`, `R/drugs_hydroxybupropion.R:15-41,64-74`: all parameters, fm 0.1, structure, citation.
- `inst/help/drugs/bupropion.md:3-13,21,25,31,42`; `inst/help/drugs/hydroxybupropion.md:3-12`; `inst/help/models/antidepressants.md:14`; `inst/help/models/metabolites.md:13`; `inst/help/investigators.md:77`; `tests/testthat/test-drugs-bupropion.R:5,17-20,74,105`.

## Verification against stanpumpR
- Bupropion Vc/F 524.6 L, CL/F 148.4 L/h, Vp/F 5006.9 L, Q/F 257 L/h, ka 0.29 /h (`R/drugs_bupropion.R:140-144`; help table): **matches** Table 4; ÷ 60 conversions correct (test pins 2.4733, 4.2833, 0.0048333 /min).
- Hydroxybupropion 2.2 L, 0.9 L/h, 17.7 L, 3.1 L/h (`R/drugs_hydroxybupropion.R:71-74`): **matches**.
- fm 0.1 fixed, CL_B = (1−fm)CL, CL_F = fm·CL, total CL/F includes formation (`R/drugs_bupropion.R:72-79,145,221`): **matches** Fig. 2/Methods. kFormation = 0.1 × 148.4/524.6 /h: correct.
- mwRatio 1, "source's convention not recovered" (`R/drugs_bupropion.R:85-90`): the paper converts the 150 mg dose to ng and describes no molar conversion, which supports the mass basis assumed. **matches** (can now be stated as consistent with the paper).
- Half-lives 0.86/38.5 h and 0.35/18.9 h; 16.5-fold ratio (header 38-39, 92-93; help 13, 25): recomputed, **matches**. Repo correctly notes the 38.5 h exceeds the usual ~21 h; the paper's own NCA t½ was 17-23 h.
- "19 adults, weighted towards CKD", "single 150 mg SR", "no IV", covariates (CKD, vitamin D) not retained (header 20-36; help 3, 31): **matches**; the paper tested more covariates (weight, BMI, age, sex, race, eGFR, CYP2B6), none retained.
- Body size: "published values taken as the 70 kg reference man's" (`R/drugs_bupropion.R:118-121`; help 31). The cohort's median weight was ~99-101 kg (Table 2). With the fat-free-mass switch on, the typical 70 kg patient therefore receives the parameters of a ~100 kg cohort unscaled. Not a transcription error, but the reference weight assumption is not supported by the paper. **discrepancy (modelling assumption; reference weight ~100 kg in source)**.
- Help 21 single-dose predictions (bupropion peak ~61 ng/mL at 2.2 h; hydroxybupropion ~263 ng/mL at 3.9 h): model outputs; consistent in magnitude with the paper's NCA medians (Cmax 64-74 ng/mL, Tmax 2 h; metabolite 170-289 ng/mL, Tmax 4-8 h). **matches** (comparison).
- Citation string (`R/drugs_bupropion.R:149-153`) is a description, not the article title, by design; no volume/pages, article number 3265655. Cosmetic.
