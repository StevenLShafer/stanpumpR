---
file: NJ_fda-nda211759_211759_clinpharm-prea.pdf
pmid: none
doi: none
citation: "U.S. FDA, CDER Office of Clinical Pharmacology Review, NDA 211759 resubmission, VYSCOXA (celecoxib) oral suspension 10 mg/mL, codaDose (505(b)(2), listed drug Celebrex NDA 020998). Reviewer Wei Qiu, team leader Deep Kwatra; signed 06/24/2025; Reference ID 5613582."
kind: review
drugs: [celecoxib]
source_read: "text layer (pp. 1-6, 8-13, 18) plus page images: p. 7 (full-page image: study design, sampling times), p. 8 Figure 1 (JPEG, 805 x 375 px, examined enlarged), pp. 10, 11, 13 (Tables 3, 4, 6, 7 as JPEGs), pp. 14-17 (full-page images: Table 8, sections 2.1-3). All 19 pages read."
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: discrepancy
---

# FDA OCP review 2025, NDA 211759: celecoxib suspension vs Celebrex capsule (Study 915/22)

## Study
- Study 915/22 (p. 7, page image): open-label, randomized, single-dose, three-period, six-sequence crossover in healthy
  adult men and women; at least 7 days washout. Clinical site Quinta-Analytica, Prague (section 4.2).
- **54 randomized (25 F / 29 M), 51 completed** (p. 7).
- Treatments: 200 mg suspension fasting (TFA), 200 mg suspension after a high-fat high-calorie meal (TFE),
  200 mg Celebrex capsule fasting (RFA).
- Plasma sampling: pre-dose and **0.33, 0.67, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 6, 8, 10, 12, 14, 24, 36, 48, 72 h**
  (p. 7; 20 post-dose times).
- Assay: LC/MS/MS, LLOQ 10.0 ng/mL, range 10-2000 ng/mL, QC precision 1.97-2.59% CV, accuracy 97.20-98.99% (4.1).
  Total plasma celecoxib.
- Figure 1 (p. 8): mean plasma concentration vs **nominal** time, three treatments, N = 51; y axis 0-800 ng/mL,
  x axis 0-80 h. Source given as study report 915/22 p. 8.

## Model and parameters
No population-model coefficients are published. The applicant's popPK models (fitted separately to RFA, and TFA/TFE)
were **two-compartment with linear elimination** (p. 10); the capsule-fasting model was "customized" with a literature
food correction (AUC +22%) to simulate fed Celebrex (pp. 12, 14). Only exposure summaries of the simulations appear.

Table 1 (p. 8-9), noncompartmental, mean ± SD (%CV); Tmax median (range):

| Parameter | TFA (N = 52) | RFA capsule (N = 51) | TFE (N = 52) |
|---|---|---|---|
| Tmax, h | 1.50 (0.67-8.00) | 2.50 (1.00-8.00) | 3.00 (0.67-6.05) |
| Cmax, ng/mL | 346.2 ± 130.9 (37.81%) | 476.3 ± 287.8 (60.42%) | 825.9 ± 266.6 (32.28%) |
| AUC0-t, ng·h/mL | 4720.8 ± 4291.8 (90.91%) | 5379.0 ± 6148.2 (114.30%) | 6631.0 ± 4993.1 (75.30%) |
| AUC0-inf, ng·h/mL | 5506.8 ± 5511.3 (100.08%) | 6035.8 ± 7146.9 (118.41%) | 6892.7 ± 5395.0 (78.27%) |
| Kel, 1/h | 0.050 ± 0.020 | 0.056 ± 0.020 | 0.119 ± 0.046 |
| t½, h | 16.49 ± 7.06 | 14.43 ± 6.63 | 6.70 ± 2.73 |

Table 2 (GLSM, N = 51 each): AUC0-t 3932.2 vs 4281.5 (ratio 91.84%, 90% CI 86.79-97.19); AUC0-inf 4512.7 vs 4784.3
(94.32%, 89.50-99.40); Cmax 324.2 vs 416.1 ng/mL (77.92%, 71.08-85.41). Table 5 (food, suspension, N = 52):
AUC0-t 149.64% (139.58-160.44), AUC0-inf 134.96% (125.94-144.63), Cmax 244.36% (224.42-266.08); median Tmax delayed
1.5 h.

Simulated 200 mg BID for 14 days (Tables 3, 6, 7; N = 54, 52, 53): e.g. RFA day 15 AUC0-12 arithmetic mean 5820 ng·h/mL
(CV 104.9%), geometric 4790; Cmax 819 (geometric 701) ng/mL; accumulation ratio (AUC) geometric 1.97. TFA day 15
AUC0-12 5810 (geometric 4780). The Cmin rows are labelled µg/mL but the values (e.g. 117, 322) can only be ng/mL
(unit typo in the tables). Table 4 (fasting): AUC0-12 ratio 99.80% (84.86-117.36), Cmax 80.80% (69.52-93.91).
Table 8 (fed): AUC0-12 124.73% (107.43-144.81), Cmax 107.62% (92.93-124.62).

```yaml
parameters:
  - {name: Cmax_capsule_fasting, value: 476.3, unit: ng/mL, source: "Table 1", note: "mean of individual Cmax, 200 mg, N=51"}
  - {name: AUC0-t_capsule_fasting, value: 5379.0, unit: ng*h/mL, source: "Table 1", note: arithmetic mean}
  - {name: AUC0-inf_capsule_fasting, value: 6035.8, unit: ng*h/mL, source: "Table 1", note: arithmetic mean}
  - {name: AUC0-inf_capsule_fasting_GLSM, value: 4784.3, unit: ng*h/mL, source: "Table 2"}
  - {name: t_half_capsule_fasting, value: 14.43, unit: h, source: "Table 1", note: mean of individual values}
  - {name: Tmax_capsule_fasting, value: 2.50, unit: h, source: "Table 1", note: median}
  - {name: structural_model, value: "2-compartment, linear elimination", unit: none, source: "p. 10", note: "no coefficients published"}
```

## Pharmacodynamics and end points
None. Efficacy is bridged to Celebrex by AUC bioequivalence; no ke0, EC50 or analgesic concentration. The reviewer
argues AUC rather than Cmax matters for the chronic indications sought.

## Covariates
None analysed. Labeling text carried over: halve the dose in moderate hepatic impairment (Child-Pugh B) and in
CYP2C9 poor metabolizers (p. 16). The review quotes the Celebrex label half-life as about 11 h (pp. 7, 17).

## Caveats
- Healthy adults, single 200 mg dose; less-than-proportional exposure above 200 mg (poor solubility) not studied here.
- Very large AUC CVs (90-118%) suggest a few subjects with very high exposure (CYP2C9 status not given).
- TFE half-life (6.7 h) is shorter than fasting, a sampling/absorption artefact of NCA rather than a disposition change.
- Fed-Celebrex simulations use a literature correction factor, criticised by the reviewer (p. 14).

## Used by stanpumpR
R/drugs_celecoxib.R (header lines 9-53, reference string 150-157), inst/help/drugs/celecoxib.md:3,
tests/testthat/test-drugs-celecoxib.R:2, 39-59, docs/nsaid-models.md:17, 37-39: Figure 1 (RFA curve) digitized to 19
points and fitted to a two-compartment model with lag; Table 1 AUC, Cmax and half-life used as checks.

## Verification against stanpumpR
- **"Two-compartment, no coefficients published"** (drugs_celecoxib.R:9-13, celecoxib.md:3, nsaid-models.md:37):
  **matches** (p. 10; no coefficient table anywhere in the 19 pages).
- **Figure 1 identity** (R:15-18): 200 mg Celebrex fasting, N = 51, image 805 x 375 px: **matches**.
- **Table 1 numbers** (R:26-37, test:53-59): AUC0-t 5379, mean Cmax 476 at median 2.5 h, t½ 14.4 h, AUC0-inf 6036
  arithmetic and 4784 GLSM: all **match** Tables 1-2.
- **Digitized times** (R:21-24, test:41-45): **discrepancy**. Figure 1 is plotted against nominal time, and the
  nominal schedule is on p. 7. The repo's absorption-phase times (0.5, 1.06, 1.26, 1.47, 1.89, 2.57, 3.24, 4.17, 4.58,
  5.10, 6.04 h) do not fall on that schedule (0.33, 0.67, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 6 h), and one early point
  is missing (19 digitized against 20 sampled; apparently 3.5 h). Matched in order, the rising-limb points sit
  0.2-0.4 h late (142 ng/mL at 0.67 h nominal read as 1.06 h; 257 at 1.0 h read as 1.26 h). An enlarged crop of the
  figure puts those markers at about 0.75 and 1.1 h, i.e. near nominal. The fitted lag, 0.853 h, gives
  0 ng/mL at 0.67 h and about 110 ng/mL at 1.0 h (derived: closed-form 2-compartment with the repo's parameters),
  against mean values of about 142 and 257 at those nominal times. The lag and ka are therefore probably biased late
  and slow; refitting with the nominal times is suggested. Late points (8-72 h) are on schedule.
- **"whose make-up the review does not give"** (R:98-99, sex of the cohort): **discrepancy**. p. 7 gives 54
  randomized, 25 F / 29 M (the 51 completers' split is not given).
- **Not modelled list** (R:65-67, celecoxib.md:30-32): suspension Cmax 22% lower at equal AUC; food +144% Cmax,
  +35-50% AUC: **matches** Tables 2 and 5.
- **docs/nsaid-models.md:37 "Neither FDA review"**: the archive holds this OCP review, its 07/21/2025 addendum, and the
  Division Director summary review; none publishes coefficients, so the statement holds whichever two are meant
  (**matches**, wording ambiguous).
- Observation, not a repo claim: the fit's Vss/F, 237 + 413 = 650 L, and terminal t½ 15.0 h compare with 400 L and
  about 11 h in the label (see the VYSCOXA label digest); the review's own NCA t½ is 14.4 h.
