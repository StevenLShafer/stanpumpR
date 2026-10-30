---
file: NJ_fda-nda211759_211759_clinical-prea.pdf
pmid: none
doi: none
citation: "U.S. FDA, CDER Division of Anesthesiology, Addiction Medicine, and Pain Medicine. Division Director Summary Review for Regulatory Action, NDA 211759/6, VYSCOXA (celecoxib) oral suspension, codaDOSE Inc. CDR Leah H. Crisafi; signed 07/29/2025; Reference ID 5633646. (File named 'clinical'; it is the Division Director summary.)"
kind: review
drugs: [celecoxib]
source_read: "partial: text layer pp. 3, 4, 8, 13, 14, 17 and page images pp. 1, 9, 11, 12, 15 (title page, product quality, clinical pharmacology section 5, pediatrics, labeling); pp. 2, 5-7, 10, 16 (contents, benefit-risk tables, CMC, labeling table continuation) not read"
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
---

# FDA Division Director summary 2025, NDA 211759 (VYSCOXA approval)

## Study
Regulatory summary recommending **approval** (overruling the CDTL, Dr Shibuya, who recommended Complete Response
because of the food effect) for OA, RA, JRA (2 years and older) and AS, single doses up to 200 mg. Submission
09/30/2024, PDUFA goal 07/29/2025. Evidence: Study 915/22 only (54 healthy volunteers enrolled; single 200 mg doses,
three-period crossover), summarized from Dr Qiu's Clinical Pharmacology review. Earlier-formulation studies 083/17 and
087/17 (400 mg) were not considered by Clinical Pharmacology.

## Model and parameters
No PK parameters or model coefficients. Restated results (pp. 3-4, 12-14):
- Suspension vs Celebrex fasting: AUC bioequivalent, Cmax 22% lower.
- Suspension fed vs fasted: AUC0-t +50%, AUC0-inf +35%, Cmax +144%.
- Suspension fed vs Celebrex fasted: AUC0-t +23%, AUC0-inf +14%, Cmax +73%.
- Simulated 200 mg BID x 14 days: fasting AUC0-12 bioequivalent, Cmax 19% lower; fed, AUC0-12 about 25% higher than
  fed Celebrex, Cmax comparable.
- Bioequivalence below 200 mg not required because celecoxib PK is dose-proportional up to 200 mg (p. 12).
- Earlier formulation at 400 mg: higher food effect (Cmax +78%, AUC +55-58%), per the OCP review it summarises.

```yaml
parameters: []
```

## Pharmacodynamics and end points
None. Efficacy rests on AUC bridging to Celebrex (NDA 20998).

## Covariates
None analysed. Labeling: take on an empty stomach (at least 2 h before or 1 h after food); discontinue if not
tolerated fasting (p. 15 labeling table).

## Caveats
Summary document: the PK facts are secondary, taken from the OCP review. Pediatric: full waiver for OA, RA, AS;
partial waiver below 2 years (p. 15).

## Used by stanpumpR
The context lists the same celecoxib locations as the OCP review (R/drugs_celecoxib.R 10-152, celecoxib.md:3,
test-drugs-celecoxib.R, docs/nsaid-models.md:17, 37-39). The repo's specific citations (Figure 1, Table 1, signed
6/24/2025) are to the OCP review, not to this document.

## Verification against stanpumpR
- Figure 1 and Table 1 are **not in this document**: cannot verify here (see the OCP review digest).
- Not-modelled facts (R/drugs_celecoxib.R:65-67; celecoxib.md:30-32): suspension Cmax 22% lower at equal AUC; food
  raises Cmax 144% and AUC 35-50%: **match** pp. 3, 13-14.
- "Exposure less than proportional above 200 mg" (R:66-67): consistent with p. 12 (dose-proportional up to 200 mg).
- docs/nsaid-models.md:37 "Neither FDA review publishes the applicant's two-compartment coefficients": true of this
  summary (**matches**).
