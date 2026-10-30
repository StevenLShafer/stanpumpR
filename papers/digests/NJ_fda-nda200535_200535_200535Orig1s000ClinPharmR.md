---
file: NJ_fda-nda200535_200535_200535Orig1s000ClinPharmR.pdf
pmid: none
doi: none
citation: "US FDA CDER. Clinical Pharmacology and Biopharmaceutics Review(s), NDA 200535 (oxycodone hydrochloride oral solution 20 mg/mL, Lehigh Valley Technologies; 505(b)(2), reference listed drug ROXICODONE NDA 21-011). Reviewer Wei Qiu, team leader Suresh Doddapaneni; signed 10/12/2010 (filing memo 03/01/2010; ONDQA biopharmaceutics review M. Hughes 08/18/2010). https://www.accessdata.fda.gov/drugsatfda_docs/nda/2010/200535Orig1s000ClinPharmR.pdf"
kind: review
drugs: [oxycodone]
source_read: text layer, whole document (30 pp.; clin pharm review sections 1-2.6, biopharmaceutics review, filing memo). Draft labeling (3 pp.) and the study synopsis pages are withheld as (b)(4).
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# FDA 2010: NDA 200535 clinical pharmacology review, oxycodone oral solution vs tablet

## Study
One pivotal study, UPN-1189: single-dose, five-period crossover in healthy adults (35 enrolled, 25 completed all
periods: 21 men, 4 women; DecisionLine, Toronto). Fasted treatments: LVT oxycodone IR capsules 3 x 5 mg, LVT
oral solution 0.75 mL of 20 mg/mL (15 mg), ROXICODONE 15 mg IR tablet, a redacted fourth arm (an ER reference,
per the filing memo); fifth period capsules 3 x 5 mg after a high-fat meal. Assay: validated LC-MS/MS in plasma,
LLOQ 0.50 ng/mL oxycodone, 0.25 ng/mL noroxycodone, 0.025 ng/mL oxymorphone. Non-compartmental analysis.
Submitted 22 Dec 2009.

## Model and parameters
No compartmental model. Table 3 (mean (%CV), N = 25, fasted, 15 mg):

| Parameter | Oral solution 15 mg | ROXICODONE tablet 15 mg |
|---|---|---|
| AUClast (ng.h/mL) | 212.4 (32.8) | 193.6 (30.4) |
| AUCinf (ng.h/mL) | 214.3 (34.2) | 196.5 (31.3) |
| Cmax (ng/mL) | 38.5 (30.2) | 36.3 (27.4) |
| t1/2 (h) | 3.90 | 4.15 |
| tmax (h), median (range) | 1.00 (0.50-4.00) | 1.00 (0.50-6.00) |
| CL/F (L/h) | 69.8 | 74.8 |
| Kel (1/h) | 0.18 | 0.18 |
| Vd/F (L) | 385 | 439 |

Geometric mean ratios solution/tablet (90% CI), Table 3 and p.1-2 summary: Cmax 105.2% (96.7-114.5), AUCt 108.5%
(103.2-114.2), AUCinf 107.6% (101.7-113.9); bioequivalent. (Table 3's ratio header reads "capsule/tablet", an
evident label slip: the text and summary give these as solution/tablet.)

Noroxycodone (Table 4): Cmax 22.7 vs 20.7 ng/mL, AUCinf 194.8 vs 179.4 ng.h/mL, t1/2 6.12 vs 6.48 h; GMR Cmax
110.4%, AUCinf 108.1%. Oxymorphone about 2% of parent exposure.

Food (capsules, Table 5): fed/fasted Cmax 85.9% (74.7-98.6), AUCt 120.6%, AUCinf 122.7%; tmax 1.00 -> 3.00 h;
extrapolated to the solution. Capsule/solution GMR Cmax 94.6%, AUCt 90.1%, AUCinf 90.3%. The filing memo cites the
ROXICODONE label: high-fat meal raised oral-solution AUC 27% with a 1.25 h tmax delay.

General (p.4, from background, not measured here): oral absorption about 60-87%; apparent elimination
half-life 3.5-4 h; noroxycodone via CYP3A4, oxymorphone via CYP2D6.

```yaml
parameters:
  - {name: GMR_Cmax_solution_vs_tablet, value: 105.2, unit: "%", source: "Table 3 / p.2", note: "90% CI 96.7-114.5"}
  - {name: GMR_AUCt, value: 108.5, unit: "%", source: "Table 3 / p.2", note: "90% CI 103.2-114.2"}
  - {name: GMR_AUCinf, value: 107.6, unit: "%", source: "Table 3 / p.2", note: "90% CI 101.7-113.9"}
  - {name: tmax_solution, value: 1.00, unit: h, source: "Table 3", note: "median, range 0.50-4.00"}
  - {name: tmax_tablet, value: 1.00, unit: h, source: "Table 3", note: "median, range 0.50-6.00"}
  - {name: Cmax_solution, value: 38.5, unit: ng/mL, source: "Table 3", note: "mean, 15 mg"}
  - {name: CL_F_solution, value: 69.8, unit: L/h, source: "Table 3", note: mean}
```

## Pharmacodynamics and end points
Opioid audit: no MEAC or analgesic concentration, no ke0 / t1/2ke0 / tPeak or hysteresis, and no
equianalgesic or potency ratio are reported. No PD end point was measured (BE study only).

## Covariates
None studied (pediatric studies deferred; DDI with voriconazole and rifampin noted from literature for labeling).

## Caveats
Healthy volunteers, n = 25, mostly men; single 15 mg dose; DSI inspection pending at signing. The executive
summary misnames the NDA as 200534 in one sentence (200534 is the sponsor's capsule NDA).

## Used by stanpumpR
- R/drugs_oxycodone.R:296 (header block 291-301) — bioequivalence of 20 mg/mL solution and ROXICODONE tablet,
  justifying the tablet's oral parameters for "mg PO liquid".
- inst/help/drugs/oxycodone.md:9 — the same claim in the help page.
- tests/testthat/test-drugs-oxycodone.R:138 — test comment citing the review.

## Verification against stanpumpR
- R/drugs_oxycodone.R:296-300: "15 mg of the 20 mg/mL solution against a 15 mg ROXICODONE tablet, fasted: GMR
  105.2% (Cmax), 108.5% (AUCt) and 107.6% (AUCinf), inside the 80-125% limits" and "not absorbed faster".
  Paper: identical values (Table 3, p.2); tmax medians 1.00 h in both. **matches**. (The ROXICODONE-label figures
  quoted in the same block, 21.1/130.6/1.9 h etc., are not in this review: **cannot verify** here.)
- inst/help/drugs/oxycodone.md:9: solution and IR tablet equivalent in peak and exposure, solution no faster.
  **matches**.
- tests/testthat/test-drugs-oxycodone.R:138: comment only, consistent. **matches**.
