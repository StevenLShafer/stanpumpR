---
file: NJ_label-amiodarone-iv_022325_2025-02-25.pdf
pmid: none
doi: none
citation: NEXTERONE (amiodarone HCl) premixed injection, NDA 022325 (Baxter Healthcare), supplement S-019 labeling posted on Drugs@FDA 2025-02-25 (Reference ID 5536045; signed M. R. Southworth 02/21/2025). https://www.accessdata.fda.gov/drugsatfda_docs/label/2025/022325Orig1s019lbl.pdf
kind: label
drugs: [amiodaroneIV]
source_read: "page images, all 5 pages, re-read 2026-10-14: PDF pp. 1-4 Nexterone carton and bag-label artwork, p. 5 FDA signature page; the file contains ONLY carton and container labels, no prescribing information"
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: not-applicable
revised: 2026-10-14 (source file problem stated at the top); 2026-10-27 (Source file problem names NJ_label-amiodarone-iv_022325_2011-12, the Nexterone PI, which verifies the regimen)
---

# NEXTERONE S-019 labeling 2025: carton and container labels only

**Source file problem.** The archived PDF is not the Nexterone prescribing information: PDF pp. 1-4 are carton and
bag-label artwork and p. 5 is the FDA signature page (confirmed on page images 2026-10-14). It has no dosing
section, so the first-day regimen the repo attributes to the label (150 mg over 10 min, then 1 mg/min for 6 h,
then 0.5 mg/min) is unverified; this card names no other source for it. Only the product concentrations and
excipients below come from this file, and the repo does not use them.
Since 2026-10-27 the Nexterone PI is archived as `NJ_label-amiodarone-iv_022325_2011-12` (2011, 50 mg/mL concentrate), whose Table 1 (PDF p2) verifies the first-day regimen and section 2 the supplemental 150 mg over 10 min.

## Study
Not a prescribing information document. The five pages are: carton for 360 mg/200 mL (p1), carton for 150 mg/100 mL
(p2), the 200 mL GALAXY bag label (p3), the 100 mL bag label (p4), and the FDA electronic-signature page (p5).
Each carton/bag says "Dosage: See prescribing information." No sections 2 or 12 are present.

## Model and parameters
Product facts only (from the carton and bag labels):
```yaml
parameters:
  - {name: concentration_360, value: 1.8, unit: mg/mL, source: "p1, p3", note: "360 mg/200 mL; NDC 43066-360-20; 18 mg/mL Betadex sulfobutyl ether sodium; 41.4 mg/mL dextrose"}
  - {name: concentration_150, value: 1.5, unit: mg/mL, source: "p2, p4", note: "150 mg/100 mL; NDC 43066-150-10; 15 mg/mL Betadex sulfobutyl ether sodium; 42.1 mg/mL dextrose"}
```
Also 0.362 mg/mL citric acid anhydrous, 0.183 mg/mL sodium citrate dihydrate; no polysorbate 80 or benzyl alcohol;
iso-osmotic in dextrose; single-dose GALAXY container; store 20-25 C, protect from light. No dosing regimen, no PK.

## Pharmacodynamics and end points
None in this document. Not an opioid; no MEAC, ke0 or therapeutic range.

## Covariates
None.

## Caveats
The archived PDF is the wrong part of the labeling for its purpose: the 24-hour loading regimen the repo cites is
in the NEXTERONE prescribing information (section 2), which this file does not include. The archive needs the full
PI (e.g. an earlier or later NDA 022325 "lbl" posting that contains it).

## Used by stanpumpR
- R/drugs_amiodaroneIV.R:100 (header) - "the label's first-day regimen (150 mg over 10 minutes, 1 mg/min for 6
  hours, then 0.5 mg/min)".
- R/help-scenarios.R:183, :188 (now :202-212) - 'amiodarone-iv-loading' scenario: 15 mg/min at 0, 1 mg/min at 10,
  0.5 mg/min at 370 min.
- inst/help/drugs/amiodaroneIV.md:30; inst/help/in-development.md:22; inst/help/scenarios/amiodarone-iv-loading.md:3,
  :7, :13, :14 - the regimen, 1050 mg in 24 h, and the supplemental 150 mg over 10 min.
- tests/testthat/test-drugs-amiodaroneIV.R:78-82 and later lines - LABEL_TIMES c(0, 10, 370), LABEL_RATES c(15, 1,
  0.5).

## Verification against stanpumpR
- Every location (regimen 150 mg/10 min, 360 mg/6 h, 540 mg/18 h; supplemental 150 mg over 10 min): **cannot
  verify** from this file, which holds no dosing information.
- Internal consistency only (derived): 15 mg/min x 10 min = 150 mg; 1 mg/min x 360 min = 360 mg; 0.5 mg/min x 1080
  min = 540 mg; total 1050 mg; switch times 10 and 370 min are right for 10 min and 6 h 10 min. The code, scenario
  and tests agree with one another.
- The product concentrations here (1.5 and 1.8 mg/mL premixed) are not used by the repo.
