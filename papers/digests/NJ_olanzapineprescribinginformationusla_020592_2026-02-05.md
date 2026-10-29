---
file: NJ_olanzapineprescribinginformationusla_020592_2026-02-05.pdf
pmid: none
doi: none
citation: "ZYPREXA (olanzapine) tablets, ZYPREXA ZYDIS, ZYPREXA IntraMuscular, and ZYPREXA RELPREVV US prescribing information (combined PDF), Eli Lilly / Cheplapharm, NDA 020592 (supplement s079), Drugs@FDA label dated 2026-02-05 (\"Revised: 1/2026\"). https://www.accessdata.fda.gov/drugsatfda_docs/label/2026/20592s079lbl.pdf"
kind: label
drugs: [olanzapine]
source_read: text layer; read highlights, sections 2.1 and 2.4, 12.3 of the ZYPREXA label and 12.3 of the RELPREVV label; searched the full text for ng/mL, Cmax, peak and half-life
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: discrepancy
---

# Zyprexa (olanzapine) label (2026)

## Study
Not a study: combined US label (78 pages) for oral, orally disintegrating, short-acting IM and long-acting
pamoate (RELPREVV) olanzapine.

## Model and parameters
ZYPREXA 12.3 (oral):
- Peak concentration about 6 h after an oral dose; about 40% of the dose cleared by first-pass metabolism;
  food does not affect rate or extent; tablets and ZYDIS bioequivalent.
- Linear over the clinical range. Half-life 21-54 h (5th-95th percentile; mean 30 h); apparent clearance
  12-47 L/h (5th-95th; mean 25 L/h).
- Once-daily dosing reaches steady state in about 1 week at about twice single-dose concentrations.
- V about 1000 L; 93% protein bound over 7-1100 ng/mL (albumin, alpha1-acid glycoprotein).
- Metabolism: direct glucuronidation and CYP1A2 (CYP2D6 minor in vivo); 10-N-glucuronide 44% and
  4'-N-desmethyl 31% of parent at steady state, both inactive. 7% unchanged in urine; 57% urine, 30% faeces.
- IM (ZYPREXA IntraMuscular): peak 15-45 min; 5 mg IM Cmax about 5 times 5 mg oral; similar AUC and half-life.
- Specific populations: elderly half-life ~1.5x; women clearance ~30% lower; smokers clearance ~40% higher;
  young male smokers may clear 3 times faster than elderly nonsmoking women; renal impairment no effect, not
  dialysable; mild-moderate cirrhosis little effect.
RELPREVV 12.3: effective half-life about 30 days; 150 mg/2 weeks ~ 10 mg/day oral, 300 mg/2 weeks ~ 20 mg/day.
Dosing (2.1): schizophrenia start 5-10 mg once daily, target 10 mg/day, max 20 mg/day; IM agitation 10 mg.

The label gives NO single-dose Cmax value or range for any oral dose (no ng/mL figure other than the
protein-binding range and prolactin thresholds).

```yaml
parameters:
  - {name: tmax_oral, value: 6, unit: h, source: "ZYPREXA 12.3", note: approximate}
  - {name: t_half, value: 30, unit: h, source: "ZYPREXA 12.3", note: "mean; 5th-95th 21-54"}
  - {name: CL_F, value: 25, unit: L/h, source: "ZYPREXA 12.3", note: "mean; 5th-95th 12-47"}
  - {name: V, value: 1000, unit: L, source: "ZYPREXA 12.3", note: approximate}
  - {name: first_pass, value: 40, unit: "% of dose", source: "ZYPREXA 12.3"}
  - {name: CL_female, value: -30, unit: "%", source: "ZYPREXA 12.3"}
  - {name: CL_smoker, value: 40, unit: "% higher", source: "ZYPREXA 12.3"}
```

## Pharmacodynamics and end points
No plasma concentration-effect, therapeutic range, receptor occupancy or ke0 in the sections read. Not an
opioid.

## Covariates
Sex, smoking, age (qualitative multipliers above); CYP1A2 inducers/inhibitors (fluvoxamine raises Cmax 54-77%;
charcoal lowers Cmax and AUC ~60%).

## Caveats
Population statistics (percentiles) of half-life and clearance pool smokers and nonsmokers, men and women.

## Used by stanpumpR
- tests/testthat/test-drugs-olanzapine.R:58 — comment: peak about 5 h after 10 mg, 11 to 13 ng/mL, "the
  label's 10 mg Cmax range"; the test asserts only that the peak time is between 240 and 360 min.

## Verification against stanpumpR
- test-drugs-olanzapine.R:58: the label has no 10 mg (or any) Cmax value or range. **discrepancy**: the
  11-13 ng/mL figure needs another source (e.g. a bioequivalence or PK study) or the attribution should go.
  The peak-time window 4-6 h brackets the label's ~6 h only at its open upper bound (expect_lt 360);
  model peak "about 5 h" is a little earlier than the label's ~6 h.
- Derived context (not a repo claim): R/drugs_olanzapine.R uses Sun 2021 CL/F 15.5 L/h for a nonsmoking man,
  within the label's 12-47 L/h percentile range and below its 25 L/h population mean, as expected for a
  nonsmoker; Vc/F + Vp/F = 881 L vs label ~1000 L.
