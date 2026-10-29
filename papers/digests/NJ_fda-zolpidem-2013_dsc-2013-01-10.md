---
file: NJ_fda-zolpidem-2013_dsc-2013-01-10.pdf
pmid: none
doi: none
citation: "US Food and Drug Administration. FDA Drug Safety Communication: Risk of next-morning impairment after use of insomnia drugs; FDA requires lower recommended doses for certain drugs containing zolpidem (Ambien, Ambien CR, Edluar, and Zolpimist). Safety announcement, 10 January 2013."
kind: guideline
drugs: [zolpidem]
source_read: "text layer; whole document (6 pages: announcement, facts, patient and HCP information, Data Summary, dosing table, insomnia drug lists)"
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# FDA 2013: zolpidem next-morning impairment, Drug Safety Communication

## Study
Regulatory safety communication (dated 1-10-2013). Basis: driving-simulation and laboratory studies submitted
to FDA, and pooled PK trials of 10 mg Ambien or bioequivalent products in about 250 men and 250 women, plus
extended-release 12.5 mg and 6.25 mg trials. No study designs, assays or matrices are given.

## Model and parameters
No PK model. Data Summary figures:

```yaml
parameters:
  - {name: impairing_level, value: "approximately 50", unit: ng/mL, source: "Data Summary", note: "zolpidem 'blood levels' above which driving is impaired enough to raise motor-vehicle accident risk; matrix not specified"}
  - {name: IR10_over50_at8h_women, value: "about 15", unit: "%", source: "Data Summary", note: "10 mg IR, ~8 h post-dose, ~250 women"}
  - {name: IR10_over50_at8h_men, value: "3", unit: "%", source: "Data Summary", note: "~250 men"}
  - {name: IR10_ge90_at8h, value: "3 women, 1 man", source: "Data Summary", note: "measurements >=90 ng/mL at ~8 h"}
  - {name: CR12.5_over50_at8h, value: "approx 33% women, 25% men", source: "Data Summary", note: "about 5% >=100 ng/mL"}
  - {name: CR6.25_ge50_at8h, value: "about 15% women, 5% men; ~10% elderly", source: "Data Summary"}
```

Dosing changes (HCP section and table): women's recommended initial dose lowered from 10 to 5 mg (IR: Ambien,
Edluar, Zolpimist) and from 12.5 to 6.25 mg (Ambien CR); labels to recommend considering 5 mg (IR) or 6.25 mg
(CR) for men; increase to 10 / 12.5 mg allowed if needed, with greater next-morning risk. Intermezzo unchanged.
Reason given: women eliminate zolpidem more slowly than men. 2011 usage: ~39 million prescriptions, ~9 million
patients, 63% female.

## Pharmacodynamics and end points
End point: driving impairment (simulation and laboratory studies), threshold approximately 50 ng/mL zolpidem in
blood. No concentration-effect model, no ke0. Not an opioid: no MEAC or potency ratio.

## Covariates
Sex (slower elimination in women, qualitative); elderly mentioned for CR 6.25 mg.

## Caveats
The 50 ng/mL is approximate, from unspecified studies; "blood levels" is not specified as plasma or whole
blood. Percentages are from pooled trials without design detail.

## Used by stanpumpR
- R/drugs_zolpidem.R:75 (FDA lowered the starting dose for women to 5 mg in 2013), :98-102 (50 ng/mL quote,
  time-until-threshold level), :132 (reference entry).
- docs/sedatives-verification.md:59 ("Threshold: 50 ng/mL (FDA 2013 driving warning)").
- inst/help/drugs/zolpidem.md:15 (2013 dose lowering for women), :23 (50 ng/mL quote, default threshold).
- tests/testthat/test-antibiotic-thresholds.R:47 and tests/testthat/test-drugs-zolpidem.R:125 (endCe 50).

## Verification against stanpumpR
- **50 ng/mL quote (zolpidem.R:99-102; zolpidem.md:23)**: the quoted sentence matches the Data Summary
  word for word (the R header capitalises its first word). **Matches.**
- **endCe 50 (inst/extdata/drugDefaults_global.csv:29; test-drugs-zolpidem.R:125; test-antibiotic-thresholds.R:47;
  sedatives-verification.md:59)**: **matches** the DSC value. The repo reads it against plasma; the DSC says
  "blood levels" without a matrix, so plasma is an interpretation, not stated in the source (worth noting only).
- **Women's starting dose lowered to 5 mg in 2013 (zolpidem.R:75; zolpidem.md:15)**: DSC: 10 to 5 mg for IR
  products. **Matches.**
- **Citation (zolpidem.R:132-133)**: "Drug Safety Communication on zolpidem, 10 January 2013" — date matches
  (1-10-2013). **Matches.**
