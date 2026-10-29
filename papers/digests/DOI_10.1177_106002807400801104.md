---
file: DOI_10.1177_106002807400801104.pdf
pmid: none (not indexed in PubMed)
doi: 10.1177/106002807400801104
citation: Devine BJ. Gentamicin therapy (Clinical pharmacy case studies, case number 25, ed. McCarron MM). Drug Intell Clin Pharm 1974;8(11):650-655.
kind: other
drugs: [metronidazole, phenobarbital]
source_read: text layer
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# Devine 1974: Gentamicin therapy (origin of the "Devine" weight formula)

## Study
Not a study: a continuing-education teaching case (USC School of Pharmacy series, case 25). An 85-year-old man,
73 kg, 178 cm, with aspiration pneumonia, given gentamicin, who developed ototoxicity and a rising serum creatinine
(1.3 -> 1.9 -> 2.7 mg/dL). The article reviews gentamicin toxicity and methods for estimating creatinine clearance
and adjusting the dose (Rule of 8's, Siersbaek-Nielsen nomogram, Jelliffe formula, creatinine-production formulae,
McHenry half-life estimate, Chan nomogram), compared in Table 2 for the case patient (Ccr estimates 35-45 mL/min).

## Model and parameters
No PK model is fitted. The weight formulae appear as equations 8 and 9 (p. 653), introduced as formulae for
estimating **lean body weight** for the creatinine-production equations (6, 7), since creatinine production occurs
in muscle and obese patients need their weight adjusted:

- Eq 8, males: 50 kg + 2.3 kg for each inch over 5 feet.
- Eq 9, females: 45.5 kg + 2.3 kg for each inch over 5 feet.

No derivation, data, population, or height range is given for these formulae; no citation is attached to them.

Other equations in the article (for completeness): Jelliffe Ccr(male) = [98 - 0.8 (age - 20)] / SCr, female x 0.9;
creatinine production P(male) = [29.3 - 0.203 x age] x LBW, P(female) = [25.3 - 0.175 x age] x LBW (mg/day);
McHenry gentamicin t1/2 (h) = 277 / Ccr (mL/min); Rule of 8's (dosing interval h = 8 x SCr).

```yaml
parameters:
  - {name: LBW_male_intercept, value: 50, unit: kg, source: "eq 8, p.653", note: "at 5 ft (60 in)"}
  - {name: LBW_female_intercept, value: 45.5, unit: kg, source: "eq 9, p.653", note: "at 5 ft (60 in)"}
  - {name: LBW_slope, value: 2.3, unit: kg per inch over 60 in, source: "eqs 8-9, p.653"}
```

## Pharmacodynamics and end points
Not applicable. Gentamicin therapeutic range quoted as 3-10 mcg/mL (from earlier references); toxicity thresholds
by daily dose quoted from Jackson & Arcieri.

## Covariates
Sex and height (weight formulae); age, weight, serum creatinine (Ccr estimates).

## Caveats
Not a research paper; the case was altered for teaching. The formulae are presented as **lean** body weight, not
"ideal" body weight; the "ideal body weight" name comes from later usage. No validation, no stated height range;
the formula is linear and becomes negative at short stature (derived: zero at about 38 in / 97 cm for men, 40 in /
102 cm for women).

## Used by stanpumpR
- R/drugs_metronidazole.R:16, 37, 43-52, 100-125: idealBodyWeightDevine() and adjustedBodyWeight() (IBW + 0.4 x
  excess) for the da Silva Neto metronidazole volume with the FFM switch off.
- R/drugs_phenobarbital.R:32-34, 78: shared idealBodyWeightDevine() for phenobarbital's IBW allometry.
- inst/help/drugs/metronidazole.md:3, 13; inst/help/drugs/phenobarbital.md:7; docs/weight-adjustment.md:153.
- tests/testthat/test-drugs-metronidazole.R:48-89; tests/testthat/test-drugs-phenobarbital.R:11;
  tests/testthat/test-drug-library-pediatric.R:5.

## Verification against stanpumpR
- idealBodyWeightDevine() (R/drugs_metronidazole.R:102-106): 50 (men) or 45.5 (women) + 2.3 x (height_cm / 2.54 -
  60). Paper eqs 8-9: 50 / 45.5 kg + 2.3 kg per inch over 5 feet. Inch conversion 2.54 cm correct. **matches**
- Help/header descriptions (metronidazole.md:3, 13; phenobarbital R:32-34; help phenobarbital:7) of 50/45.5 + 2.3
  kg/inch over 60 in. **matches**
- Negative-weight thresholds "below about 97 cm in a man, 102 cm in a woman" (R/drugs_metronidazole.R:120;
  help:13). Derived: 60 - 50/2.3 = 38.3 in = 97.2 cm; 60 - 45.5/2.3 = 40.2 in = 102.0 cm. **matches**
- Terminology: the repo calls it "ideal body weight"; Devine labelled it an estimate of lean body weight for
  creatinine production. Conventional usage, and the downstream sources (da Silva Neto, the phenobarbital study)
  call it IBW, so not a discrepancy in the numbers; noted only. The repo has no full citation for Devine anywhere.
