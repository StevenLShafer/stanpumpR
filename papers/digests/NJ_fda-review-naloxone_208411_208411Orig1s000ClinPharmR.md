---
file: NJ_fda-review-naloxone_208411_208411Orig1s000ClinPharmR.pdf
pmid: none
doi: none
citation: "US FDA CDER. Clinical Pharmacology and Biopharmaceutics Review(s), NDA 208411, NARCAN (naloxone HCl) nasal spray 4 mg/0.1 mL (Adapt Pharma; 505(b)(2), listed drug Narcan injection NDA 016636). Primary review S.B. Naraharisetti, Y. Xu, signed 10/22/2015 (Reference ID 3837063); OSIS addendum signed 11/02/2015; filing form 09/11/2015. https://www.accessdata.fda.gov/drugsatfda_docs/nda/2015/208411Orig1s000ClinPharmR.pdf"
kind: review
drugs: [naloxone]
source_read: "text layer, whole document (34 pp.: addendum, primary review sections 1-4.2, filing form). Some tables (2.4.1b) are scrambled in the text layer and were reassembled by row; figures not read."
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: discrepancy
---

# FDA 2015: NDA 208411 NARCAN nasal spray clinical pharmacology review (with OSIS addendum)

## Study
One pivotal relative bioavailability study, Naloxone-Ph1a-002 (Vince & Associates, Overland Park KS; Nov 2014 -
Jan 2015): open-label, randomized, 5-period, 5-treatment crossover, 4-day washouts. 30 healthy adults randomized
(18 men, 12 women; mean age 36 (22-55) y, BMI 27.1 (19.6-29.8) kg/m2), 28 completed; PK n = 29 per arm.
Treatments: A 2 mg IN (20 mg/mL, one nostril), B 4 mg IN (20 mg/mL, both nostrils), C 4 mg IN (40 mg/mL, one
nostril, the marketed product), D 8 mg IN (40 mg/mL, both nostrils), E 0.4 mg IM (gluteus, ANDA generic of
Narcan). Supine for 1 h. Samples 0-720 min (2.5, 5, 10, 15, 20, 30, 45, 60, 120 ... 720). HPLC-MS/MS, calibration
0.01-10 ng/mL, naloxone-d5. No intravenous arm. Non-compartmental analysis. The addendum records OSIS findings
(sampling-time deviations, bench stability) and leaves conclusions unchanged.

## Model and parameters
No compartmental model. Table 2.4.1b, geometric mean (CV%), n = 29:

| Parameter | 4 mg IN one nostril | 8 mg IN two nostrils | 0.4 mg IM |
|---|---|---|---|
| Cmax (ng/mL) | 4.8 (43) | 9.7 (36) | 0.88 (31) |
| AUC0-2h (h.ng/mL) | 5.5 (35) | 10.5 (21) | 1.06 (22) |
| AUC0-4h | 7.1 (36) | 13.5 (22) | 1.52 (21) |
| AUCt | 7.9 (37) | 15.3 (23) | 1.72 (23) |
| AUCinf | 8.0 (37) | 16 (23) | 1.8 (23) |
| tmax (h), median (range) | 0.50 (0.17-1.00) | 0.33 (0.17-1.00) | 0.38 (0.08-2.05) |
| Cmax/dose (ng/mL/mg) | 1.2 | 1.21 | 2.2 |
| AUCinf/dose (h.ng/mL/mg) | 2.0 | 1.9 | 4.4 |
| t1/2 (h) | 2.1 (30) | 2.1 (32) | 1.2 (26) |
| Dose-normalized BA vs IM (%) | 47 (31) | 44 (24) | 100 |

GMR IN/IM (Table 1.3a/2.4.1c): 4 mg Cmax 555% (464-665), AUCt 469% (418-527), AUCinf 462% (412-519); 8 mg 1110%
(925-1320), 890% (793-999), 878% (783-985). Mean concentrations 2.5-60 min (Table 2.4.1a): IM peak 0.754 ng/mL
at 20 min; 4 mg IN 4.53 ng/mL at 20 min; 8 mg IN 8.14 at 20 min. Synopsis (all four IN arms): relative BA vs IM
43.9-53.6%; dose-normalized Cmax 1.21-1.55 ng/mL/mg; slightly less than dose proportional (Rdnm AUCinf 0.847).
IM Cmax 880 pg/mL at about 23 min; IN stayed above 880 pg/mL to 120 min (4 mg) and 180 min (8 mg).

```yaml
parameters:
  - {name: AUCinf_4mg_IN, value: 8.0, unit: h.ng/mL, source: "Table 2.4.1b", note: geometric mean}
  - {name: Cmax_4mg_IN, value: 4.8, unit: ng/mL, source: "Table 2.4.1b", note: geometric mean}
  - {name: relBA_4mg_IN_vs_IM, value: 47, unit: "%", source: "Table 2.4.1b", note: "dose-normalized, assumes IM = 100%"}
  - {name: relBA_8mg_IN_vs_IM, value: 44, unit: "%", source: "Table 2.4.1b"}
  - {name: CL_F_4mg_IN, value: 500, unit: L/h, source: "derived: 4000 ug / 8.0 h.ng/mL"}
  - {name: t_half_IN, value: 2.1, unit: h, source: "Table 2.4.1b"}
  - {name: t_half_IM, value: 1.2, unit: h, source: "Table 2.4.1b"}
```

## Pharmacodynamics and end points
Opioid-antagonist audit: no MEAC, effective or reversal concentration, no ke0 / t1/2ke0 / tPeak or hysteresis,
no potency ratio. No PD end point measured (PK bridging only). Labelled adult IV dose of Narcan cited as 0.4-2 mg.

## Covariates
None; no apparent sex difference (synopsis).

## Caveats
Healthy volunteers, single doses, supine, instructed not to breathe through the nose. Relative BA is against IM,
not IV; absolute bioavailability is not estimated anywhere in the review. Doses are naloxone hydrochloride.

## Used by stanpumpR
- R/drugs_naloxone.R:44 (header 39-50) — cited as "the FDA review's own route to F (fixing intravenous
  clearance)", supporting nasal F = reference IV clearance / apparent CL/F (75.4 / 396 = 0.190), and the Narcan
  label exposure (4 mg AUC near 7.9 ng.h/mL, CL/F about 500 L/h).

## Verification against stanpumpR
- R/drugs_naloxone.R:44-45: "the FDA review's own route to F (fixing intravenous clearance)". This review has no
  intravenous arm, no compartmental model and no estimate of absolute F; it reports only relative BA against
  0.4 mg IM (47% and 44%). Nothing in it fixes an IV clearance. **discrepancy** (claim unsupported by this
  document; the review meant may be a different one, which the repo does not name).
- R/drugs_naloxone.R:45-47: 4 mg gives AUC near 7.9 ng.h/mL, CL/F about 500 L/h. Review: AUCt 7.9, AUCinf 8.0
  h.ng/mL (geometric), so CL/F about 500 L/h (derived). **matches** (the repo attributes it to the label; the
  review holds the same numbers).
- R/drugs_naloxone.R:47-49: label "relative to intramuscular" figures 0.47-0.52. Review: 47% (4 mg), 44% (8 mg);
  43.9-53.6% across all IN arms. **cannot verify** the 0.52 from this review (label not read); the 0.47 matches.
