---
file: NJ_fda-review-naloxone_208411_208411-naloxone-clinpharm-prea.pdf
pmid: none
doi: none
citation: "US FDA CDER. Clinical Pharmacology Review, NDA 208411, NARCAN (naloxone HCl) nasal spray 4 mg/0.1 mL (Adapt Pharma), posted as the pediatric (PREA) review. S.B. Naraharisetti, Y. Xu, signed 10/22/2015 (Reference ID 3837063). https://www.accessdata.fda.gov/drugsatfda_docs/pediatric/208411 naloxone clinpharm prea.pdf"
kind: review
drugs: [naloxone]
source_read: "text layer, whole document (23 pp.: primary review sections 1-4.2). Same primary review as in NJ_fda-review-naloxone_208411_208411Orig1s000ClinPharmR (same Reference ID), without the OSIS addendum and filing form, with fewer redactions (molecular weight 399.87 shown) and with Table 2.4.1c printed."
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: discrepancy
---

# FDA 2015: NDA 208411 NARCAN nasal spray clinical pharmacology review (PREA posting)

## Study
Pivotal relative bioavailability study Naloxone-Ph1a-002: open-label 5-period, 5-treatment crossover in healthy
adults (30 randomized, 18 M / 12 F, mean age 36 y, mean BMI 27.1; 28 completed; PK n = 29). Arms: 2 mg and 4 mg IN
of a 20 mg/mL solution, 4 mg (one nostril) and 8 mg (two nostrils) IN of the marketed 40 mg/mL, and 0.4 mg IM
(reference). Samples to 720 min; HPLC-MS/MS 0.01-10 ng/mL. No IV arm. Non-compartmental. Pediatric section: the
applicant proposed one 4 mg IN dose for all ages from infants, supported by literature; neonatal delivery under
discussion with PeRC.

## Model and parameters
No model. Table 2.4.1b, geometric means (CV%), n = 29: 4 mg IN Cmax 4.8 (43) ng/mL, AUCt 7.9 (37), AUCinf 8.0
(37) h.ng/mL, tmax 0.50 h, t1/2 2.1 h, dose-normalized BA vs IM 47 (31)%; 8 mg IN Cmax 9.7, AUCinf 16, tmax 0.33 h,
t1/2 2.1 h, BA 44 (24)%; 0.4 mg IM Cmax 0.88, AUCinf 1.8, tmax 0.38 h (0.08-2.05), t1/2 1.2 h. Table 2.4.1c GMR
IN/IM: 4 mg Cmax 555%, AUCt 469%, AUCinf 462%; 8 mg 1110%, 890%, 878%. Early means (Table 2.4.1a), 4 mg IN at 2.5,
5, 10, 20 min: 0.28, 1.48, 3.24, 4.53 ng/mL; IM 0.079, 0.306, 0.578, 0.754. Synopsis: relative BA 43.9-53.6%
across IN arms; slightly less than dose proportional.

```yaml
parameters:
  - {name: AUCinf_4mg_IN, value: 8.0, unit: h.ng/mL, source: "Table 2.4.1b", note: geometric mean}
  - {name: relBA_4mg_IN_vs_IM, value: 47, unit: "%", source: "Table 2.4.1b", note: "dose-normalized vs IM"}
  - {name: CL_F_4mg_IN, value: 500, unit: L/h, source: "derived: 4000 ug / 8.0 h.ng/mL"}
```

## Pharmacodynamics and end points
No MEAC or reversal concentration, no ke0 / t1/2ke0 / tPeak, no potency ratio; no PD end point.

## Covariates
None.

## Caveats
As for the Orig1s000 posting: healthy supine volunteers; relative BA against IM only; no absolute F. This file is a
second posting of the same review, not a separate evaluation.

## Used by stanpumpR
- R/drugs_naloxone.R:44 — "the FDA review's own route to F (fixing intravenous clearance)" for nasal
  F = 75.4 / 396 = 0.190.

## Verification against stanpumpR
- R/drugs_naloxone.R:44-45: no IV data, no model, no IV-clearance-based F in this review; only relative BA vs IM
  (47%, 44%). **discrepancy** (claim unsupported by this document).
- R/drugs_naloxone.R:45-47: AUC near 7.9 ng.h/mL for 4 mg, CL/F about 500 L/h: AUCt 7.9 / AUCinf 8.0. **matches**.
