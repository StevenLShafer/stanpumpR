---
file: NJ_fda-nda211759_211759_clinpharm-addendum-prea.pdf
pmid: none
doi: none
citation: "U.S. FDA, CDER Office of Clinical Pharmacology, Addendum: Update on OSIS Inspection Assessment, NDA 211759 resubmission, VYSCOXA (celecoxib) oral suspension 10 mg/mL. Wei Qiu, Deep Kwatra; dated and signed 07/21/2025; Reference ID 5628526."
kind: review
drugs: [celecoxib]
source_read: text layer, both pages (complete)
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: not-applicable
---

# FDA OCP addendum 2025, NDA 211759: OSIS inspection of Study 915/22

## Study
No new data. A two-page memo closing the item left open in the 06/24/2025 Clinical Pharmacology review: the Office of
Study Integrity and Surveillance (OSIS) inspection of comparative bioavailability Study 915/22.
- Analytical site inspected (name redacted): OSIS memo 01/31/2025, no concern with the analytical data.
- Clinical site, Quinta-Analytica, Prague: OSIS review 07/18/2025, no concerns about data reliability or human-subject
  protection.
- Conclusion: from a clinical pharmacology standpoint the resubmission is acceptable (the review's pending condition
  is removed).

## Model and parameters
None reported.
```yaml
parameters: []
```

## Pharmacodynamics and end points
None.

## Covariates
None.

## Caveats
Administrative only. Its relevance is that the Study 915/22 data behind the review's Figure 1 and Table 1 passed
inspection.

## Used by stanpumpR
The context assigns it the same celecoxib locations as the main OCP review (R/drugs_celecoxib.R, celecoxib.md:3,
test-drugs-celecoxib.R, docs/nsaid-models.md:17, 37-39); the repo cites "the FDA Office of Clinical Pharmacology
Review", not this addendum.

## Verification against stanpumpR
- No numbers in this document; nothing in the repo depends on it: **not applicable**.
- docs/nsaid-models.md:37 "Neither FDA review publishes the applicant's two-compartment coefficients": true of this
  addendum too (**matches**).
- The review the repo uses was signed 06/24/2025 (R/drugs_celecoxib.R:16-17); this addendum confirms that date.
