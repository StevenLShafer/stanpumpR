---
file: NJ_label-mirtazapine_020415_2026-05-21.pdf
pmid: none
doi: none
citation: "REMERON (mirtazapine) tablets, NDA 020415 supplement S-039 (Organon). The archived PDF is not the prescribing information: it is 2 pages of the FDA S-039 action package (carton/container label images for the 15 mg and 30 mg bottles, page marked 'Page 3', and a digital-signature page dated 7/23/2025). Source URL https://www.accessdata.fda.gov/drugsatfda_docs/label/2026/020415Orig1s039lbl.pdf, catalogue date 2026-05-21."
kind: label
drugs: [mirtazapine]
source_read: "page images, both pages, re-read 2026-10-14: PDF p. 1 is two Remeron bottle-label images, p. 2 an FDA signature block. No prescribing-information sections are present."
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: not-applicable
revised: 2026-10-14 (source file problem stated at the top); 2026-10-27 (Source file problem names NJ_label-mirtazapine_020415_s023s024, the Remeron PI, which verifies F about 50% and the 20 to 40 h half-life)
---

# FDA 2025/2026: Remeron NDA 020415 S-039 (carton labels only)

**Source file problem.** The archived PDF is not the Remeron prescribing information: PDF p. 1 is bottle-label
artwork (15 and 30 mg) and p. 2 is the FDA signature page (confirmed on page images 2026-10-14). No number in this
card was verified from it: the repo's oral bioavailability "about 50%" and "label" half-life 20-40 h are
unverified, and neither the repo nor this card names the document they came from.
Since 2026-10-27 the Remeron PI is archived as `NJ_label-mirtazapine_020415_s023s024`, which verifies both (absolute bioavailability about 50%, half-life about 20 to 40 h; PDF p2).

## Study
Not a study. The archived file holds:
- Page 1 (headed "NDA 020415/S-039, Page 3"): two bottle-label images, REMERON (mirtazapine) Tablets 30 mg (NDC 78206-161-01) and 15 mg (NDC 78206-160-01), 30 tablets each; Organon USA LLC, manufactured by Organon Pharma (UK); storage 20-25 °C; "Usual dosage: see package insert".
- Page 2: digital signature of an FDA official, dated 7/23/2025.

There is no Highlights, no Dosage and Administration, and no section 12 Clinical Pharmacology in this file.

## Model and parameters
None in this document.
```yaml
parameters: []   # the archived file contains no PK information
```

## Pharmacodynamics and end points
None. (Not an opioid; no MEAC, ke0 or potency information.)

## Covariates
None.

## Caveats
- **Wrong or truncated document archived.** The file named as the mirtazapine label is only the carton-label pages of the S-039 approval package. The full Remeron prescribing information (section 12.3: bioavailability, half-life) needs to be downloaded in its place before the repo's "label" statements can be checked.
- The repo itself never identifies which mirtazapine label (product, regulator, date) it relies on (context notes).

## Used by stanpumpR
- `R/drugs_mirtazapine.R:51` - "about 50% by the label" for the unmeasured oral bioavailability F behind CL/F and V/F.
- `R/drugs_mirtazapine.R:62` - model half-life 7.4 h "against the label's 20 to 40 h", used to call the short half-life a trough-data artefact.
- `inst/help/drugs/mirtazapine.md:17` - same contrast (7.4 h vs label 20 to 40 h).
- `tests/testthat/test-drugs-mirtazapine.R:129` - test name cites the label's 20 to 40 h; the test pins only the model's 7.435 h.
- `inst/help/models/antidepressants.md:22` - mirtazapine about 7 h against the label's 20 to 40.

## Verification against stanpumpR
- `R/drugs_mirtazapine.R:51` (F about 50%): **cannot verify** - no bioavailability in the archived file.
- `R/drugs_mirtazapine.R:62`, `inst/help/drugs/mirtazapine.md:17`, `inst/help/models/antidepressants.md:22` (label half-life 20 to 40 h): **cannot verify** - no half-life in the archived file.
- `tests/testthat/test-drugs-mirtazapine.R:129`: the test checks only the model's own value (ln 2 x V/CL = 7.435 h, from 310 L and 28.9 L/h; derived: 0.693 x 310 / 28.9 = 7.43 h), which is internally consistent; the label comparison in its name **cannot be verified** from this file.
- Action needed (not done here): replace the archived PDF with the full Remeron prescribing information and re-digest; the repo should also name the label (Remeron, NDA 020415, revision date) where it says "the label".
