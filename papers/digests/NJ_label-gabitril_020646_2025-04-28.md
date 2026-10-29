---
file: NJ_label-gabitril_020646_2025-04-28.pdf
pmid: none
doi: none
citation: "GABITRIL (tiagabine hydrochloride) tablets, Teva Pharmaceuticals; NDA 020646; FDA document dated 2025-04-28 (020646Orig1s023lbl.pdf). The archived 4-page PDF contains only the bottle (container) labels, Rev. 7/2022. https://www.accessdata.fda.gov/drugsatfda_docs/label/2025/020646Orig1s023lbl.pdf"
kind: label
drugs: [tiagabine]
source_read: "page images, all 4 pages, re-read 2026-10-14: each is a 30-tablet bottle label (2, 4, 12, 16 mg). The prescribing information (Dosage and Administration, Clinical Pharmacology) is NOT in this file."
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
revised: "2026-10-14 (source file problem stated at the top); 2026-10-27 (the 2010 prescribing information is now archived; pointer added; Used-by lines updated to master 4ea0cab)"
---

# Gabitril (tiagabine) container labels (archived file), Rev. 7/2022

**Full label now archived (2026-10-27).** The prescribing information of NDA 020646/S-017 (Revised September
2010) is `NJ_label-gabitril_020646_2010-09.pdf`, digested in `NJ_label-gabitril_020646_2010-09.md`: half-life 7-9 h
in normal volunteers (2-5 h with enzyme inducers), Tmax about 45 min fasting and a mean 2.5 h after a high-fat meal.
The repository's CHECKS line now cites "label 7-9 h without inducers" and a "fasted-to-fed Tmax range of about 45
min to 2.5 h" (`R/drugs_tiagabine.R:33-36`), both of which match that label. The 2025 prescribing information is
still not archived.

**Source file problem.** The archived PDF is not the Gabitril prescribing information: PDF pp. 1-4 are bottle
labels only (confirmed on page images 2026-10-14). No pharmacokinetic number in this card was verified from it:
the repo's "label 7-9 h without inducers" half-life and "label Tmax 1-2 h" are unverified, and this card names no
other source for them. The only facts checked are the marketed strengths (2, 4, 12, 16 mg tiagabine
hydrochloride), which is why the verdict is partly-verified (the 4 mg dose in the repo check is a real strength).

## Study
Not a study. The four pages are the bottle labels for Gabitril tablets, 30 per bottle, manufactured for Teva
Pharmaceuticals (Parsippany, NJ), made in Malta, each "Rev. 7/2022":
- p.1: 2 mg, NDC 63459-402-30
- p.2: 4 mg, NDC 63459-404-30
- p.3: 12 mg, NDC 63459-412-30
- p.4: 16 mg, NDC 63459-416-30
Each says each tablet contains the stated mg of tiagabine hydrochloride, store at 20-25 C, protect from light
and moisture, Medication Guide required, and "See package insert for full prescribing information".

## Model and parameters
None in this file: no half-life, Tmax, bioavailability, clearance, protein binding or dosing.
```yaml
parameters:
  - {name: tablet strengths, value: "2, 4, 12, 16", unit: mg tiagabine HCl, source: "pp.1-4 bottle labels"}
```

## Pharmacodynamics and end points
None. Not an opioid.

## Covariates
None.

## Caveats
- The archived file is the wrong document for the repo's purpose: the FDA posting at this URL (or the
  download) captured only the container labels. The full prescribing information (12.3 Pharmacokinetics,
  where the 7-9 h half-life and 1-2 h Tmax would be) needs to be archived separately.
- Strengths are stated as the hydrochloride salt; whether stanpumpR doses are salt or base is not settled by
  this file.

## Used by stanpumpR
- R/drugs_tiagabine.R:33-34 (master 4ea0cab) - check: model half-life 5.9 h vs "label 7-9 h without inducers".
- R/drugs_tiagabine.R:34-36 - check: a single 4 mg dose peaks at about 60 ng/mL at 2.0 h, "inside the label's
  fasted-to-fed Tmax range of about 45 min to 2.5 h". (When this card was first written, at 7bc2719, these lines
  were :28-29 and quoted a "Tmax 1-2 h" and a 25 ng/mL peak at 1.3 h.)

## Verification against stanpumpR
- **"label 7-9 h without inducers"**: not in this file. **cannot verify** here; the 2010 label
  (`NJ_label-gabitril_020646_2010-09.md`) gives 7-9 h in normal volunteers, **matches**.
- **"fasted-to-fed Tmax range of about 45 min to 2.5 h"**: not in this file; the 2010 label gives about 45 min
  fasting and a mean 2.5 h after a high-fat meal, **matches**.
- **4 mg dose** used in the check is a marketed strength (p.2). **matches**.
- The earlier side note on the 62 vs 52 L volume has been resolved in the code (V/F 52.0 L, half-life 5.9 h, with
  the abstract's 62.0 L explained as a misprint).
