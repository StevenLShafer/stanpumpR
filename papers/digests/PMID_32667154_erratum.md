---
file: PMID_32667154_erratum.pdf
pmid: 32667154
doi: 10.1097/ALN.0000000000003549
citation: "Erratum: Perioperative use of gabapentinoids for the management of postoperative acute pain: a systematic review and meta-analysis. Anesthesiology 2020;133(5):1159."
kind: other
drugs: [pregabalin, gabapentin]
source_read: "text layer (one page), checked on the page image 2026-10-14"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 51e02bd
verification: not-applicable
---

# Verret 2020 erratum: corrected control-group opioid dose

## Study
One-page erratum (Anesthesiology 2020;133(5):1159, November 2020) to Verret et al., Anesthesiology 2020;133:265-79
(PMID 32667154, digested in `PMID_32667154.md`). No data of its own.

## Model and parameters
In the Results subsection on cumulative opioid dose within 24, 48 and 72 h (article p. 270), the control group's mean
IV morphine equivalent is corrected from 38.7 mg to 32.6 mg (gabapentinoid group 25.3 mg). The archived article
(`PMID_32667154.pdf`, corrected November 13, 2020) already prints 32.6 mg.

```yaml
parameters:
  - {name: control IV morphine equivalent 0-24 h, value: 32.6, unit: mg, source: "erratum p. 1159", note: "corrected from 38.7; gabapentinoid group 25.3"}
```

## Pharmacodynamics and end points
None.

## Covariates
None.

## Caveats
None beyond the correction itself.

## Used by stanpumpR
Not cited directly; stanpumpR cites the article (`inst/help/drugs/pregabalin.md:27`), which does not quote the opioid
doses.

## Verification against stanpumpR
Nothing in stanpumpR depends on the corrected value. **not-applicable**.
