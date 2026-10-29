---
file: NJ_label-methylprednisolone_011153_2025-11-07.pdf
pmid: none
doi: none
citation: MEDROL (methylprednisolone tablets, USP) 2, 4, 8, 16, 32 mg (Pfizer). Prescribing information, NDA 011153/S-082 (011153s082lbl), FDA label dated 2025-11-07 (Reference ID 5688056). https://www.accessdata.fda.gov/drugsatfda_docs/label/2025/011153s082lbl.pdf
kind: label
drugs: [methylprednisolone]
source_read: "text layer (whole label: Description, Actions, Indications, Contraindications, Warnings, Precautions incl. drug interactions, Adverse Reactions, Dosage and Administration incl. alternate-day therapy). The label is in the old non-PLR format and has no Clinical Pharmacology / Pharmacokinetics section."
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: discrepancy
---

# Medrol label 2025: methylprednisolone tablets

## Study
Not a study: US prescribing information for Medrol tablets, an old-format label with no PK section.

## Model and parameters
No pharmacokinetic data of any kind: no tmax, Cmax, half-life, bioavailability, clearance or volume.
The only absorption statement (Description) is that glucocorticoids are readily absorbed from the
gastrointestinal tract.

```yaml
parameters:
  - {name: potency_equivalence, value: "4 mg methylprednisolone = 5 mg prednisolone", unit: mg, source: "Dosage and Administration (multiple sclerosis paragraph)"}
  - {name: initial_dose_range, value: "4-48", unit: mg/day, source: "Dosage and Administration"}
  - {name: adrenal_suppression_single_dose, value: "1.25-1.5", unit: days, source: "Dosage and Administration, alternate-day therapy", note: "methylprednisolone classed as short acting"}
```

Drug interactions (Precautions): cyclosporin and methylprednisolone inhibit each other's metabolism;
phenobarbital, phenytoin and rifampin may increase methylprednisolone clearance; troleandomycin and
ketoconazole may decrease it; methylprednisolone may increase clearance of chronic high-dose aspirin.
Dosing: initial 4-48 mg/day individualised; alternate-day therapy rationale described (ACTH peak about
6 am, cortisol maximal 2-8 am).

## Pharmacodynamics and end points
No concentration-effect data. Potency: 4 mg methylprednisolone equivalent to 5 mg prednisolone.
Therapeutic effect persists longer than the drug's physical presence (alternate-day rationale).

## Covariates
None quantified.

## Caveats
- This label gives no time to peak, so it cannot support any absorption parameter.

## Used by stanpumpR
- `R/drugs_methylprednisolone.R:25-26`: the provisional ka_PO (1.277 /h) puts the oral plasma peak at
  90 min, "within the 1-2 h that product information gives for the tablet".
- `inst/help/drugs/methylprednisolone.md:7`: same claim ("within the 1 to 2 hours product information gives").

## Verification against stanpumpR
- `R/drugs_methylprednisolone.R:25-26` and help line 7: the US Medrol label (this edition, NDA 011153)
  states no time to peak at all; it has no pharmacokinetics section. The "1-2 h" is **not supported by
  this document**. It may come from other product information (for example a non-US SmPC) that the repo
  does not identify. **discrepancy** (unsupported citation; the ka value itself is already flagged
  PROVISIONAL in the code).
- No other value in the code is taken from this label. The label's 4 mg = 5 mg prednisolone equivalence
  is not used in the cited locations.
