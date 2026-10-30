---
file: NJ_label-furosemideinjection_018579_2024-10-03.pdf
pmid: ""
doi: ""
citation: "Furosemide Injection, USP 10 mg/mL (American Regent, Inc.; NDA 018579). FDA-approved labeling, Rev. October 2024, Reference ID 5454004 (https://www.accessdata.fda.gov/drugsatfda_docs/label/2024/018579s034lbl.pdf)."
kind: label
drugs: [furosemide]   # not a stanpumpR drug model; antihypertensive registry catalogue record only
source_read: "text layer, all 11 PDF pages (each footed 'Page n of 11'): Boxed warning, Description, Clinical Pharmacology (p. 1-2), Geriatric and Pediatric Population (p. 2), Indications (p. 2-3), Contraindications and Warnings (p. 3), Precautions incl. Pregnancy, Pediatric and Geriatric Use (p. 4-8), Adverse Reactions (p. 8-9), Overdosage (p. 9-10), Dosage and Administration (p. 10-11), How Supplied (p. 11). Clean prose; nothing rendered."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: not-applicable
---

# Furosemide Injection USP (American Regent), FDA label NDA 018579, October 2024

Old-format label, distributed by American Regent (Shirley, NY); vials 2, 4 and 10 mL of 10 mg/mL (NDC 0517-5702, -5704, -5710) for IV and IM use. Molecular weight 330.74; each mL has sodium hydroxide 1.5 mg, pH 8.0-9.3 (p. 1). The file name's date (2024-10-03) agrees with the label's Rev. October 2024.

## Study
Not a study.

## Model and parameters
No population model. Clinical Pharmacology (p. 2), word for word the same facts as the Baxter SPL (NJ_dailymed-c71371a7):

```yaml
parameters:
  - {name: protein_binding, value: "91-99", unit: "%", source: "Clinical Pharmacology p. 2", note: "at 1-400 ug/mL; unbound 2.3-4.1%"}
  - {name: F_tablet, value: 64, unit: "%", source: "Clinical Pharmacology p. 2", note: "relative to IV"}
  - {name: F_oral_solution, value: 60, unit: "%", source: "Clinical Pharmacology p. 2", note: "relative to IV"}
  - {name: t_half_terminal, value: 2, unit: h, source: "Clinical Pharmacology p. 2", note: "approximately"}
  - {name: CL_preterm_PNA_lt30d, value: 8.9, unit: mL/h/kg, source: "Pediatric Population p. 2", note: "median (2.1-21.2); 51 premature infants GA 23-29 wk"}
  - {name: CL_preterm_PNA_ge30d, value: 25.3, unit: mL/h/kg, source: "Pediatric Population p. 2", note: "median (8.3-44.2)"}
  - {name: F_enteral_preterm, value: 79, unit: "%", source: "Pediatric Population p. 2", note: "about 79% relative to IV"}
```

Absorption times 50 min (solution) and 87 min (tablet); glucuronide the main metabolite; elderly men lower renal clearance after IV (p. 2). Hemodialysis does not accelerate elimination (p. 10).

## Pharmacodynamics and end points
IV diuresis begins within 5 min, peaks within 30 min, lasts about 2 h; IM somewhat later (p. 2). No therapeutic or toxic concentration; toxic level unknown. Preterm infants < 31 wk post-conceptual age on > 1 mg/kg/24 h risk ototoxic levels (p. 3).

## Covariates
Postnatal age and weight on preterm clearance; age on renal clearance (descriptive).

## Caveats
Dosage (p. 10-11): adult edema 20-40 mg IV (over 1-2 min) or IM, again or +20 mg after >= 2 h; high-dose infusion <= 4 mg/min; acute pulmonary edema 40 mg then 80 mg after 1 h; pediatric 1 mg/kg, +1 mg/kg after >= 2 h, <= 6 mg/kg; premature infants <= 1 mg/kg/day. Do not mix with acidic drugs such as milrinone (p. 10).

## Used by stanpumpR
From the context file: the official label for record furosemide__injection__iv__catalog__none__label, named by model_id in tests/testthat/test-antihypertensive-registry.R, docs/antihypertensives.md and inst/help/models/antihypertensives.md (at 8baa733: test lines 169-170, docs line 30, help line 19).

## Verification against stanpumpR
- At 8baa733 this document is not cited: the record's source_url (inst/extdata/antihypertensiveRegistry.csv:60; data-raw/antihypertensiveRegistry.R:334) is the DailyMed SPL of Baxter's generic (setid c71371a7..., digested as NJ_dailymed-c71371a7), and the accessdata URL "018579s034lbl" does not occur anywhere in the repo's history (git log -S over all refs). Verdict **not-applicable**.
- For what it is worth, this label supports the same identity (furosemide injection, intravenous, also IM) and its clinical pharmacology text is identical to the cited SPL.
