---
file: NJ_dailymed-db8c69b0.pdf
pmid: null
doi: null
citation: "DILANTIN (extended phenytoin sodium capsules, USP) 30 mg, oral. Prescribing information, DailyMed SPL setid db8c69b0-4697-433e-98c7-b0b2d2c52a83, labeler RemedyRepack Inc. (repackager; source NDC 58151-118, ANDA084349; highlights revised 4/2025, SPL revised 10/2025). Retrieved 2026-10-21."
kind: label
drugs: [phenytoin]
source_read: "text layer, all 28 PDF pages; read in full: Highlights and contents (pp 1-2), 2 Dosage and Administration incl. 2.3, 2.4, 2.7 (pp 2-5), 7.1 interaction table (pp 12-13), 8 Use in Specific Populations 8.1-8.7 (pp 14-16), 10 Overdosage (p 16), 11 Description (p 17), 12.1, 12.3 and 12.5 (pp 17-19), 16 How Supplied and product data (pp 20, 27-28); page 17 rendered to confirm the Section 11 Tmax wording"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# DILANTIN extended phenytoin sodium capsules label (RemedyRepack SPL)

## Study
Not a study: a US prescribing information (PLR format) for DILANTIN **extended** phenytoin sodium capsules,
30 mg strength, as re-issued by the repackager RemedyRepack (NDC 70518-4325; marketing category ANDA084349).
The document covers only the extended capsule. It does not cover DILANTIN-125 suspension or DILANTIN Infatabs
(chewable tablets), which are mentioned only in passing (2.4, p 5, and the Medication Guide). There is no
PK table, no Cmax/AUC table and no population-PK content. The document is what its setid names; it is simply
the extended-capsule label, not a suspension or chewable-tablet label.

## Model and parameters
No model. Label statements (PDF page; all prose, clean text layer):
- Tmax, DILANTIN extended capsules: peak serum levels 4 to 12 h after administration (12.3, p 18; also 11, p 17).
- Section 11 (p 17) contrasts the extended capsule with **Prompt Phenytoin Sodium Capsules, USP**, peak
  1½ to 3 h. This is the only 1.5-3 h figure in the document, and it is about prompt sodium capsules.
- Plasma half-life after oral dosing: mean 22 h, range 7 to 42 h (12.3, p 18).
- Steady state: 7 to 10 days (5-7 half-lives) at 300 mg/day (12.3, p 18; 2.3, p 4).
- Salt: the free acid (suspension, Infatabs) has about 8% more phenytoin than the sodium salt (2.4, p 5).
- Metabolism CYP2C9, lesser CYP2C19; hydroxylation saturable, so a 10% dose increase can give a
  disproportionate rise (12.3, p 18).
- Elderly: clearance about 20% lower over 70 years than at 20-30 years (12.3, p 19). Sex and race: no
  significant effect.
- Dosing (2.1-2.2, pp 3-4): adults 100 mg three times daily, maintenance 100 mg three to four times daily, up
  to 200 mg three times daily; once-daily 300 mg allowed for the extended capsule only; oral load 1 g as 400,
  300, 300 mg at 2-h intervals, maintenance from 24 h. Children 5 mg/kg/day in 2-3 doses, maintenance
  4-8 mg/kg/day, maximum 300 mg/day.
```yaml
parameters:
  - {name: Tmax_ER_capsule, value: "4-12", unit: h, source: "12.3 and 11, pp 17-18", note: DILANTIN extended capsules}
  - {name: Tmax_prompt_capsule, value: "1.5-3", unit: h, source: "11, p 17", note: "Prompt Phenytoin Sodium Capsules USP, a different product; no Tmax for suspension or chewable tablet in this label"}
  - {name: t_half_oral, value: 22, unit: h, source: "12.3, p 18", note: "mean; range 7-42 h"}
  - {name: free_acid_vs_sodium_content, value: 1.08, unit: ratio, source: "2.4, p 5", note: "approximately 8% more drug in the free acid"}
  - {name: CL_elderly_vs_young, value: 0.8, unit: ratio, source: "12.3, p 19", note: "over 70 y vs 20-30 y (20% less)"}
```

## Pharmacodynamics and end points
Therapeutic range (2.3, p 4): total serum 10 to 20 mcg/mL; unbound 1 to 2 mcg/mL. Toxicity (10, p 16):
nystagmus usually from 20 mcg/mL, ataxia 30 mcg/mL, dysarthria and lethargy above 40 mcg/mL; 50 mcg/mL reported
without toxicity; adult lethal dose estimated 2 to 5 g. No ke0 or effect-site data.

## Covariates
Qualitative only: unbound fraction raised in renal or hepatic disease and hypoalbuminemia (2.5, 8.6, p 16);
CYP2C9 intermediate (*1/*3, *2/*2) and poor (*2/*3, *3/*3) metabolisers have lower clearance (8.7, 12.5,
pp 16, 19), no numeric factor; pregnancy: clearance generally rises, peaks in the third trimester and returns
weeks to months after delivery (12.3, p 19), monitor unbound levels (2.7, p 5).

## Caveats
A repackager's copy of the 30 mg extended-capsule label; the strength-specific text also describes the 100 mg
capsule. Highlights carry "Revised: 4/2025", the SPL footer "Revised: 10/2025". No quantitative PK beyond the
statements above.

## Used by stanpumpR
- `inst/extdata/antiseizureParameters.csv:6` (also `data-raw/antiseizureRegistry.R:264`): phenytoin "ka
  (suspension/chewable)" 2.0 /h, provenance "calibration", population "label Tmax 1.5-3 h", source URL this
  setid.

## Verification against stanpumpR
- `inst/extdata/antiseizureParameters.csv:6` and `R/drugs_phenytoin.R:90-94` (header: the suspension and
  chewable tablet have no population estimate, "The label gives the peak at 1.5 to 3 h", ka 2.0 /h gives a peak
  at 2.4 h), used at `R/drugs_phenytoin.R:302-303` (`fast <- list(ka_PO = 2.0 / 60, ...)`).
  The label at this URL gives 1½ to 3 h only for Prompt Phenytoin Sodium Capsules, USP (Section 11, p 17). It
  gives no Tmax for the suspension or the chewable tablet. The extended capsule's own figure is 4 to 12 h.
  **discrepancy** (source attribution): the cited document does not support a 1.5-3 h peak for the suspension or
  Infatabs. The suspension and Infatabs labels are not in the archive, so whether they state 1.5-3 h was not
  checked. The ka 2.0 /h itself, and the 2.4 h peak it is said to give, are the model's own calibration and
  cannot be checked against a label.
- `R/drugs_phenytoin.R:87-89` (ER capsule ka 0.225 /h, fixed from Jun 2020, puts a single 300 mg dose's peak at
  about 11 h, "inside the label's 4 to 12 h"). The 4 to 12 h is in this label (12.3, p 18). **matches** for the
  label range. The 11 h peak is the code's own simulation.
- Salt factor 0.92 (`R/drugs_phenytoin.R:71-80`; registry row 7 cites Odani, not this label). Label 2.4 says
  about 8% more drug in the free acid; 1/0.9199 = 1.087 (derived). **consistent**.
