---
file: NJ_dailymed-2b70cf0c.pdf
pmid: null
doi: null
citation: "Padagis US LLC. Amiloride HCl Tablets, USP, 5 mg (amiloride hydrochloride tablet). DailyMed SPL, setid 2b70cf0c-45be-428f-b396-5001ed4e30fc; NDA authorized generic, NDA018200; revised 5/2023 (Rev 05-23). https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=2b70cf0c-45be-428f-b396-5001ed4e30fc"
kind: label
drugs: [amiloride]
source_read: "text layer, all 11 PDF pages (11 form-feed chunks = 11 pages). An old-format label with no numbered sections. Read: Description and Clinical Pharmacology (pp 1-2), Indications (p 2), Contraindications (pp 2-3), Warnings (pp 3-4), Precautions including Drug Interactions, Pregnancy, Nursing, Pediatric and Geriatric Use (pp 4-6), Adverse Reactions (pp 6-8), Overdosage (p 8), Dosage and Administration (pp 8-9), How Supplied and the SPL product data (pp 9-11). Page 1 rendered for identity. The PK passage on p 2 is clean prose in the text layer."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Padagis 2023: amiloride HCl 5 mg tablet label (DailyMed)

## Study
A US prescribing label (DailyMed SPL), not a study. Product: amiloride HCl tablets USP, 5 mg of anhydrous amiloride
HCl, from Padagis US LLC. It is marketed as an NDA authorized generic under NDA018200, marketing start 04/06/2009,
revised 5/2023 (pp 9-11). The document is what its URL says (page 1 rendered). Salt: the dihydrate monohydrochloride,
MW 302.12, pKa 8.7 (p 1).

The label reports no study population, sample size, assay or pharmacokinetic table.

## Model and parameters
There is no model. The descriptive PK statements are all on Clinical Pharmacology, p 2:
- Onset of action within 2 h of an oral dose.
- Peak effect on electrolyte excretion at 6-10 h; the effect lasts about 24 h.
- Peak plasma levels at 3-4 h.
- Plasma half-life 6-9 h.
- The electrolyte effect increases with single doses up to about 15 mg.
- Not metabolised by the liver; excreted unchanged by the kidneys.
- Of a 20 mg dose, about 50% appears in urine and 40% in stool within 72 h. The label does not say whether the
  stool fraction is unabsorbed drug.
- Little effect on GFR or renal blood flow.
- Accumulation is not expected in hepatic dysfunction but can occur in hepatorenal syndrome.

The label does not report bioavailability, volume, clearance, protein binding or plasma concentrations.

```yaml
parameters:
  - {name: tmax, value: "3-4", unit: h, source: "Clinical Pharmacology p 2", note: "peak plasma levels; descriptive range"}
  - {name: t_half, value: "6-9", unit: h, source: "Clinical Pharmacology p 2", note: "plasma half-life; descriptive range"}
  - {name: urine_recovery_20mg, value: 0.50, unit: fraction of dose, source: "Clinical Pharmacology p 2", note: "about 50% in urine within 72 h"}
  - {name: stool_recovery_20mg, value: 0.40, unit: fraction of dose, source: "Clinical Pharmacology p 2", note: "about 40% in stool within 72 h"}
  - {name: onset_effect, value: 2, unit: h, source: "Clinical Pharmacology p 2", note: "begins to act within 2 h"}
  - {name: peak_electrolyte_effect, value: "6-10", unit: h, source: "Clinical Pharmacology p 2", note: "lasts about 24 h"}
```

## Pharmacodynamics and end points
- **End point:** urinary electrolyte excretion. Natriuresis is weak compared with thiazides; potassium is conserved
  by inhibiting distal sodium reabsorption.
- **Time course:** effect peaks 6-10 h after a dose, later than the 3-4 h plasma peak, and lasts about 24 h (p 2).
- **Dose response:** the effect rises with single doses up to about 15 mg.
- **Blood pressure:** antihypertensive effect is weak; there is little additive effect over a thiazide (p 2).
- **Hyperkalemia** (serum K > 5.5 mEq/L): about 10% when amiloride is used alone and about 1-2% with a thiazide in
  uncomplicated patients (pp 2-3). The risk is higher in renal impairment, diabetes and the elderly. Treat actively
  if K exceeds 6.5 mEq/L (p 4).
- No concentration-effect relationship, EC50 or ke0 is given.

## Covariates
- **Renal function:** contraindicated in anuria, acute or chronic renal insufficiency, and diabetic nephropathy.
  Patients with BUN > 30 mg/dL or creatinine > 1.5 mg/dL need careful, frequent electrolyte monitoring (p 3).
- **Interactions:** ACE inhibitors, ARBs, cyclosporine and tacrolimus raise the hyperkalemia risk; NSAIDs and
  lithium are also listed (pp 4-5).
- **Elderly:** dose cautiously; the drug is substantially renally excreted (p 6).
- **Pediatric:** safety and effectiveness not established.
- No quantitative covariate effects are given.

## Caveats
- All values are descriptive ranges with no n, cohort or estimator.
- The text says "excreted unchanged by the kidneys", but the 20 mg mass balance puts only about 50% in urine (40%
  in stool). Renal excretion is the elimination route for absorbed drug; it is not a statement of near-complete
  urinary recovery.
- It is not known whether the drug is dialysable (p 8). Give with food (p 8).
- **Dosing:** 5 mg daily, which may be raised to 10 mg; for documented persistent hypokalemia, 15 then 20 mg with
  monitoring (pp 8-9).

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:4`, row `amiloride__ir_tablet__oral__catalog__none__label`, built by
  `data-raw/antihypertensiveRegistry.R:346-350`.
  - Fields: oral immediate-release tablet; analyte amiloride; matrix plasma; pk_evidence
    official_label_descriptive; execution_status descriptive_only; bp_simulation_enabled FALSE; source_type
    official_label; source_url this SPL.
  - limitations: "Label Tmax 3-4 h, half-life 6-9 h, unchanged renal elimination. Renal PK, the delayed natriuresis
    and BP are all missing."
- `tests/testthat/test-antihypertensive-registry.R:528` lists amiloride among the diuretics that must be in the
  registry, with no curve.

## Verification against stanpumpR
- **Tmax 3-4 h: matches.** p 2, "Peak plasma levels are obtained in 3 to 4 hours".
- **Half-life 6-9 h: matches.** p 2.
- **"unchanged renal elimination": matches** the label's statement that the drug is not metabolised and is excreted
  unchanged by the kidneys (p 2). The same paragraph also gives about 40% of a 20 mg dose in stool, so urinary
  recovery is only about 50% of the dose. This is a nuance, not a discrepancy.
- **"delayed natriuresis": matches.** The electrolyte effect peaks at 6-10 h, after the 3-4 h plasma peak (p 2).
- **Route, formulation and matrix: match.** Oral 5 mg immediate-release tablet; the label's PK statements refer to
  plasma.
- **Descriptive-only status with no BP simulation: consistent.** The label provides no model, no concentrations and
  no BP dose-response.
