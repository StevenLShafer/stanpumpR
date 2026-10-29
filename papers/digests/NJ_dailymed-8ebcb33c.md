---
file: NJ_dailymed-8ebcb33c.pdf
pmid: null
doi: null
citation: "PROCARDIA XL (nifedipine) extended release tablets 30 and 60 mg, GITS (Pfizer Laboratories Div Pfizer Inc; NDA019684). DailyMed SPL setid 8ebcb33c-f43b-4b36-9f94-9774b2a59e06, label LAB-0176-14.0 revised September 2026, SPL revised 9/2026; retrieved 2026-10-21."
kind: label
drugs: [nifedipine]
source_read: "text layer, all 18 PDF pages: Description and System Components and Performance (pp. 1-2), Clinical Pharmacology incl. Mechanism, Pharmacokinetics and Metabolism, Hemodynamics, Electrophysiology (pp. 2-4), Indications (pp. 4-6), Contraindications and Warnings (pp. 6-7), Precautions incl. Drug Interactions, Grapefruit, Pregnancy, Lactation, Pediatric and Geriatric Use (pp. 7-10), Adverse Experiences (pp. 10-12), Overdosage (p. 13), Dosage and Administration (pp. 13-14), How Supplied (pp. 14-15), SPL product data (pp. 15-18). Clean prose; nothing rendered, no PK table."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Procardia XL (nifedipine GITS), DailyMed SPL (Pfizer)

Brand osmotic extended-release (GITS) tablet, old-format label. Marketing start 09/06/1989. Each tablet contains 33 or 66 mg nifedipine to deliver 30 or 60 mg (p. 1). Product and route match the registry record (osmotic ER tablet, oral).

## Study
Not a study.

## Model and parameters
No population model. Description (pp. 1-2) and Pharmacokinetics and Metabolism (pp. 3-4):

```yaml
parameters:
  - {name: release_design, value: "approximately constant rate over 24 h", unit: "", source: "System Components p. 2", note: "osmotic push-pull core; independent of pH or GI motility; rate falls to zero as the gradient fails"}
  - {name: time_to_plateau, value: 6, unit: h, source: "Pharmacokinetics p. 3", note: "approximately, after the first dose"}
  - {name: F_relative_to_IR_capsule, value: 86, unit: "%", source: "Pharmacokinetics p. 3", note: "at steady state"}
  - {name: fluctuation_IR_tid_vs_XL, value: 4, unit: fold, source: "Pharmacokinetics p. 3", note: "peak:trough index about four-fold higher with IR capsules t.i.d."}
  - {name: linear_dose_range, value: "30-180", unit: mg, source: "Pharmacokinetics p. 3"}
  - {name: t_half_elimination, value: 2, unit: h, source: "Pharmacokinetics p. 3", note: approximately}
  - {name: urinary_metabolites, value: "60-80", unit: "% of dose", source: "Pharmacokinetics p. 3", note: "inactive; < 0.1% unchanged"}
  - {name: protein_binding, value: "92-98", unit: "%", source: "Pharmacokinetics p. 4"}
  - {name: CL_elderly_IV, value: -33, unit: "% vs young", source: "Pharmacokinetics p. 4"}
  - {name: steady_state, value: "second day of dosing", unit: "", source: "Dosage p. 13"}
```

- Completely absorbed (p. 3).
- Food slightly alters the early absorption rate but not its extent. Short bowel may lower levels (p. 3).
- No dose dumping in more than 150 subjects (p. 3).
- Hepatic CYP3A4 metabolism (p. 9). Renal impairment and dialysis do not significantly alter PK; cirrhosis raises half-life and bioavailability (pp. 3-4).
- Interactions (p. 9): cimetidine raises Cmax 80% and AUC 74%; phenytoin lowers exposure about 70%; grapefruit juice about doubles AUC and Cmax.

## Pharmacodynamics and end points
- BP falls through reduced peripheral resistance. In normotensive volunteers the fall is usually under 5-10 mm Hg systolic. With XL there is no significant change in heart rate (p. 4).
- No concentration-effect relation or therapeutic range is given.

## Covariates
Age (clearance down 33%, higher AUC; p. 4, p. 10), hepatic impairment, and CYP3A4 inducers and inhibitors, all descriptive.

## Caveats
- Dosage (pp. 13-14): start at 30 or 60 mg once daily; titrate over 7-14 days; above 120 mg not recommended. Angina patients on 10 mg IR capsules t.i.d. switch to 30 mg XL once daily. Swallow whole.
- The empty tablet shell is passed in the stool. GI obstruction and bezoars have been reported (p. 7).
- Washout of at least 36 h is advised before high-dose fentanyl anaesthesia in patients also on beta-blockers (p. 6).

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:95 (8baa733): source_url of record nifedipine__er_gits_tablet__oral__catalog__none__label (official_label_descriptive, proposed_unfitted, descriptive_only). Built from data-raw/antihypertensiveRegistry.R:219-223.
- Limitations text: a 24-hour release-controlled input over an intrinsic half-life of about 2 h; immediate-release, sustained-release and GITS inputs are not interchangeable.
- The drug is also listed among the calcium-channel blockers in tests/testthat/test-antihypertensive-registry.R:521. There is no nifedipine drug model.

## Verification against stanpumpR
- "24-hour release-controlled input": **matches** (drug delivered at an approximately constant rate over 24 h, p. 2).
- "intrinsic half-life of about 2 h": **matches** (elimination half-life approximately two hours, p. 3).
- "IR, SR and GITS inputs not interchangeable": **consistent** with the label. It gives a four-fold higher fluctuation with IR capsules, F 86% relative to capsules, and the instruction to make sure the ER form was prescribed (pp. 3, 14). The label says nothing about SR products.
- "The effect-site BP numbers in the parameter audit are a secondary transcription": not from this label (no BP model here); not applicable.
- Product and route identity: **matches**.
