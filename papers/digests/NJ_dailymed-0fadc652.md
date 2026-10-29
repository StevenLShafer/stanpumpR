---
file: NJ_dailymed-0fadc652.pdf
pmid: null
doi: null
citation: "ATENOLOL tablets, USP 50 mg (Cardinal Health 107, LLC; manufactured for Mylan Pharmaceuticals Inc. by Alphapharm; ANDA073457). DailyMed SPL setid 0fadc652-b075-46ec-ae05-beaf7d91ae13, label text revised 5/2024 (ALP:ATEN:R3), SPL revised 7/2026; retrieved 2026-10-21."
kind: label
drugs: [atenolol]
source_read: "text layer, all 18 PDF pages: Description and Clinical Pharmacology incl. Pharmacokinetics and Metabolism (pp. 1-2), Pharmacodynamics (pp. 2-3), ISIS-1 and Geriatric Pharmacology (pp. 3-4), Indications (pp. 4-5), Contraindications, Warnings and Precautions incl. Pregnancy, Nursing Mothers, Pediatric and Geriatric Use (pp. 5-10), Adverse Reactions (pp. 10-12), Overdosage (pp. 12-13), Dosage and Administration incl. renal table (pp. 13-15), How Supplied (pp. 15-16), SPL product data (pp. 17-18). Page 15 rendered for the renal dosing table, which the text layer scrambles."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Atenolol tablets, DailyMed SPL (Cardinal Health repackage of Mylan)

Old-format (non-PLR) generic label. Unit-dose 50 mg tablets (NDC 55154-0283-0), marketing start 07/20/2026, ANDA073457. The product and route match the registry record (immediate-release oral tablet).

## Study
Not a study. Describes ISIS-1 (16,027 patients with suspected MI; 8037 on atenolol) for the MI indication (p. 3).

## Model and parameters
No population model. Pharmacokinetics and Metabolism (pp. 1-2), Geriatric Pharmacology (p. 4), renal table (p. 15, rendered):

```yaml
parameters:
  - {name: fraction_absorbed_oral, value: 50, unit: "%", source: "Clinical Pharmacology p. 1", note: "approximately; remainder excreted unchanged in feces"}
  - {name: Tmax_oral, value: "2-4", unit: h, source: "Clinical Pharmacology p. 1", note: "peak blood levels"}
  - {name: protein_binding, value: "6-16", unit: "%", source: "Clinical Pharmacology p. 1"}
  - {name: urinary_recovery_24h_IV, value: ">85", unit: "% of dose", source: "Clinical Pharmacology p. 1", note: "about 50% after an oral dose"}
  - {name: t_half_oral, value: "6-7", unit: h, source: "Clinical Pharmacology p. 2", note: "unchanged by chronic dosing"}
  - {name: Tmax_IV, value: 5, unit: min, source: "Clinical Pharmacology p. 2", note: "5- to 10-fold fall over the first 7 h"}
  - {name: interpatient_variation, value: 4, unit: fold, source: "Clinical Pharmacology p. 2", note: "about"}
  - {name: t_half_CrCl_15_35, value: "16-27", unit: h, source: "Dosage table p. 15 (rendered)", note: "max 50 mg daily"}
  - {name: t_half_CrCl_lt15, value: ">27", unit: h, source: "Dosage table p. 15 (rendered)", note: "max 25 mg daily"}
  - {name: milk_plasma_ratio, value: "1.5-6.8", unit: ratio, source: "Nursing Mothers p. 9"}
```

- Little or no hepatic metabolism. The absorbed fraction is eliminated mainly by the kidney (p. 1).
- When renal function is impaired, elimination follows GFR. Significant accumulation occurs below CrCl 35 mL/min/1.73 m2 (p. 2). Peak levels are significantly higher below 30 mL/min (p. 15).
- In the elderly, total clearance is about 50% lower and the half-life markedly longer (p. 4).
- Removed by haemodialysis (p. 13).

## Pharmacodynamics and end points
- Beta-blockade, measured as reduction of exercise tachycardia: evident within 1 h of an oral dose, maximal at 2-4 h, lasting at least 24 h. After IV dosing, maximal within 5 min (p. 2).
- Duration of action is dose-related and linear in log plasma concentration. The antihypertensive effect "does not appear to be related to plasma level" (p. 2).
- Above 100 mg once daily, no added antihypertensive effect (p. 3).
- No therapeutic or toxic concentration. Survival after 5 g; one death after a possible 10 g (p. 12).

## Covariates
Renal function (CrCl, half-life and dose table) and age (clearance about 50% lower), descriptive only.

## Caveats
- Dosage (pp. 13-15):
  - Hypertension: 50 mg once daily, full effect in 1-2 weeks, up to 100 mg.
  - Angina: 50-100 mg, some patients 200 mg.
  - Acute MI: 5 mg IV over 5 min, repeated after 10 min, then 50 mg orally 10 min later and 50 mg at 12 h, then 100 mg daily or 50 mg twice daily.
  - Renal impairment: maximum 50 mg daily at CrCl 15-35 and 25 mg daily below 15. Haemodialysis patients get 25-50 mg after each dialysis.
- Pediatric safety and effectiveness not established (p. 9).

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:7 (8baa733): source_url of record atenolol__ir_tablet__oral__catalog__none__label (official_label_descriptive, proposed_unfitted, descriptive_only, plasma). Built from data-raw/antihypertensiveRegistry.R:277-281.
- Limitations text: label about 50% absorbed, Tmax 2-4 h, half-life 6-7 h, mainly unchanged renal excretion with accumulation as renal function falls.
- The drug is also listed among the beta-blockers in tests/testthat/test-antihypertensive-registry.R:523. There is no atenolol drug model.

## Verification against stanpumpR
- "about 50% absorbed": **matches** (p. 1).
- "Tmax 2-4 h": **matches** (peak blood levels at 2-4 h, p. 1).
- "half-life 6-7 h": **matches** (p. 2).
- "mainly unchanged renal excretion with accumulation as renal function falls": **matches**. Little or no hepatic metabolism (p. 1); accumulation below CrCl 35 mL/min/1.73 m2 (pp. 2, 15).
- "An effect-compartment SBP/DBP structure is published in healthy subjects": not from this label, which offers no concentration-BP relation and says the antihypertensive effect does not track plasma level (p. 2); not applicable here.
- Product and route identity: **matches**.
