---
file: NJ_dailymed-41c0e5e0.pdf
pmid: null
doi: null
citation: "Aliskiren tablets 150 mg and 300 mg (aliskiren hemifumarate), Prasco Laboratories; NDA authorized generic of NDA021985 (marketed from 03/04/2019). DailyMed SPL setid 41c0e5e0-eb26-4d9e-bcd2-d0f2cc8fc6db, label revised 5/2023; retrieved 2026-10-21."
kind: label
drugs: [aliskiren]
source_read: "text layer, all 20 PDF pages: Highlights and contents (p. 1-2), 1 Indications and 2 Dosage and Administration (p. 3), 4-5 (p. 3-4), 7 Drug Interactions and 8 Use in Specific Populations incl. 8.4-8.6 (p. 7-8), 10 Overdosage, 11 Description, 12.1-12.3 Clinical Pharmacology (p. 8-11), 14 Clinical Studies skimmed, SPL product and marketing data (p. 19-20). The 12.3 interaction figures (Figures 1-2) are images not carried in the text layer and were not used. Clean prose; no table numbers recorded."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Aliskiren tablets (Prasco, authorized generic of Tekturna), DailyMed SPL

PLR-format label; initial US approval 2007; boxed warning for fetal toxicity. Labeler Prasco; adverse-event contact Noden Pharma USA. Tablets contain 165.75 or 331.5 mg aliskiren hemifumarate, equal to 150 or 300 mg aliskiren (base MW 551.8, hemifumarate 609.8) (section 11, p. 8-9).

## Study
Not a study.

## Model and parameters
No population model. Section 12.3 (PDF p. 9-11):

```yaml
parameters:
  - {name: F_oral, value: 0.025, unit: fraction, source: "12.3 (p. 9)", note: "about 2.5%, poorly absorbed"}
  - {name: t_half_accumulation, value: 24, unit: h, source: "12.3 (p. 9)", note: "approximate accumulation half-life"}
  - {name: time_to_steady_state, value: "7-8", unit: days, source: "12.3 (p. 9)"}
  - {name: tmax, value: "1-3", unit: h, source: "12.3 (p. 9)"}
  - {name: high_fat_meal_AUC_change, value: -71, unit: "%", source: "12.3 (p. 9)", note: "mean AUC"}
  - {name: high_fat_meal_Cmax_change, value: -85, unit: "%", source: "12.3 (p. 9)", note: "mean Cmax"}
  - {name: fraction_absorbed_dose_in_urine_unchanged, value: 0.25, unit: fraction, source: "12.3 (p. 9)", note: "about one-fourth of the absorbed dose"}
```

- Metabolism: extent unknown; CYP3A4 the major enzyme in vitro; no CYP inhibition or CYP3A4 induction. P-gp the major efflux system in preclinical studies.
- Interactions (text): avoid cyclosporine and itraconazole; with furosemide 60 mg/day in heart failure, aliskiren 300 mg/day reduced furosemide AUC 17%, Cmax 27%, 24 h urinary furosemide 29% (p. 10).
- Specific populations (p. 10-11): renal impairment, no consistent relation of exposure to severity; ESRD on hemodialysis not clinically different; poorly dialysed; hepatic impairment no significant effect; AUC increased at 65 y and older (no dose change); pediatric 6-17 y (2 and 6 mg/kg, n = 39) similar to adults, exposure fell with body weight.

## Pharmacodynamics and end points
- PRA reduced 50-80% in placebo-controlled trials; not dose related and not correlated with BP reduction (12.2, p. 9).
- Dosing (2.1, p. 3): 150 mg once daily, up to 300 mg; doses above 300 mg gave no more BP effect but more diarrhoea; 85-90% of effect by 2 weeks. Establish a routine pattern with meals (2.2).
- No therapeutic concentration range.

## Covariates
Descriptive only (age, weight in children, food).

## Caveats
Contraindicated with ARBs or ACEIs in diabetes and in children under 2 y; avoid dual RAAS block with CrCl < 60 mL/min; not established below CrCl 30 mL/min (8.6). Pediatric indication limited to 6 y and older weighing 50 kg or more.

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:3 (8baa733): record aliskiren__ir_tablet__oral__catalog__none__label (official_label_descriptive, descriptive_only, PD proposed_unfitted, BP simulation disabled); source_url this SPL; limitations text quotes label PK and the PRA statement.

## Verification against stanpumpR
- "Label F about 2.5%, Tmax 1-3 h, accumulation half-life about 24 h": 12.3 p. 9. **matches**.
- "exposure sharply reduced by a high-fat meal": AUC -71%, Cmax -85% (12.3). **matches**.
- "the label states plasma renin activity reduction does not correlate with BP reduction": 12.2. **matches**.
- The human biomarker model with adaptive renin feedback mentioned in the same field is not in this label (another source). Nothing numeric is taken from the label (descriptive_only).
