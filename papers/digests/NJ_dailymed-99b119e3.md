---
file: NJ_dailymed-99b119e3.pdf
pmid: null
doi: null
citation: "Spironolactone tablets USP 25/50/100 mg (generic, ANDA040750; manufactured by Oxford Pharmaceuticals, repackaged and relabeled by Proficient Rx LP) prescribing information; DailyMed SPL setid 99b119e3-537a-464c-9376-4219bf283753, highlights revised 2/2025."
kind: label
drugs: [spironolactone]
source_read: "text layer, all 23 pages: Highlights and contents (PDF p1-3), Dosage and Administration 2.1-2.5 (p4-5), Warnings 5.1-5.4 (p5-6), Use in Specific Populations 8.1-8.7 (p9-10), Clinical Pharmacology 12.1-12.3 (p11-12), Clinical Studies 14.1-14.2 (p14-15), How Supplied (p16-17), carton panels and SPL data (p17-23)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Spironolactone tablets (generic, DailyMed SPL)

## Study
This is a **generic** spironolactone tablet (Oxford Pharmaceuticals, ANDA040750, Proficient Rx repackage) in PLR format, not the Aldactone brand. Section 12.3 and the dosing text are the same as the Aldactone label (`NJ_label-aldactone_012151_2025-11-17`). The tablets are white coated tablets (25, 50, 100 mg; How Supplied, p16).

## Model and parameters
No model. Section 12.3 (p11-12):

```yaml
parameters:
  - {name: tmax_spironolactone, value: 2.6, unit: h, source: "12.3 p11", note: "mean, healthy volunteers"}
  - {name: tmax_canrenone, value: 4.3, unit: h, source: "12.3 p11", note: mean}
  - {name: food_effect_AUC, value: 95.4, unit: "% increase", source: "12.3 p11", note: "spironolactone AUC with food"}
  - {name: protein_binding, value: ">90", unit: "%", source: "12.3 p11", note: "spironolactone and metabolites"}
  - {name: t_half_spironolactone, value: 1.4, unit: h, source: "12.3 p12", note: mean}
  - {name: t_half_canrenone, value: 16.5, unit: h, source: "12.3 p12", note: mean}
  - {name: t_half_TMS, value: 13.8, unit: h, source: "12.3 p12", note: "7-alpha-thiomethylspirolactone"}
  - {name: t_half_HTMS, value: 15, unit: h, source: "12.3 p12", note: "6-beta-hydroxy-7-alpha-thiomethylspirolactone"}
```

- Metabolites fall into two groups: those that lose sulfur (canrenone) and those that keep it (TMS, HTMS).
- TMS and 7-alpha-thiospirolactone are about one third as potent as spironolactone against fludrocortisone. Their concentrations were not measured.
- Excretion is mainly urinary, then biliary.
- The effects of age, sex, race and renal impairment on the PK have not been studied. The half-life is longer in cirrhotic ascites (12.3, p12).

## Pharmacodynamics and end points
- Hypertension: start 25-100 mg/day, titrate at 2-week intervals; above 100 mg/day there is generally no further BP fall (2.3, p5; 14.2, p15).
- The BP dose response is not well characterised (14.2).
- Heart failure (2.2): 25 mg daily if K+ is 5.0 mEq/L or less and eGFR above 50; 25 mg every other day if eGFR is 30-50.
- Edema: 100 mg/day, range 25-200 (2.4).
- Primary hyperaldosteronism: 100-400 mg/day before surgery (2.5).

## Covariates
- Renal impairment raises the hyperkalemia risk (8.6).
- Hepatic: start in hospital, because clearance of the drug and its metabolites is reduced (8.7).
- Paediatric safety and effectiveness are not established (8.4).
- Food: take consistently with respect to meals (2.1).

## Caveats
This is a generic SPL. The PK is summary text without n. No canrenone clearance or volume is given.

## Used by stanpumpR
`inst/extdata/antihypertensiveRegistry.csv:116`: the `source_url` of `spironolactone__tablet__oral__catalog__none__label` (catalog only).

## Verification against stanpumpR
- **"The tablet is a different input from the research suspension"**: consistent. This is a tablet label, and the suspension is a separate product.
- **"The label names active metabolites beyond canrenone"**: matches. Section 12.1 refers to "active metabolites", and 12.3 names TMS and HTMS with their half-lives.
- **"Food alters exposure"**: matches (+95.4% AUC, 12.3).
- **"Chronic BP is missing"**: consistent. Section 14.2 says the dose response is not well characterised and gives no concentration-BP relation.
- **analyte** "spironolactone and canrenone": matches; the label gives Tmax for both.
- **Source type** `official_label`: the linked document is a generic repackager's SPL, not the Aldactone NDA label. The text is the same, so there is no numeric consequence.
