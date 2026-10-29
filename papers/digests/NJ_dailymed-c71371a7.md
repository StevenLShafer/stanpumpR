---
file: NJ_dailymed-c71371a7.pdf
pmid: ""
doi: ""
citation: "Furosemide Injection, USP 10 mg/mL (Baxter Healthcare Corporation; ANDA202747). DailyMed SPL setid c71371a7-75c5-45b8-b762-8d782d4c71bc, label Rev. 2025-06-30 (SPL revised 6/2025); retrieved 2026-10-21."
kind: label
drugs: [furosemide]   # not a stanpumpR drug model; antihypertensive registry catalogue record only
source_read: "text layer, all 22 PDF pages: Boxed warning and Description (p. 1), Clinical Pharmacology incl. Geriatric and Pediatric Population (p. 2), Indications, Contraindications, Warnings (p. 3), Precautions incl. Pregnancy, Pediatric and Geriatric Use (p. 4-7), Adverse Reactions (p. 8-9), Overdosage (p. 9), Dosage and Administration (p. 10-11), How Supplied (p. 11), carton/vial panels (p. 12-17), SPL product data (p. 18-22). Clean prose; no tables needing rendering."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Furosemide Injection USP 10 mg/mL (Baxter), DailyMed SPL

Old-format label (no numbered sections, no 12.x subsections). Generic: marketing category ANDA, ANDA202747, marketed from 02/06/2014; manufactured by Baxter Pharmaceuticals India (p. 18-22). Vials 20 mg/2 mL, 40 mg/4 mL, 100 mg/10 mL, single-dose amber; for IV or IM use (panels p. 12-17), although the SPL product records list the route as intravenous only.

## Study
Not a study. Composition per mL: furosemide 10 mg, sodium hydroxide 1.34 mg, sodium chloride, pH 8.0-9.3 (p. 1).

## Model and parameters
No population model. Label pharmacokinetic statements (Clinical Pharmacology, p. 2):

```yaml
parameters:
  - {name: protein_binding, value: "91-99", unit: "%", source: "Clinical Pharmacology p. 2", note: "at 1-400 mcg/mL in healthy subjects; unbound 2.3-4.1% at therapeutic concentrations"}
  - {name: F_tablet, value: 64, unit: "%", source: "Clinical Pharmacology p. 2", note: "relative to IV, fasted normal men"}
  - {name: F_oral_solution, value: 60, unit: "%", source: "Clinical Pharmacology p. 2", note: "relative to IV"}
  - {name: absorption_time_solution, value: 50, unit: min, source: "Clinical Pharmacology p. 2", note: "printed as absorption time; not defined further"}
  - {name: absorption_time_tablet, value: 87, unit: min, source: "Clinical Pharmacology p. 2"}
  - {name: t_half_terminal, value: 2, unit: h, source: "Clinical Pharmacology p. 2", note: "approximately"}
  - {name: CL_preterm_PNA_lt30d, value: 8.9, unit: mL/h/kg, source: "Pediatric Population p. 2", note: "median (2.1-21.2); 51 premature infants, GA 23-29 wk"}
  - {name: CL_preterm_PNA_ge30d, value: 25.3, unit: mL/h/kg, source: "Pediatric Population p. 2", note: "median (8.3-44.2)"}
  - {name: F_enteral_preterm, value: 79, unit: "%", source: "Pediatric Population p. 2", note: "around 79% relative to IV"}
```

- Metabolism: furosemide glucuronide the only or major product; predominantly excreted unchanged in urine; more excreted in urine after IV than oral. Peak plasma concentration rises with dose, time to peak independent of dose. Hemodialysis does not accelerate elimination (p. 9).
- Elderly (60-70 y vs 20-35 y men): renal clearance after IV significantly smaller; albumin binding may be reduced; initial diuretic effect smaller (p. 2).

## Pharmacodynamics and end points
- Diuresis begins within 5 min of IV (later after IM), peaks within the first half hour, lasts about 2 h (p. 2).
- No therapeutic or toxic concentration range: the concentration associated with toxicity or death is not known (p. 9). Preterm infants < 31 wk post-conceptual age given > 1 mg/kg/24 h may reach potentially ototoxic levels (p. 3).
- Ototoxicity linked to rapid injection, severe renal impairment, high doses, hypoproteinaemia, aminoglycosides (p. 3).

## Covariates
Postnatal age and weight on clearance in preterm infants; age on renal clearance (descriptive only).

## Caveats
- Dosage (p. 10-11): adults, edema 20-40 mg IV or IM, IV over 1-2 min, repeat or raise by 20 mg no sooner than 2 h; high-dose therapy as a controlled infusion not exceeding 4 mg/min, pH above 5.5; acute pulmonary edema 40 mg IV over 1-2 min, 80 mg if no response within 1 h. Pediatric 1 mg/kg IV or IM, increments of 1 mg/kg no sooner than 2 h, not above 6 mg/kg; premature infants not above 1 mg/kg/day.
- Incompatible with acidic solutions in the same line (labetalol, ciprofloxacin, amrinone, milrinone) (p. 10).
- Indication is edema and adjunctive acute pulmonary edema; this injection label carries no hypertension indication.

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:60 (8baa733): source_url of record furosemide__injection__iv__catalog__none__label (status official_label_descriptive, readiness descriptive_only, analyte furosemide, matrix "plasma and urine"); also data-raw/antihypertensiveRegistry.R:331-335. The record is referred to by model_id in tests/testthat/test-antihypertensive-registry.R:169-170 (formulation and route as separate records), docs/antihypertensives.md:30 and inst/help/models/antihypertensives.md:19.

## Verification against stanpumpR
- Product and route identity (furosemide injection, IV): **matches**; the label also allows IM, which the record (route "iv") does not mention.
- The record's notes (oral bioavailability and the Weibull input do not apply to the injection; the published IV PD record is an indirect-response model of urinary excretion, not BP) are about other sources; the label does not contradict them and states oral F relative to IV (64% / 60%), so oral F indeed is not an injection parameter.
- No parameter is taken from the label (descriptive_only): nothing numeric to check.
- Note: the registry cites this Baxter ANDA SPL, whereas NJ_label-furosemideinjection_018579_2024-10-03 is the American Regent NDA 018579 label; their clinical pharmacology text is the same.
