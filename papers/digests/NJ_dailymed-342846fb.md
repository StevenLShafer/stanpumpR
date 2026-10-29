---
file: NJ_dailymed-342846fb.pdf
pmid: null
doi: null
citation: "Northstar Rx LLC (manufactured by Dr. Reddy's Laboratories; relabelled by A-S Medication Solutions). Metoprolol succinate extended-release tablets USP, 25 mg (product NDC 50090-7117), ANDA090617. DailyMed SPL setid 342846fb-d42f-4d96-a8b5-7bcf1dcaef2f; label text issued 06/2023, SPL revised 3/2024; archived 2026-10-21."
kind: label
drugs: [metoprolol succinate]
source_read: "text layer, all 23 PDF pages (PLR format; form-feed chunks equal PDF pages, one trailing): Highlights p1-2; contents p2-3; Indications p3-4; Dosage and Administration 2.1-2.4 p4-5; Warnings 5.1-5.10 p5-7; Adverse Reactions p7-9; Drug Interactions 7.1-7.3 p9; Use in Specific Populations 8.1-8.7 p9-12; Overdosage p12-13; Description p13; Clinical Pharmacology 12.1-12.5 p13-16; Clinical Studies 14.1-14.3 p17-19; How Supplied, counselling, SPL data p20-23. PDF p15 (12.3) checked on a rendered image."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: partly-verified
---

# Metoprolol succinate ER US generic label (Dr. Reddy's / Northstar, 2023)

## Study
Not a study: a generic US label for metoprolol succinate extended-release tablets (a multiple-unit pellet system;
23.75 and 47.5 mg of succinate equal 25 and 50 mg of metoprolol tartrate, PDF p13), relabelled by A-S Medication
Solutions; the SPL product is the 25 mg tablet (Basis of Strength: metoprolol tartrate 25 mg, PDF p22). Initial US
approval 1992. Indications: hypertension, angina, heart failure (NYHA II-III) (PDF p3-4).

## Model and parameters
No model. Section 12.3 (PDF p15-16):

```yaml
parameters:
  - {name: F_rel_ER_vs_IR, value: 0.77, unit: fraction, source: "12.3 Absorption, PDF p15", note: "steady state, 50-400 mg once daily, relative to conventional metoprolol"}
  - {name: Cmax_ratio_ER_vs_IR, value: "0.25-0.50", unit: ratio, source: "12.3 Absorption, PDF p15", note: "ER peak as a fraction of the IR peak at the same daily dose"}
  - {name: first_pass_IR, value: 0.50, unit: fraction, source: "12.3 Absorption, PDF p15", note: "about; IR oral peaks about 50% of IV"}
  - {name: thalf, value: "3-7", unit: h, source: "12.3 Elimination, PDF p15", note: "plasma half-life range"}
  - {name: protein_binding, value: 0.12, unit: fraction, source: "12.3 Distribution, PDF p15", note: "about; to albumin"}
  - {name: CSF_to_plasma, value: 0.78, unit: ratio, source: "12.3 Distribution, PDF p15", note: "reported"}
  - {name: urine_unchanged_oral, value: "<0.05", unit: fraction of dose, source: "12.3 Elimination, PDF p15", note: "IV: about 0.10"}
```
No volume, clearance, tmax or absorption rate. F rises with dose, not proportionally; food has no significant effect on
the ER form (PDF p15). CL/F rises linearly with body weight; age, sex and race no significant effect; paediatric PK
(120 patients 6-17 y, 12.5-200 mg) similar to adults (PDF p16).

## Pharmacodynamics and end points
- Exercise heart rate: beta1 blockade of 30-80% of maximal effect (about 8-23% fall in exercise HR) corresponds to
  plasma metoprolol 30-540 nmol/L; beta1 selectivity diminishes above 300 nmol/L (12.2, PDF p14). The concentration-
  exercise-HR relation is independent of formulation.
- ER once daily gives total 24-h beta1 blockade comparable to IR 1-4 times daily at 100-400 mg; at 50 mg once daily ER
  gives more (12.2, PDF p14).
- Hypertension (14.1, PDF p17): 25, 100 or 400 mg once daily lowered sitting BP by 6-8/4-7 mmHg placebo-corrected at
  24 h after 9 weeks (n = 1,092 trial incl. felodipine arms). The label states there is no consistent relationship of
  antihypertensive activity to plasma concentration, so dosing is by titration. Maximum effect of a dose apparent after
  1 week (2.1, PDF p4).
- Paediatric (8.4, PDF p11): 144 patients 6-16 y, 0.2/1/2 mg/kg; primary SBP dose-response end point not met.
- MERIT-HF (14.3, PDF p18-19): all-cause mortality RR 0.66 (34% reduction), mean final dose 159 mg.

## Covariates
CYP2D6: poor metabolisers (about 8% of Caucasians, 2% of most other populations) and strong inhibitors (quinidine,
fluoxetine, paroxetine, propafenone) raise concentrations several-fold or double them; quinidine tripled S-metoprolol
and doubled the half-life (7.2 PDF p9; 12.3, 12.5 PDF p16). Hepatic impairment: likely raises levels (8.6, PDF p12).
Renal failure: no clinically significant change (8.7, PDF p12).

Dosing (2.1-2.3, PDF p4-5): hypertension 25-100 mg once daily, weekly titration, not studied above 400 mg; paediatric
>= 6 y 1 mg/kg once daily (max initial 50 mg), not studied above 2 mg/kg or 200 mg; angina 100 mg once daily; heart
failure 25 mg (12.5 mg if more severe) doubled every 2 weeks to 200 mg.

## Caveats
Generic label with rounded summary values; no study n for the PK descriptors. The SPL lists only the 25 mg product.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:84` (`data-raw/antihypertensiveRegistry.R:267-271`):
  `metoprolol__succinate_er_tablet__oral__catalog__none__label`, plasma, `official_label_descriptive`,
  `proposed_unfitted`, `descriptive_only`, BP simulation off. Limitations: a published two-compartment IR/ER population
  model with formulation-specific absorption exists, with exercise-heart-rate Emax; its numbers are in an unverified
  supplement; resting BP coefficients are missing. (The fitted record is the separate row 85, `...kim2019`.)

## Verification against stanpumpR
- "Resting BP coefficients are missing": **matches** (the label gives trial mean BP falls and explicitly no consistent
  concentration-BP relation; its only concentration-effect statement is for exercise heart rate, 30-540 nmol/L).
- The existence and form of a published IR/ER population model: **cannot verify** from this label (it names no model).
- Analyte metoprolol, plasma, oral ER tablet: **matches**.
- Note (no verdict): the row's limitations describe the research model rather than the label; the label's own PK
  descriptors (half-life 3-7 h, ER F 77% of IR, IR first-pass about 50%) are not quoted in the registry.
