---
file: NJ_dailymed-fb5ef3ba.pdf
pmid: null
doi: null
citation: "Mullan Pharmaceutical Inc. (manufactured by Gland Pharma). Labetalol Hydrochloride Injection, USP, 5 mg/mL (100 mg/20 mL and 200 mg/40 mL multiple-dose vials), ANDA090699. DailyMed SPL setid fb5ef3ba-0a7e-4f41-9547-d734983da0d3; text revised 06/2025, SPL revised 12/2025; archived 2026-10-21."
kind: label
drugs: [labetalol]
source_read: "text layer, all 17 PDF pages (old non-PLR format): Description p1; Clinical Pharmacology incl. Pharmacodynamics p1-3 and Pharmacokinetics and Metabolism p3; Indications, Contraindications, Warnings p3-5; Precautions incl. Pregnancy, Nursing Mothers, Pediatric Use p5-9; Adverse Reactions p9-10; Overdosage p10; Dosage and Administration p11-13; How Supplied p13; carton and SPL data p13-17. PDF p3 (Pharmacokinetics and Metabolism) checked on rendered image."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Labetalol hydrochloride injection US label (Mullan generic, 2025)

## Study
Not a study: a generic US label for labetalol HCl injection, 5 mg/mL, for control of blood pressure in severe
hypertension (PDF p3). Racemate, two diastereoisomeric pairs; dilevalol (R,R') is 25%; MW 364.9 as HCl (PDF p1).
Strength is labelled as the hydrochloride (PDF p1, SPL Basis of Strength p15-16).

## Model and parameters
No model. Pharmacokinetics and Metabolism (PDF p3):

```yaml
parameters:
  - {name: thalf_iv, value: 5.5, unit: h, source: "Pharmacokinetics and Metabolism, PDF p3", note: "about; after IV infusion"}
  - {name: CL_iv, value: 33, unit: mL/min/kg, source: "Pharmacokinetics and Metabolism, PDF p3", note: "approximately; total body clearance"}
  - {name: thalf_oral, value: "6-8", unit: h, source: "Pharmacokinetics and Metabolism, PDF p3", note: "plasma half-life after oral dosing"}
  - {name: protein_binding, value: 0.50, unit: fraction, source: "PDF p3", note: "approximately"}
  - {name: urinary_recovery_24h, value: "0.55-0.60", unit: fraction of dose, source: "PDF p3", note: "as conjugates or unchanged"}
```
No volumes, no distribution half-life. Half-life not altered in hepatic or renal impairment; oral bioavailability up in
hepatic impairment; dialysis removes < 1% (PDF p3). The D&A section quotes a half-life of 5 to 8 h (PDF p12).
(derived: CL x t1/2 / ln2 = 33 x 330 / 0.693 = 15.7 L/kg, an implied Varea; the label gives no volume.)

## Pharmacodynamics and end points
- Alpha:beta blockade about 1:3 oral and 1:7 IV (PDF p1).
- Severe hypertension: 0.25 mg/kg supine gave -11/7 mmHg; further 0.5 mg/kg every 15 min to 1.75 mg/kg (some 3.25
  mg/kg) gave dose-related falls; maximal effect of each dose within 5 min; BP back towards baseline over 16-18 h on
  average (PDF p2). Continuous infusion, mean 136 mg (27-300) over about 2 h 39 min: -60/35 mmHg (PDF p3).
- Posture: BP falls more standing; symptomatic postural hypotension 58% if tilted or upright within 3 h (PDF p2, p6, p9).
- No concentration-effect relation or therapeutic range given.

## Covariates
Hepatic impairment (first-pass for oral), elderly not addressed for IV; halothane synergy; cimetidine raises oral F
(PDF p7).

## Caveats
Old-format generic label; PK figures are rounded summary values with no study or n.

Dosing (PDF p11-12): 20 mg (0.25 mg/kg for 80 kg) over 2 min, then 40 or 80 mg every 10 min to a total of 300 mg; or
infusion of 200 mg in 160 mL (1 mg/mL) at 2 mL/min = 2 mg/min, usual effective dose 50-200 mg, up to 300 mg; then
oral 200 mg, then 200 or 400 mg in 6-12 h.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:73` (`data-raw/antihypertensiveRegistry.R:438`):
  `labetalol__injection__iv__catalog__none__label`, IV injection, plasma, `official_label_descriptive`,
  `proposed_unfitted`, `descriptive_only`, BP simulation off; limitations: "Label half-life about 5.5 h and clearance
  about 33 mL/min/kg, with a response within about 5 min, but no volumes and no fitted concentration-BP coefficients.
  Trial mean BP reductions by regimen are not a PD model. Posture is an essential response covariate."
- Linked from `inst/extdata/vasoactiveRegistry.csv:9` (Kanto row, `antihypertensive_model_id`).

## Verification against stanpumpR
- Half-life about 5.5 h, clearance about 33 mL/min/kg, response within 5 min, no volumes, no fitted
  concentration-BP coefficients, trial mean BP reductions only, posture as an essential covariate: **matches** (PDF p2-3).
- Note (no verdict needed): the label's 33 mL/min/kg and 5.5 h exceed the implemented Kanto model's 24.8 mL/min/kg and
  4.9 h; the label's dose is the hydrochloride, consistent with the model's dose-as-labelled assumption.
