---
file: NJ_dailymed-a4195473.pdf
pmid: null
doi: null
citation: "Prometheus Laboratories Inc. (manufactured by WellSpring Pharmaceutical Canada). TRANDATE (labetalol hydrochloride) Tablets, 100, 200 and 300 mg, NDA018716. DailyMed SPL setid a4195473-51a9-40f1-8678-1478ebca2d84; label text dated November 2010, SPL revised 12/2009; archived 2026-10-21."
kind: label
drugs: [labetalol]
source_read: "text layer, all 16 PDF pages (old non-PLR format): Description and Clinical Pharmacology incl. Pharmacodynamics p1-2; Pharmacokinetics and Metabolism p2-3; Elderly Patients p3; Indications, Contraindications, Warnings p3-4; Precautions incl. Pregnancy, Nursing Mothers, Pediatric and Elderly Use p4-6; Adverse Reactions p6-8; Overdosage and Dosage and Administration p9-10; How Supplied p10; bottle labels and SPL data p10-16. PDF p2 (Pharmacodynamics, start of Pharmacokinetics) checked on rendered image."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: partly-verified
---

# TRANDATE (labetalol HCl) tablets US label, Prometheus, 2009-2010

The SPL carries two dates: "November 2010" at the end of the label text (PDF p10) and "Revised: 12/2009" on the last
page (PDF p16). This is the version archived; the setid may now resolve to a later one.

## Study
Not a study: the brand (NDA018716, marketed since 05/31/1984) label for oral labetalol HCl 100, 200 and 300 mg tablets
for hypertension (PDF p1, p3, p13-15). Strength as the hydrochloride (MW 364.9).

## Model and parameters
No model. Pharmacokinetics and Metabolism (PDF p2-3):

```yaml
parameters:
  - {name: F_oral, value: 0.25, unit: fraction, source: "Pharmacokinetics and Metabolism, PDF p2-3", note: "absolute vs IV infusion; first-pass; increased with food"}
  - {name: F_rel_tablet_vs_solution, value: 1.00, unit: fraction, source: "PDF p2", note: "relative bioavailability"}
  - {name: tmax, value: "1-2", unit: h, source: "PDF p2", note: "peak plasma levels"}
  - {name: thalf_oral, value: "6-8", unit: h, source: "PDF p3", note: "plasma half-life after oral dosing"}
  - {name: protein_binding, value: 0.50, unit: fraction, source: "PDF p3", note: "approximately"}
  - {name: urinary_recovery_24h, value: "0.55-0.60", unit: fraction of dose, source: "PDF p3", note: "conjugates or unchanged"}
```
Completely absorbed; peak levels linear in dose over 100-3,000 mg; steady state by about day 3 of repeated dosing;
half-life unchanged in hepatic or renal impairment, oral bioavailability up in hepatic impairment; dialysis < 1%;
elimination reduced in some elderly patients (PDF p2-3). No clearance, volume or ka is given.

## Pharmacodynamics and end points
- Peak effect of single oral doses within 2-4 h; duration at least 8 h after 100 mg and > 12 h after 300 mg; maximal
  steady-state BP response within 24-72 h on b.i.d. dosing (PDF p2).
- Antihypertensive effect linear in log plasma concentration; reduction of exercise tachycardia at 2 h linear in log
  concentration; about 70% of maximal beta-blockade at 5 h and about 40% at 8 h after 400 mg (PDF p2). No coefficients.
- Postural hypotension 2%, most likely 2-4 h after a dose (PDF p1-2).

## Covariates
Food (F up), hepatic impairment (F up), elderly (slower elimination; usual maintenance 100-200 mg b.i.d.), cimetidine
(F up) (PDF p2-3, p5, p10).

Dosing (PDF p9-10): 100 mg b.i.d., titrate by 100 mg b.i.d. every 2-3 days; usual maintenance 200-400 mg b.i.d.;
severe hypertension 1,200-2,400 mg/day; increments not above 200 mg b.i.d.

## Caveats
Summary values without studies or n; 2009-2010 version.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:74` (`data-raw/antihypertensiveRegistry.R:305`):
  `labetalol__ir_tablet__oral__catalog__none__label`, oral IR tablet, matrix "plasma (free concentration in the fitted
  record)", `official_label_descriptive`, `proposed_unfitted`, `descriptive_only`; limitations: "Label F about 25%,
  Tmax 1-2 h, half-life 6-8 h with first-pass and food effects. The hypertensive-patient record is separate and its BP
  slope was not retrievable."

## Verification against stanpumpR
- F about 25%, Tmax 1-2 h, half-life 6-8 h, first-pass and food effects, IR tablet, oral route: **matches** (PDF p2-3).
- Matrix "free concentration in the fitted record" and the separate hypertensive-patient record: not from this label,
  which says nothing about free concentrations (**cannot verify** here), hence partly-verified.
