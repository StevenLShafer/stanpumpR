---
file: NJ_label-ketorolactromethamine_019645_2013-04-01.pdf
pmid: none
doi: none
citation: TORADOL ORAL (ketorolac tromethamine tablets 10 mg, Roche), US prescribing information, NDA 019645 (S-019), package insert revised February 2013; FDA label file dated 2013-04-01 (https://www.accessdata.fda.gov/drugsatfda_docs/label/2013/019645s019lbl.pdf). Reference ID 3281582.
kind: label
drugs: [ketorolac]
source_read: "text layer, partial (boxed warning, Description, Clinical Pharmacology incl. Tables 1-2, Warnings: Renal, Geriatric Use, Dosage and Administration incl. Table 4); Table 1 re-extracted with pypdf because the layout text scrambles its columns"
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# Toradol Oral label 2013: ketorolac tromethamine pharmacokinetics

## Study
US prescribing information. 10 mg ketorolac tromethamine tablets; racemate of S(−) and R(+); MW 376.41,
C19H24N2O6; pKa 3.5 (Description). Oral only as continuation after IV/IM ketorolac; combined duration ≤5 days.

Dosing (Table 4): age 17-64: 20 mg once, then 10 mg q4-6 h prn, ≤40 mg/day. Age ≥65, renally impaired and/or
weight <50 kg: 10 mg once, then 10 mg q4-6 h prn, ≤40 mg/day. Not for age <17. Contraindicated in advanced
renal impairment and in patients at risk of renal failure from volume depletion.

## Model and parameters
No compartmental model. Table 1 (approximate average, mean ± SD; § = simulated from observed data):

| Parameter | Oral 10 mg | IM 15 mg | IM 30 mg | IM 60 mg | IV 15 mg | IV 30 mg |
|---|---|---|---|---|---|---|
| Bioavailability (extent) | 100% (all routes) | | | | | |
| Tmax (min) | 44 ± 34 | 33 ± 21§ | 44 ± 29 | 33 ± 21§ | 1.1 ± 0.7§ | 2.9 ± 1.8 |
| Cmax single (µg/mL) | 0.87 ± 0.22 | 1.14 ± 0.32§ | 2.42 ± 0.68 | 4.55 ± 1.27§ | 2.47 ± 0.51§ | 4.65 ± 0.96 |
| Cmax SS qid (µg/mL) | 1.05 ± 0.26§ | 1.56 ± 0.44§ | 3.11 ± 0.87§ | N/A | 3.09 ± 1.17§ | 6.85 ± 2.61 |
| Cmin SS qid (µg/mL) | 0.29 ± 0.07§ | 0.47 ± 0.13§ | 0.93 ± 0.26§ | N/A | 0.61 ± 0.21§ | 1.04 ± 0.35 |
| Cavg SS qid (µg/mL) | 0.59 ± 0.20§ | 0.94 ± 0.29§ | 1.88 ± 0.59§ | N/A | 1.09 ± 0.30§ | 2.17 ± 0.59 |
| V (L/kg) | 0.175 ± 0.039 (oral and IM) | | | | 0.210 ± 0.044 | |

Populations: oral 77 fasted volunteers; IM 54; IV 24. Urine 91%, feces 6%, metabolised <50%, protein
binding 99%. Whether concentrations are expressed as free acid or tromethamine salt is not stated.

Table 2 (total clearance L/h/kg; terminal t½ h; IM 30 mg / oral 10 mg):
normal 0.023 / 0.025, 5.3 / 5.3; healthy elderly (65-78 y) 0.019 / 0.024, 7.0 / 6.1; hepatic dysfunction
0.029 / 0.033, 5.4 / 4.5; renal impairment (Scr 1.9-5.0 mg/dL) 0.015 / 0.016, 10.3 / 10.8; dialysis
0.016, 13.6 (IM/oral pooled).

Text values: V ~13 L after complete distribution (single dose); S-enantiomer cleared ~2× faster than R;
t½ S 2.5 h (SD 0.4), R 5 h (SD 1.7), racemate 5-6 h; little or no R→S inversion; renal impairment t½ 6-19 h,
AUC of each enantiomer ~+100%; poor correlation of CrCL with clearance (r = 0.5); children 4-8 y (0.5 mg/kg
IV) t½ 5.8 ± 1.6 h, CL 0.042 ± 0.01 L/h/kg, Vss 0.26 ± 0.08 L/kg. High-fat meal lowers peak and delays tmax by
~1 h; antacids no effect. Linear kinetics in the recommended range.

```yaml
parameters:
  - {name: F_oral, value: 1.0, unit: fraction, source: "Table 1; Absorption", note: "100% absorbed"}
  - {name: Tmax_oral_10mg, value: 44, unit: min, source: "Table 1", note: "mean ± 34"}
  - {name: Cmax_oral_10mg, value: 0.87, unit: ug/mL, source: "Table 1", note: "mean ± 0.22"}
  - {name: t_half_S, value: 2.5, unit: h, source: "Excretion", note: "SD 0.4"}
  - {name: t_half_R, value: 5, unit: h, source: "Excretion", note: "SD 1.7"}
  - {name: CL_normal_oral, value: 0.025, unit: L/h/kg, source: "Table 2", note: racemate}
  - {name: CL_renal_impairment_oral, value: 0.016, unit: L/h/kg, source: "Table 2"}
```

## Pharmacodynamics and end points
Peak analgesic effect within 2-3 h; larger doses mainly prolong analgesia (Clinical Pharmacology:
Pharmacodynamics). Activity resides in the S-form. No analgesic concentration, EC50 or effect-site value.
Not an opioid; no MEAC/ke0/potency ratio given.

## Covariates
Age (elderly t½ 5→7 h), renal impairment (CL down, t½ up), weight <50 kg (dose cap), hepatic (no
significant change). No equations.

## Caveats
Many Table 1 values are simulated (§). Concentration basis (salt vs free acid) not stated.

## Used by stanpumpR
- `R/drugs_ketorolac.R:45` — complete oral bioavailability of the racemate (IV/IM/oral crossover and the oral
  label).
- `R/drugs_ketorolac.R:74`, `inst/help/drugs/ketorolac.md:28` — the label limits dosing in renal impairment
  and the elderly.

## Verification against stanpumpR
- `R/drugs_ketorolac.R:44-45`: complete bioavailability (`bioavailability_PO = 1`, line 106). Label: oral
  100% absorbed, extent equal to IV bolus. **matches**.
- `R/drugs_ketorolac.R:74`, `ketorolac.md:28`: label limits dosing in renal impairment and the elderly.
  Label Table 4: ≥65 y, renally impaired or <50 kg get 10 mg (not 20 mg) loading, ≤40 mg/day; contraindicated
  in advanced renal impairment. **matches** (the label also limits by weight <50 kg, not mentioned).
- `R/drugs_ketorolac.R:89`: MW tromethamine 376.409; label 376.41. **matches**.
- Comparison (not a citation): code lines 50-52 put 10 mg oral at 1.01 mg/L at 12 min; label Table 1 gives
  0.87 ± 0.22 µg/mL at 44 ± 34 min. The code already flags its oral curve as provisional and early/high.
  Label S-enantiomer t½ 2.5 h contrasts with the model's 24 h S terminal phase (code line 21 flags this tail
  as uncertain). No discrepancy with what the code claims the label says.
