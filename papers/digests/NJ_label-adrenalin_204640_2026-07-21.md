---
file: NJ_label-adrenalin_204640_2026-07-21.pdf
doi: none
citation: "ADRENALIN (epinephrine injection) 1 mg/mL, Par Health. FDA prescribing information, NDA 204640 (supplement S-022 per URL 204640Orig1s022lbl.pdf), Highlights 'Revised: 06/2026', footer R0 06/2026, signed 2026-07-20; Drugs@FDA date 2026-07-21."
kind: label
drugs: [epinephrine]
source_read: "text layer, whole label (sections 1-17); page images rendered and read for PDF pages 1 (Highlights, revision date), 10 (12.2-12.3), 11 (16-17), 13 (carton label). PDF pages 12-14 are carton and container labels."
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: partly-verified
open_questions:
  - "[source] inst/extdata/therapeuticRanges.csv:196 cites 'label read: ADRENALIN, rev. 10/2026', but this archived label is Revised 06/2026 (PDF p1, footer R0 06/2026). The 10/2026 revision is not in the archive; reading it would confirm the row still holds (no therapeutic range stated)."
  - "[check] The context file gives therapeuticRanges.csv:195; at 50bb4219 line 195 is phenylephrine and the epinephrine row is line 196. A pointer problem in _build context, not the code."
---

# ADRENALIN label (NDA 204640, revised 06/2026)

## Study
Not a study: FDA prescribing information for ADRENALIN (epinephrine injection, USP) 1 mg/mL, single-dose 1 mL vial
and 30 mg/30 mL multiple-dose vial (sections 3, 11, 16). Initial U.S. approval 1939. Indications: anaphylaxis (1.1)
and raising MAP in adults with septic-shock hypotension (1.2).

Dosing (section 2, PDF p1-2):
- Anaphylaxis, IM or SC into the anterolateral thigh, repeat every 5-10 min as needed: 0.3-0.5 mg (max 0.5 mg per
  injection) for adults and children 30 kg or more; 0.01 mg/kg up to 0.3 mg for children under 30 kg (2.2; the
  Highlights say "30 kg or less" for the paediatric band, so the label is inconsistent at exactly 30 kg).
- Septic shock: dilute 1 mg in 1,000 mL D5W or D5NS (1 mcg/mL); infuse 0.05-2 mcg/kg/min titrated to MAP, adjusted
  every 10-15 min in steps of 0.05-0.2 mcg/kg/min; wean, e.g. every 10 min (2.3). Diluted solution stable 4 h at room
  temperature, 24 h refrigerated.

## Model and parameters
No compartmental model, no volumes or clearances in absolute or per-kg form for the indicated populations, and no
Cmax/AUC/tmax tables. Section 12.3 (PDF p10, confirmed on the rendered page):
- Effective half-life after IV injection < 5 min; steady state during continuous infusion within 10-15 min.
- Dose-proportional PK in septic shock over 0.03-1.7 mcg/kg/min.
- Metabolism by MAO and COMT to vanillylmandelic acid; removal of circulating exogenous epinephrine: liver 32%,
  kidneys 25%, skeletal muscle 20%, mesenteric organs 12%.
- Elderly: in healthy men given 45-min infusions at 0.0143 mcg/kg/min, mean steady-state plasma metabolic clearance
  was 144.8 mL/kg/min in men aged 60-65 vs 78 mL/kg/min in men aged 20-25 (higher in the older men, as printed).
- Body weight: higher weight associated with higher clearance and a lower concentration plateau (no equation).
- Molecular weight 183.2 (section 11).

```yaml
parameters:
  - {name: t_half_effective, value: "< 5", unit: min, source: "12.3, PDF p10", note: "after IV injection, label wording"}
  - {name: time_to_steady_state, value: "10-15", unit: min, source: "12.3, PDF p10", note: continuous IV infusion}
  - {name: MCR_elderly_60_65y, value: 144.8, unit: mL/kg/min, source: "12.3, PDF p10", note: "mean, healthy men, 0.0143 mcg/kg/min for 45 min"}
  - {name: MCR_young_20_25y, value: 78, unit: mL/kg/min, source: "12.3, PDF p10", note: "mean, healthy men, same infusion"}
  - {name: dose_proportional_range, value: "0.03-1.7", unit: mcg/kg/min, source: "12.3, PDF p10", note: septic shock}
  - {name: molecular_weight, value: 183.2, unit: g/mol, source: "section 11, text layer"}
```

## Pharmacodynamics and end points
- 12.2 (PDF p10): after IV dosing, BP rise begins within < 5 min and offsets within 15 min; low doses lower SVR and
  diastolic BP (beta2), higher doses raise them (alpha1). Hyperglycaemia and raised lactate; mydriasis.
- No ke0, tPeak, EC50, or therapeutic or toxic plasma concentration range anywhere in the label. Overdose (10)
  is described clinically (hypertension, cerebral haemorrhage, pulmonary oedema, arrhythmias) without
  concentrations. Not an opioid; no MEAC applies.

## Covariates
Age (elderly clearance figure above) and body weight (qualitative). Geriatric use (8.5): start low for anaphylaxis;
cautious dose selection in septic shock. Paediatric septic-shock safety not established (8.4).

## Caveats
Label PK is summary prose with no study references; the elderly clearance figure comes from a small study in
healthy men at a very low rate. Endogenous epinephrine is not discussed. The archived PDF is the 06/2026 revision,
not the 10/2026 revision the repo cites.

## Used by stanpumpR
- `inst/extdata/therapeuticRanges.csv:196` (context says :195): epinephrine row, analyte "epinephrine above the
  endogenous baseline", plasma, lower/upper/typical 0/0/0 ng/mL, band_status none_established, source "no
  therapeutic concentration range; titrated to effect (label read: ADRENALIN, rev. 10/2026)", verification "label read".

## Verification against stanpumpR
- **No therapeutic range; titrated to effect**: this label gives no plasma concentration range and doses the
  septic-shock infusion by titration to MAP (2.3). **matches** for the 06/2026 revision.
- **0/0/0 ng/mL, none_established**: consistent with the label stating no range; no unit conversion is involved.
- **"rev. 10/2026"**: **cannot verify**; the archived label is Revised 06/2026 (PDF p1; footer R0 06/2026). See
  `open_questions`.
- **"above the endogenous baseline"**: the label does not mention endogenous concentrations; this wording is the
  repo's own and is not supported or contradicted by the label.
