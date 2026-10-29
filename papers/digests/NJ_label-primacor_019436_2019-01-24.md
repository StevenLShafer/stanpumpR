---
file: NJ_label-primacor_019436_2019-01-24.pdf
pmid: null
doi: null
citation: "PRIMACOR (milrinone lactate injection) prescribing information, Sanofi-Synthelabo / Abbott, NDA 019436, revised January 2003 (posted on Drugs@FDA 2019-01-24; file 019436s021s022lbl.pdf)."
kind: label
drugs: [milrinone, milrinonePediatric]
source_read: "text layer, all 11 pages (old-format label: Description, Clinical Pharmacology, Pharmacokinetics, Pharmacodynamics, Indications, Warnings, Precautions incl. Pediatric Use, Adverse Reactions, Dosage and Administration, renal dose table); page images rendered and read for PDF pages 2, 3 and 9"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: matches
---

# PRIMACOR (milrinone) label, rev. January 2003

## Study
FDA label (old, pre-PLR format) for milrinone lactate injection, a bipyridine PDE III inhibitor (inotrope and
vasodilator). Vials 1 mg/mL (10, 20, 50 mL) and premixed 200 mcg/mL in 5% dextrose (100, 200 mL). Molecular
weight 211.2 (p1). Indication: short-term IV treatment of acute decompensated heart failure (p4). Clinical
experience summarised: acute IV milrinone in more than 1600 patients with chronic heart failure, heart failure
after cardiac surgery or with myocardial infarction (p2). About 90% of trial patients were aged 45 to 70 years,
mean 61 (pp6-7). No assay or study-level design details are given.

## Model and parameters
No compartmental model. Non-compartmental values in congestive heart failure patients (Pharmacokinetics, PDF
p2, confirmed on page image):

| Setting | Vd | Terminal t½ | Clearance |
|---|---|---|---|
| IV injections 12.5 to 125 mcg/kg | 0.38 L/kg | 2.3 h (mean) | 0.13 L/kg/h |
| IV infusions 0.20 to 0.70 mcg/kg/min | about 0.45 L/kg | 2.4 h (mean) | 0.14 L/kg/h |

Parameters not dose-dependent. Protein binding about 70% (equilibrium dialysis). Urinary excretion after oral
dosing: milrinone 83%, O-glucuronide 12%; about 60% recovered in 2 h and 90% in 8 h in normal subjects; mean
renal clearance about 0.3 L/min (pp2-3). Severe renal impairment (CrCl 0 to 30 mL/min) prolongs terminal
half-life (p10); no numbers given.

```yaml
parameters:
  - {name: Vd, value: 0.38, unit: L/kg, source: "PDF p2", note: "CHF patients, IV injections 12.5-125 mcg/kg"}
  - {name: t_half_terminal, value: 2.3, unit: h, source: "PDF p2", note: "mean, injections"}
  - {name: CL, value: 0.13, unit: L/kg/h, source: "PDF p2", note: "injections"}
  - {name: Vd, value: 0.45, unit: L/kg, source: "PDF p2", note: "about; infusions 0.20-0.70 mcg/kg/min"}
  - {name: t_half_terminal, value: 2.4, unit: h, source: "PDF p2", note: "mean, infusions"}
  - {name: CL, value: 0.14, unit: L/kg/h, source: "PDF p2", note: "infusions"}
  - {name: protein_binding, value: 70, unit: "%", source: "PDF p2", note: "approximately, equilibrium dialysis"}
  - {name: CL_renal, value: 0.3, unit: L/min, source: "PDF p3", note: "mean, approximately; active secretion"}
  - {name: therapeutic_range_low, value: 100, unit: ng/mL, source: "PDF p2", note: "plasma; inotropic and vasodilatory effects observed"}
  - {name: therapeutic_range_high, value: 300, unit: ng/mL, source: "PDF p2", note: "plasma"}
  - {name: near_max_effect_low, value: 150, unit: ng/mL, source: "PDF p3", note: "cardiac output and PCWP"}
  - {name: near_max_effect_high, value: 250, unit: ng/mL, source: "PDF p3"}
  - {name: Css_at_0.5_mcg_kg_min, value: 200, unit: ng/mL, source: "PDF p3", note: "approximately, after 6-12 h"}
```

## Pharmacodynamics and end points
- Therapeutic range: inotropic and vasodilatory effects observed over plasma milrinone 100 to 300 ng/mL
  (Clinical Pharmacology, PDF p2, confirmed on image). Population: congestive heart failure patients; matrix
  plasma; assay not stated; total (not free) concentration implied.
- Near-maximal favourable effects on cardiac output and pulmonary capillary wedge pressure at 150 to 250 ng/mL;
  steady state after about 6 to 12 h of 0.50 mcg/kg/min is about 200 ng/mL (PDF p3, image).
- Loading + maintenance regimens 37.5 mcg/kg/0.375, 50/0.50 and 75/0.75 mcg/kg/min raised cardiac index by
  25, 38 and 42%, lowered PCWP by 20, 23 and 36% and SVR by 17, 21 and 37%; MAP fell up to 5% at the two lower
  regimens and 17% at the highest (PDF p3, image). Onset of haemodynamic improvement within 5 to 15 min. No
  tachyphylaxis at 48 h (some patients to 72 h).
- No ke0, EC50, Emax or toxic threshold stated. Arrhythmia incidence was not related to dose or plasma
  concentration (p7). Not an opioid: no MEAC or potency ratio applies.

## Dosing (Dosage and Administration, pp8-10)
Loading 50 mcg/kg over 10 min. Maintenance 0.375 (minimum), 0.50 (standard), 0.75 (maximum) mcg/kg/min, total
daily 0.59, 0.77, 1.13 mg/kg (p8, text layer; the text layer scrambles this table, its row order was inferred
from the daily totals, and the page was not rendered; stanpumpR does not use it). Not to exceed 1.13 mg/kg/day.
Infusion-rate table at 200 mcg/mL (p9, image), e.g. 0.500 mcg/kg/min at 70 kg = 10.5 mL/h. Renal dose table
(p10, text layer): CrCl 5, 10, 20, 30, 40, 50 mL/min/1.73 m² gives 0.20, 0.23, 0.28, 0.33, 0.38,
0.43 mcg/kg/min.

## Covariates
Renal function (dose table above). No age effect on PK in controlled studies (p7). Weight-normalised dosing.

## Caveats
- Label dates from 2003 and gives no study references, assay, n or variability for PK values.
- PK and the therapeutic range come from adults with congestive heart failure; the label says safety and
  effectiveness in pediatric patients are not established (p6).
- The 100 to 300 ng/mL range is where effects "have been observed", not a formally derived target or a
  toxicity threshold.

## Used by stanpumpR
- `inst/extdata/therapeuticRanges.csv:181` (milrinone) and `:182` (milrinonePediatric): registry source for the
  plasma therapeutic range 100 to 300 ng/mL, co-cited with Hornik 2019.

## Verification against stanpumpR
Checked in `C:\dev\stanpumpRPapers\_build\master_snapshot` (master 50bb4219).

- `therapeuticRanges.csv:181` (milrinone): matrix plasma, lower 100, upper 300, units ng/mL, source "PRIMACOR
  (milrinone) label, rev. 01/2003, Clinical Pharmacology; Hornik CP et al. 2019". Label PDF p2 (image) gives
  100 to 300 ng/mL plasma in Clinical Pharmacology; revision January 2003 confirmed on p11. Units and matrix
  agree. The `label_statement` paraphrase matches p2. The notes' "near-maximal at 150 to 250" matches p3 (image).
  **matches**.
- `therapeuticRanges.csv:182` (milrinonePediatric): same values and text. The label range comes from adults; the
  label itself says pediatric safety and effectiveness are not established (p6). The row's notes attribute the
  pediatric use of 100 to 300 to Hornik 2019, so the label is cited only for the adult range. **matches** (for the
  label's part).
- `typical` = 167 ng/mL is attributed in the notes to Bailey 1994; the label does not give it (its closest values
  are about 200 ng/mL at 0.5 mcg/kg/min and the 150 to 250 band). It is outside this card's scope and is not
  verified here.
- The `sampling` field ("steady state during an infusion") is consistent with p3; the label does not define a
  sampling time for the range itself.
