---
file: NJ_label-carbocaine_012250_2018-11-05.pdf
pmid: null
doi: null
citation: "CARBOCAINE (mepivacaine hydrochloride injection, USP) prescribing information. Hospira, Inc. NDA 012250 (supplement s037), Revised 11/2018 (LAB-0996-3.2; FDA Reference ID 4343839; accessdata label dated 2018-11-05)."
kind: label
drugs: [mepivacaine]
source_read: "text layer, all 14 pages (whole label: Description, Clinical Pharmacology/Pharmacokinetics, Warnings, Precautions, Adverse Reactions, Overdosage, Dosage and Administration, How Supplied); page images of PDF pages 3 and 12 rendered and read"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: matches
---

# Carbocaine label 2018: mepivacaine injection (Hospira)

## Study
Not a study: the US prescribing information (old, non-PLR format) for Carbocaine, mepivacaine HCl 1%, 1.5% and
2% for infiltration, peripheral nerve block, caudal and lumbar epidural block; not for spinal or dental use
(p. 1). Pharmacokinetic statements are narrative, with no tables of parameters and no cited studies.

## Model and parameters
No compartmental model. Narrative PK (Clinical Pharmacology, pp. 2-3):
- Protein binding about 75% (p. 2).
- Half-life 1.9 to 3.2 h in adults, 8.7 to 9 h in neonates (p. 3).
- Hepatic metabolism (hydroxylation, N-demethylation); 5 to 10% excreted unchanged in urine; over 50% of a dose
  excreted in bile as metabolites; most drug and metabolites eliminated within 30 h (p. 3). Metabolites: two
  phenols (as glucuronides) and 2',6'-pipecoloxylidide.
- Epinephrine 1:200,000 (5 mcg/mL) usually reduces absorption rate and plasma concentration (p. 2).
- Onset of sensory block about 3 to 20 min; anesthesia adequate for 2 to 2.5 h of surgery (p. 2).
- No clearance, volume, Cmax, tmax or AUC given anywhere in the label.

```yaml
parameters:
  - {name: protein binding, value: 75, unit: "%", source: "Clinical Pharmacology, p. 2", note: approximate}
  - {name: t1/2 adults, value: "1.9-3.2", unit: h, source: "Clinical Pharmacology, p. 3", note: range}
  - {name: t1/2 neonates, value: "8.7-9", unit: h, source: "Clinical Pharmacology, p. 3", note: range}
  - {name: fraction excreted unchanged in urine, value: "5-10", unit: "%", source: "p. 3"}
  - {name: mean seizure dose (rhesus monkey), value: 18.8, unit: mg/kg, source: "Overdosage, p. 12 (image read)"}
  - {name: mean arterial plasma concentration at seizure (rhesus monkey), value: 24.4, unit: mcg/mL, source: "Overdosage, p. 12 (image read)"}
  - {name: LD50 IV (mouse), value: "23-35", unit: mg/kg, source: "Overdosage, p. 12"}
  - {name: LD50 SC (mouse), value: 280, unit: mg/kg, source: "Overdosage, p. 12"}
  - {name: max single adult dose (usual), value: 400, unit: mg, source: "Dosage and Administration, p. 12"}
  - {name: max dose given without adverse effect (not recommended), value: 7, unit: mg/kg, source: "p. 12", note: "550 mg"}
  - {name: max total dose per 24 h, value: 1000, unit: mg, source: "p. 12"}
  - {name: pediatric max dose, value: "5-6", unit: mg/kg, source: "p. 12"}
```

## Pharmacodynamics and end points
- No therapeutic plasma concentration range is stated.
- Toxicity: in rhesus monkeys the mean seizure dose was 18.8 mg/kg, at a mean **arterial plasma** concentration of
  24.4 mcg/mL (Overdosage, p. 12; confirmed on the rendered page). Animal data; total (not free) concentration
  presumably, the label does not say; number of animals, infusion rate and assay not given.
- Clinical: age effect on epidural spread, about 40% less drug needed for a given number of dermatomes at 60-79 y
  vs 20-39 y (15 mL 2% at T9-10, n=62; p. 2). Convulsions in about 0.1% of epidural administrations (class
  statement, p. 10).
- No ke0, no effect-site data. Not an opioid; opioid audit not applicable.

## Covariates
Narrative only: age (elderly need less; neonatal t1/2 longer), hepatic and renal disease, epinephrine, urinary pH,
route. No equations.

## Caveats
Old-format label; numbers are unreferenced summary statements. The toxic concentration is from monkeys, arterial
plasma, at a mean seizure dose, so it is not a human threshold and not venous. Multiple-dose vials contain
methylparaben and are not for epidural or caudal use.

## Used by stanpumpR
- `inst/extdata/therapeuticRanges.csv:169` — mepivacaine row: `none_established` (no band; lower/upper/typical 0,
  units mg/L), source text "toxic thresholds only, no therapeutic range; Carbocaine label (2018): seizures in rhesus
  monkeys at a mean arterial 24.4 mcg/mL", `label_statement` the same.

## Verification against stanpumpR
- `therapeuticRanges.csv:169`: the label gives no therapeutic range, so `none_established` with no drawn band is
  correct. The 24.4 mcg/mL rhesus-monkey mean arterial plasma concentration at seizure is transcribed correctly
  (p. 12, rendered page). Unit: 24.4 mcg/mL = 24.4 mg/L, consistent with the row's mg/L. The row omits that it is the
  concentration at the mean seizure dose of 18.8 mg/kg and that it is plasma, but nothing is plotted from it.
  The label year (2018) matches (Revised 11/2018). **matches**
