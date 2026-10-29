---
file: NJ_label-klonopin_017533_2023-01-17.pdf
pmid: none
doi: none
citation: "KLONOPIN (clonazepam) tablets, prescribing information, NDA 017533 (Drugs@FDA 017533s062lbl.pdf), revised 1/2023; CHEPLAPHARM; Reference ID 5108498."
kind: label
drugs: [clonazepam]
source_read: "text layer: Clinical Pharmacology (pharmacodynamics, pharmacokinetics, subpopulations), Precautions (renal, geriatric, pediatric, drug interactions), Dosage and Administration"
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# Klonopin label 1/2023: clonazepam pharmacokinetics

## Study
Old-format label (no numbered 12.3). Pharmacokinetics paragraph, Clinical Pharmacology (p. 2).

## Model and parameters
- Rapidly and completely absorbed; **absolute oral bioavailability about 90%**.
- Peak plasma concentration **1 to 4 h** after oral dosing.
- About **85% bound** to plasma proteins.
- Highly metabolized; <2% excreted unchanged in urine. Main pathway: nitro-reduction to the 7-amino (4-amino
  derivative in the label's wording) compound, then acetylation, hydroxylation, glucuronidation; CYP3A may be
  important.
- **Elimination half-life typically 30 to 40 h.** Dose-independent kinetics; no autoinduction.
- Children: CL 0.42 ± 0.32 mL/min/kg (2-18 y) and 0.88 ± 0.4 mL/min/kg (7-12 y), decreasing with body weight;
  ketogenic diet without effect.
- No controlled studies of sex, age, renal or hepatic disease.
- Interactions: CYP inducers (phenytoin, carbamazepine, lamotrigine, phenobarbital) raise clearance (about 38% lower
  concentrations); sertraline, fluoxetine, felbamate without effect.

```yaml
parameters:
  - {name: F_oral, value: 0.90, unit: fraction, source: "Clinical Pharmacology, Pharmacokinetics", note: "absolute, about"}
  - {name: t_half, value: "30-40", unit: h, source: "Clinical Pharmacology, Pharmacokinetics", note: typical range}
  - {name: Tmax, value: "1-4", unit: h, source: "Clinical Pharmacology, Pharmacokinetics"}
  - {name: protein_binding, value: 0.85, unit: fraction, source: "Clinical Pharmacology, Pharmacokinetics", note: about}
  - {name: CL_children_2_18y, value: 0.42, unit: mL/min/kg, source: "Clinical Pharmacology", note: "mean ± 0.32 SD"}
```

## Pharmacodynamics and end points
Mechanism GABA enhancement; no concentration-effect or therapeutic range given in the label. Panic disorder: 1 mg/day
optimal in the fixed-dose study; 2-4 mg/day less effective with more adverse effects.

## Covariates
None quantified beyond pediatric clearance. Dosing: seizures, adults start <= 1.5 mg/day in three doses, maximum 20
mg/day; children up to 10 y or 30 kg start 0.01-0.03 mg/kg/day (max 0.05), maintenance 0.1-0.2 mg/kg/day; panic
disorder 0.25 mg BID, target 1 mg/day, maximum 4 mg/day.

## Caveats
Label values are rounded summaries without study detail.

## Used by stanpumpR
R/drugs_clonazepam.R:40 (half-life 30-40 h, among the observations dos Santos is checked against) and :130
(reference list).

## Verification against stanpumpR
- Half-life 30-40 h (R/drugs_clonazepam.R:40): **matches**.
- Bioavailability about 0.9 (R:47-48, attributed to Crevoisier 2003 and Berlin 1975): consistent with the label's
  about 90%.
- Observed peak at 1-4 h (R:39, Berlin) is also the label's range.
- Enzyme induction noted as not modelled (R:108-109, Yukawa: clearance 22-75% higher): consistent with the label's
  interaction section (about 38% lower concentrations with inducers).
