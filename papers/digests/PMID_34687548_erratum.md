---
file: PMID_34687548_erratum.pdf
pmid: 34687548
doi: 10.1002/cpt.3143
citation: "Corrigendum: Correction to Population Pharmacokinetic Modeling and Probability of Pharmacodynamic Target Attainment for Ceftazidime-Avibactam in Pediatric Patients Aged 3 Months and Older (Franzese RC et al., Clin Pharmacol Ther 2022;111:635-645). Clin Pharmacol Ther 2024;115(2):373."
kind: other
drugs: []
source_read: "text layer (one page); table not checked on a rendered image"
digest_by: Claude (unattended digest_run.py), 2026-11-26
checked_against: stanpumpR master 96311057
verification: not-applicable
---

# Franzese 2024: corrigendum to the ceftazidime-avibactam paediatric PopPK paper

## Study
One-page corrigendum (PDF p1, printed p373). Original paper: Franzese 2022 (PMID 34687548). The only change is that the column headings of Table 2 had ceftazidime and avibactam transposed. The page reprints the corrected Table 2: model-predicted steady-state AUC(0-24), Cmin,ss and Cmax,ss with joint PK/PD target attainment, for adults (phase III cUTI, cIAI, HAP/VAP) and for children (phase I single dose; phase II cIAI and cUTI multiple dose).

## Model and parameters
No model parameters. Table 2 values are geometric mean (%CV). Cmax,ss is at end of infusion; Cmin,ss is 8 h after the start of infusion. The joint target is 50% fT>8.0 mg/L for ceftazidime and 50% fT>1.0 mg/L for avibactam. In the text layer the Cmax and avibactam columns interleave across rows, so the values are not reliably assignable to rows without a page image.

```yaml
parameters: []  # corrigendum; exposure summaries only, none read for use
```

## Pharmacodynamics and end points
Joint PK/PD target attainment (%): adults 98.4-100; children 90.9-100 (footnote a: one subject in each of three cohorts missed the target). No ke0, MEAC or potency data. Not an opioid.

## Covariates
None.

## Caveats
Table layout is scrambled in the text layer.

## Used by stanpumpR
Not cited anywhere in the repo (context `locations` empty). The library has no ceftazidime or avibactam model (grep of R/ and inst/ finds ceftazidime only in a temperature-clearance note, R/physiology.R:232, citing other papers).

## Verification against stanpumpR
Not applicable: nothing in stanpumpR uses this document.
