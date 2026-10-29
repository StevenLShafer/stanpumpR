---
file: NJ_label-gabitril_020646_2010-09.pdf
pmid: none
doi: none
citation: "GABITRIL (tiagabine hydrochloride) tablets. Prescribing information, NDA 020646/S-017 (020646s017lbl.pdf), Revised September 2010. Cephalon."
kind: label
drugs: [tiagabine]
source_read: "text layer; Clinical Pharmacology (PDF pp. 2-4), Precautions on non-induced patients and drug interactions read in full; other sections skimmed"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 4ea0cab
verification: matches
---

# Gabitril label, NDA 020646/S-017 (Revised September 2010)

The full prescribing information (old, pre-PLR format, no numbered sections). The archive's 2025 file,
`NJ_label-gabitril_020646_2025-04-28.pdf`, holds only bottle labels. Each page carries FDA's note that it may not
be the latest label.

## Study
Not a study. Indication: adjunctive therapy of partial seizures in adults and children 12 and older. Efficacy trials
were in patients taking at least one enzyme-inducing antiepileptic drug (769 patients in three parallel-group trials;
PDF p. 4).

## Model and parameters
Clinical Pharmacology, Pharmacokinetics (PDF pp. 2-3):
- Elimination half-life 7-9 h in normal volunteers (stated twice, the second time "healthy subjects"); in patients
  taking enzyme inducers clearance is about 60% greater and half-life 2-5 h (half-life 50-65% shorter).
- Tmax about 45 min fasting; a high-fat meal prolongs mean Tmax to 2.5 h and lowers Cmax by about 40%, AUC unchanged.
  All clinical trials gave tiagabine with meals.
- Absorption > 95%, absolute oral bioavailability about 90%; linear for single doses of 2-24 mg; steady state within
  2 days.
- 96% protein bound (albumin, alpha1-acid glycoprotein), 10-10,000 ng/mL.
- Mean systemic plasma clearance 109 mL/min (CV 23%). CYP3A primarily. About 2% excreted unchanged.
- Diurnal effect: steady-state Cmin 40% lower and AUC 15% lower after the evening dose.
- Troughs at 30-56 mg/day in controlled trials ranged < 1 to 234 ng/mL (PDF p. 3); the concentration-response
  relation is not understood.

```yaml
parameters:
  - {name: half-life non-induced, value: "7-9", unit: h, source: "Pharmacokinetics, PDF pp. 2-3", note: "normal volunteers / healthy subjects"}
  - {name: half-life induced, value: "2-5", unit: h, source: "Pharmacokinetics, PDF p. 2", note: "patients on enzyme-inducing AEDs"}
  - {name: Tmax fasting, value: 0.75, unit: h, source: "Absorption and Distribution, PDF p. 2", note: "about 45 min; high-fat meal mean 2.5 h"}
  - {name: bioavailability, value: 0.90, unit: fraction, source: "Absorption and Distribution, PDF p. 2", note: "absolute, about"}
  - {name: systemic clearance, value: 109, unit: mL/min, source: "Metabolism and Elimination, PDF p. 3", note: "mean, CV 23% (derived: 6.54 L/h)"}
  - {name: protein binding, value: 96, unit: "%", source: "PDF p. 2"}
```

## Pharmacodynamics and end points
No concentration-effect relation or therapeutic range in the label; only the trough range above.

## Covariates
- Enzyme inducers (carbamazepine, phenytoin, phenobarbital/primidone): clearance 60% greater by population PK
  (Precautions, Drug Interactions, PDF p. 14); 32 mg/day induced is comparable to 12 mg/day non-induced.
- Renal impairment: no effect on total or unbound PK. Moderate hepatic impairment: unbound clearance about 60% lower.
- Children 3-10 y: clearance per kg similar to induced adults; on non-inducing drugs, 2-fold (per kg) higher than
  non-induced adults. Age, sex (weight-adjusted), race, smoking: no important effect.

## Caveats
- 2010 labeling; stanpumpR's "label" values are not tied to a version, and the 2025 prescribing information is not
  archived.
- Doses and strengths are tiagabine hydrochloride.

## Used by stanpumpR
- `R/drugs_tiagabine.R:33-36`: CHECKS, model half-life 5.9 h against "label 7-9 h without inducers"; a 4 mg dose
  peaking at 2.0 h "inside the label's fasted-to-fed Tmax range of about 45 min to 2.5 h".

## Verification against stanpumpR
- "label 7-9 h without inducers" (`R/drugs_tiagabine.R:34`): the label gives 7-9 h in normal volunteers, and 2-5 h
  with inducers. **matches** (healthy volunteers are the non-induced case).
- "fasted-to-fed Tmax range of about 45 min to 2.5 h" (`:35-36`): about 45 min fasting, mean 2.5 h after a high-fat
  meal. **matches**.
- The model's 5.9 h is below the label's 7-9 h; the header already says so and attributes the model to Ingwersen
  2000's monotherapy population (5.72 h). Not a transcription issue.
