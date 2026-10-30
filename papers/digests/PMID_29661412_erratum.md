---
file: PMID_29661412_erratum.pdf
pmid: 29661412
doi: 10.1016/j.bja.2018.05.045
citation: "Eleveld DJ, Colin P, Absalom AR, Struys MMRF. Corrigendum to 'Pharmacokinetic-pharmacodynamic model for propofol for broad application in anaesthesia and sedation' [Br J Anaesth 2018;120:942-959]. Br J Anaesth 2018;121(2):519."
kind: other
drugs: [propofol]
source_read: "text layer, the whole page (1 page)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 7d8b50b
verification: matches
---

# Eleveld 2018 propofol: corrigendum (Q2 in the abstract)

## Study
A one-page corrigendum. It corrects the abstract's reference-individual Q2 from 1.75 to 1.83 L/min. It also asks
implementers to work from the supplementary data files and NONMEM control streams, which "can exactly reproduce the
results of the final models", in preference to the printed equations and tables. The same corrigendum is bound into
the archived article, PMID_29661412.pdf, whose card already records it.

## Model and parameters
```yaml
parameters:
  - {name: Q2 reference individual, value: 1.83, unit: L/min, source: "Corrigendum p519", note: "abstract had 1.75; 1.75 is THETA(5), q2ref before the (1 + THETA16 (1 - MQ3)) term"}
```

## Pharmacodynamics and end points
None.

## Covariates
None stated. The 1.75 to 1.83 difference is the reference individual's Q2 after the maturation term on Q2.

## Caveats
It corrects the abstract only. Table 2 and the control streams are unchanged.

## Used by stanpumpR
Not cited on its own. The propofol model, R/drugs_propofol.R, is built from the article's control-stream THETAs.

## Verification against stanpumpR
- `R/drugs_propofol.R:142` and `:165` carry THETA(5) as `q2ref(adult)=1.750 l/min`, which is the control-stream
  value and is correct.
- At the reference individual (70 kg, 170 cm, 35-year-old man), `propofol(70, 170, 35, "male")` returns
  cl2 = 1.8309 L/min. This is the corrigendum's 1.83, run in R at 7d8b50b: **matches**.
