---
file: PMID_36691877_erratum.pdf
pmid: 36691877
doi: 10.1002/psp4.13081
citation: "Correction to: Consistent methods for fat free mass, creatinine clearance, and glomerular filtration rate to describe renal function from neonates to adults [O'Hanlon CJ, Holford N, Sumpter A, Al-Sallami HS. CPT Pharmacometrics Syst Pharmacol 2023;12:401-412]. CPT Pharmacometrics Syst Pharmacol 2024;13:181-182."
kind: other
drugs: [mannitol]
source_read: "text layer, both pages; the Figure 1 replacement is described by its caption only"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 7d8b50b
verification: not-applicable
---

# O'Hanlon 2023: correction to the neonatal fat-free-mass equations

## Study
A two-page corrigendum to O'Hanlon 2023 (PMID_36691877). It corrects three things:
- **Equation 7:** now FFMIN = FMAT x FMAT_MAX, with its defining text: the baseline is the fraction FMAT of
  FMAT_MAX describing the nadir of FRFFM.
- **Equation 8:** the neonatal component FFNEO = (FMAT_PRE - FFMIN) x exp(-log(2) x (PMA - 24) / TFF_PRE), with PMA
  in weeks and the 52 written as a divisor in the printed form.
- **Figure 1:** FRFFM from neonates to young adults, with asymptotes of 1 in males and 1.06 in females.

## Model and parameters
```yaml
parameters: []
```
It corrects equation forms only; Table 1's parameter values are not changed.

## Pharmacodynamics and end points
None.

## Covariates
Postmenstrual age, sex, total body mass and height, in the fraction-of-adult-FFM model, as in the article.

## Caveats
The printed Equation 8 is set across several lines and its fraction layout is ambiguous in the text layer; read the
PDF before implementing it.

## Used by stanpumpR
stanpumpR does not implement O'Hanlon's FFM equations. `docs/weight-adjustment.md:75` cites the paper to say that
its neonatal extension needs postmenstrual age, which stanpumpR does not collect, so the library keeps Al-Sallami
2015 throughout. `docs/mannitol.md:130` says the same of the mannitol prototype.

## Verification against stanpumpR
No R code uses Equations 7 or 8, so the correction changes nothing computed (`git grep` of R/ at 7d8b50b for
FMAT / FFMIN / FFNEO / O'Hanlon: no hits). **not-applicable**.
