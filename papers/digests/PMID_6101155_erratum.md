---
file: PMID_6101155_erratum.pdf
pmid: 6101155
doi: ""
citation: "Errata, J Pharm Sci 1981;70(12):1376 (December 1981). One page of four corrections: terfenadine RIA (Cook 1980), helenalin/brusatol/bruceantin antileukemic activity (Hall 1981), thermal hardness of tablets (Parrott 1981), nitroglycerin pharmacokinetics after IV infusion (McNiff 1981)."
kind: other
drugs: []
source_read: "text layer and rendered page image (the single page, PDF page 1)"
digest_by: Claude (unattended digest_run.py), 2026-11-20
checked_against: stanpumpR master 0da253a5
verification: not-applicable
open_questions:
  - "[source] The file stem PMID_6101155 is presumably the nitroglycerin paper (McNiff EF et al., J Pharm Sci 1981;70:1054); the erratum page itself carries no PMID, and the context file has no PubMed metadata or repo locations, so the pairing is not confirmed here. Check against the main PMID_6101155 digest if one exists."
---

# Erratum, J Pharm Sci 1981;70(12):1376

## Study
Not a study: a one-page errata notice (printed page 1376, PDF page 1). It contains four corrections, none quantitative PK data:
1. Terfenadine radioimmunoassay in human plasma (Cook CE et al., J Pharm Sci 1980;69:1419): chemical structure on p1419 redrawn (compounds Ia R = CH3, Ib R = CH2OH, Ic R = COOH).
2. Antitumor agents XLII, helenalin/brusatol/bruceantin (Hall IH et al., 1981;70:1147): structure on p1148 redrawn (compounds I and II).
3. Thermal hardness coefficient of tablets (Parrott EL, 1981;70:328): Table I title should read average (+/-SD) hardness in kiloponds at various temperatures (+/-1 degree); p329 column 2 line 11: slope about -0.02 kilopond/degree for products A-E.
4. Nitroglycerin pharmacokinetics after IV infusion in normal subjects (McNiff EF, Yacobi A, Young-Chang FM, Golden LH, Goldfarb A, Fung H-L, 1981;70:1054): p1055 column 1, line 5, under Assay, "Caprylene" should read "Octalene (aldrin)" (the internal-standard/assay reagent name; no numeric change).

## Model and parameters
```yaml
parameters: []  # errata notice; no PK parameters
```

## Pharmacodynamics and end points
None. No opioid content (no MEAC, ke0, tPeak or potency ratio).

## Covariates
None.

## Caveats
The correction in item 4 concerns a nomenclature slip in the assay description only; it does not alter any reported nitroglycerin PK value.

## Used by stanpumpR
No locations in the context file (`cited_in_repo`: false).

## Verification against stanpumpR
Not applicable: the repository does not cite this file and it carries no numeric content the code could use.
