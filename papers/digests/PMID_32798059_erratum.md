---
file: PMID_32798059_erratum.pdf
pmid: none
doi: 10.1016/j.clinthera.2021.03.018
citation: "Du P, Wang A, Ma Y, Jia A, Li Y, Li X. Corrigendum to 'Impact of SLCO4C1 genotypes, creatinine, and spironolactone on digoxin population pharmacokinetic variables in patients with cardiac insufficiency' [Clin Ther 2020;42(9):1799-1810]. Clin Ther 2021;43(4):790. (correction to Du P et al., Clin Ther 2020;42:1799-1810, PMID 32798059)"
kind: other
drugs: [digoxin]
source_read: "page image of the single page (PDF p1, printed 790), rendered and read; the text layer prints each Table 1 value one row above its label (e.g. the creatinine values sit beside 'Blood urea nitrogen' and 'Creatinine' stands alone), so it was not used for values"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master ca8ce42
verification: discrepancy
---

# Du 2021 corrigendum: Table 1 initial doses of digoxin

## Study
One-page corrigendum. The authors state that the initial digoxin doses in Table 1 of the 2020 article were wrong
and should be 0.125 mg and 0.25 mg, and reprint the whole Table 1. It is the same page that the archived article
PDF already carries as its PDF p17 (PMID_32798059.pdf; text identical).

## Model and parameters
No model change. Corrected Table 1 (rendered), modelling group (n = 126) / validation group (n = 50):

```yaml
parameters:
  - {name: observations, value: "161 / 67", source: "corrected Table 1", note: "per patient 1-3 / 1-4"}
  - {name: initial dose 0.125 mg, value: "84 (66.7%) / 36 (72.0%)", source: "corrected Table 1", note: "the corrected entry"}
  - {name: initial dose 0.25 mg, value: "42 (33.3%) / 14 (28.0%)", source: "corrected Table 1", note: "the corrected entry"}
  - {name: male, value: "85 (67.46%) / 35 (70%)", source: "corrected Table 1"}
  - {name: age, value: "61.05 (22-89) / 59.2 (31-87)", unit: y, source: "corrected Table 1", note: "mean (range)"}
  - {name: weight, value: "67.36 (42-107) / 67.80 (52-91.6)", unit: kg, source: "corrected Table 1"}
  - {name: creatinine, value: "111.31 (28-484) / 90.28 (40-378)", unit: umol/L, source: "corrected Table 1"}
```

Other rows (BMI 21.97 / 23.40; RBC, haemoglobin, haematocrit, ALT, AST, BUN 11.20 / 9.29 mmol/L) are reprinted
with the values the article card records; the corrigendum names only the doses as changed.

## Pharmacodynamics and end points
None.

## Covariates
None changed.

## Caveats
The corrigendum does not say what the wrong values were; the article card records the original Table I printing
"125 ng / 250 ng". The text layer of this file misaligns the table's rows; read the page image. The verdict
`discrepancy` refers only to the 228-concentration statement below, which this corrected table bears on; the
registry's description of the corrigendum itself matches.

## Used by stanpumpR
Not in the context file (cited_in_repo false there), but the repo cites this corrigendum:
- inst/extdata/vasoactiveRegistry.csv:60 (row digoxin__hf__du2020; data-raw/vasoactiveRegistry.R:471-477) -
  "Read from the supplied article and its corrigendum (which corrects only Table 1's initial doses to 0.125 and
  0.25 mg)"; citation "corrigendum Clin Ther 2021;43(4):790, doi 10.1016/j.clinthera.2021.03.018".
- The article's own card: PMID_32798059.md.

## Verification against stanpumpR
- "corrects only Table 1's initial doses to 0.125 and 0.25 mg": **matches** (the only correction the authors
  state; the reprinted table's other values agree with the original as recorded on the article card).
- Citation (Clin Ther 2021;43(4):790, doi 10.1016/j.clinthera.2021.03.018): **matches** the page.
- The corrected table also confirms the article card's finding that the model was fitted to 161 concentrations
  (modelling group) and that 228 = 161 + 67 includes the validation group, against the registry's "Fitted to 228
  TDM concentrations from 126 patients" (vasoactiveRegistry.csv:60): **discrepancy** (minor), already recorded on
  PMID_32798059.md.
