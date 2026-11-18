---
file: PMID_32798059_reply.pdf
pmid: null          # the reply's own PMID is not in the context; 32798059 is the parent article (Du 2020)
doi: 10.1016/j.clinthera.2021.02.005
citation: "Du P, Wang A, Ma Y, Jia A, Li Y, Li X. Impact of SLCO4C1 genotypes, creatinine, and spironolactone on digoxin population pharmacokinetic variables in patients with cardiac insufficiency (authors' reply to Dr Walson). Clin Ther 2021;43(4):788."
kind: other
drugs: [digoxin]
source_read: "text layer and rendered page image of PDF page 1 (the only page)"
digest_by: Claude (unattended digest_run.py), 2026-11-18
checked_against: stanpumpR master 32463544
verification: not-applicable
---

# Du 2021: authors' reply on their digoxin population PK model

## Study
A one-page letter (Correspondence, printed p. 788, PDF p. 1) in which Du et al. answer a letter from Dr Walson
(doi 10.1016/j.clinthera.2021.01.023) about their 2020 digoxin population PK paper (doi
10.1016/j.clinthera.2020.07.011, PMID 32798059). No new data. Points made:
- Model structure: the data were sparse routine TDM samples with few observations in the absorption and
  distribution phases, so 2- and 3-compartment models did not converge; the 1-compartment model fitted best.
  Objective function values for the 1-, 2- and 3-compartment models are said to be in the parent paper's
  Supplemental Table II (not in this file).
- Assay: routine TDM immunoassay (Siemens); the authors could not exclude interference from endogenous
  digoxin-like immunoreactive substances or other drugs; validation was done by the manufacturer.
- Sampling: 2 mL venous blood after at least 5 half-lives of continuous dosing (steady state); elapsed time
  since the first dose taken from the hospital information system.
- Target: digoxin trough concentration in the study was 0.8 to 2.0 ng/mL.
- Erratum: Table I of the parent paper was wrong; initial doses were 0.125 mg or 0.25 mg.

## Model and parameters
No parameters are reported in the letter.
```yaml
parameters: []   # letter only; the model parameters are in the parent article (Du 2020), not this file
```

## Pharmacodynamics and end points
No ke0, EC50 or exposure-response. The only concentration stated is the study's trough target, 0.8 to 2.0 ng/mL
(serum/venous blood, assay as above; p. 788). Not an opioid paper.

## Covariates
None reported here (the parent paper's SLCO4C1 genotype, serum creatinine and spironolactone effects are not
restated).

## Caveats
- Sparse, retrospective TDM data: the model cannot describe a distribution phase, as the authors concede.
- Possible immunoassay cross-reactivity was not excluded locally.

## Used by stanpumpR
The context lists no repo location citing this letter (`cited_in_repo: false`). The parent article is cited in
`data-raw/vasoactiveRegistry.R:478-484` (registry entry `digoxin__hf__du2020`, status complete_not_implemented)
and the generated `inst/extdata/vasoactiveRegistry.csv:61`.

## Verification against stanpumpR
- `data-raw/vasoactiveRegistry.R:481` says the model is one compartment, fitted to TDM concentrations with no
  distribution phase, and that the corrigendum corrects only Table 1's initial doses to 0.125 and 0.25 mg. This
  letter agrees on both points (one compartment because of sparse data; initial doses 0.125 or 0.25 mg).
  **matches**. Note the registry cites the formal corrigendum (Clin Ther 2021;43(4):790, doi
  10.1016/j.clinthera.2021.03.018), a different item from this reply (p. 788, doi 10.1016/j.clinthera.2021.02.005);
  this letter is where the authors first acknowledged the dose error.
- The digoxin therapeutic range (`inst/extdata/therapeuticRanges.csv:186-188`: 0.5 to 2 ng/mL serum trough, from
  the LANOXIN label) is not taken from this letter. The study's own target, 0.8 to 2.0 ng/mL, has the same upper
  limit and a higher lower limit; no conflict, as the library cites the label.
- The library's implemented digoxin models (`R/drugs_digoxin.R`, `R/drugs_digoxinIV.R`,
  `R/drugs_digoxinIVPediatric.R`) do not use Du 2020; the letter has no parameters to compare.
Overall **not-applicable**: the file is not cited, and nothing in it contradicts the repo.
