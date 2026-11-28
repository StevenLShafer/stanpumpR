# stanpumpR against nlmixr2lib

## Acknowledgement

The data in this directory come from **nlmixr2lib**, *A Model Library for 'nlmixr2'*, by Richard Hooijmaijers, Matthew Fidler, Bill Denney and Kiranmayi Vedantham (R package, GPL ≥ 2, https://github.com/nlmixr2/nlmixr2lib). It is a library of several thousand published population pharmacokinetic and pharmacodynamic models. Each model is transcribed with:
- its paper's reference;
- the population the model was fitted to;
- its parameter estimates.

The numbers themselves belong to the **authors of each paper**, who are named in the `reference` column. We thank the nlmixr2lib authors for assembling them and making them public.

**stanpumpR uses nlmixr2lib's data only, never its code.** `data-raw/nlmixr2libCrossCheck.R` parses the model files as text: it reads only literal values, refuses any other expression, and never sources or runs a file. No nlmixr2lib code is copied into stanpumpR. A number nlmixr2lib transcribed never enters a stanpumpR model directly. It is a lead, and it is read in the paper itself before anything changes (`AGENTS.md`). Where nlmixr2lib leads to a paper that becomes a stanpumpR model, that model's header names nlmixr2lib as how the paper was found.

**nlmixr2lib never enters the paper archive** (Steven L. Shafer, 2026-11-28). Its package, model files and any extract of them are never uploaded into `stanpumpRPapers`, filed or digested, by the Paper Management session or the pipeline. A drug or a reference of interest found here is handled as a new drug or a new manuscript by the usual route: the paper is obtained from its publisher under its own PMID and goes through the pipeline like any other (`papers/README.md`, *nlmixr2lib never gets here*).

## What it is for

There are three questions:

1. **Did both libraries read the same numbers?** For each paper that both libraries transcribed (matched by DOI or PMID to a stanpumpR digest card, or, since 2026-11-23, cited by DOI in stanpumpR's R code with no card, when the numbers are compared with the citing files alone), the script asks whether each estimate nlmixr2lib took from the paper is also among the numbers in stanpumpR's card or model code.
   - **A number found in neither is a lead to check in the PDF.** One transcription may be wrong, or the two may parameterise the model differently.
   - **Agreement is weaker evidence than it looks.** Both libraries are AI-assisted (stanpumpR's by Claude, much of nlmixr2lib's by an extraction tool), so they can share a misreading. A disagreement is the strong signal.
2. **Which papers did stanpumpR not consider?** For each drug stanpumpR already has, the script lists the nlmixr2lib papers with no digest card. Each one is a candidate source, comparison model or alternative model for the drug's model selector.
3. **Which drugs does stanpumpR not have?** The script lists the drugs nlmixr2lib models in humans that stanpumpR's Drug Library does not hold. For each, it gives the number of models and the largest study, and whether the drug is mentioned in any stanpumpR registry. A mention there is often only as an interacting drug, so it does not mean the drug is catalogued.

## Files

| File | One row per | Columns worth knowing |
|---|---|---|
| `summary.md` | n/a | The counts and the lead tables. Read this first. |
| `missing-drugs.md` | n/a | Every drug nlmixr2lib models in humans that stanpumpR lacks, largest study first, with the components given only in a combination product (`docs/library-expansion/combination-only.csv`) listed apart. |
| `audit.md` | n/a | Written by hand: the audit of stanpumpR's models that these tables support (2026-11-23), what each kind of unmatched number turned out to be, the findings that need action, and the larger models stanpumpR has not considered. |
| `shared-papers.csv` | nlmixr2lib model of a shared paper | `estimates`, `foundInCard`, `foundInCode` and `foundInNeither` count non-residual estimates. `notFound` names the leads. `closestModelOfPaper` marks the model of each paper whose numbers stanpumpR prints most of; a paper often holds several models (a reference fit, a pooled fit, one per formulation), and stanpumpR implements at most one. `implementedDrug` is true when the card, or the citing code, names a `R/drugs_` file. A paper with no card has an empty `card` and its DOI in `citation`. |
| `shared-parameters.csv` | estimate | `printed` is the number as nlmixr2lib wrote it (inside `log()` where it was written that way). `source` is nlmixr2lib's own note of where in the paper it came from. `inCard` and `inCode` say how the number was found: `exact`; a unit factor such as `/60`, `x1000` or `/100`; `exp ...` for a bare log-scale value; or `fraction ...` for a logit read as the fraction it stands for. Blank means not found. `residual` marks residual-error terms, which are never leads. |
| `candidate-papers.csv` | nlmixr2lib model, of a library drug, whose paper has no card (including papers the code cites, which are also compared in `shared-papers.csv`) | Sorted by drug, then by study size. Gives the population (n, age and weight ranges, disease) the model selector needs. |
| `candidate-drugs.csv` | drug not in the Drug Library | `models`, `largestStudyN`, `largestStudy`, `mentionedIn`, and every model (`allModels`). |

## How a number is matched, and what that cannot tell you

The script collects every number written in stanpumpR's digest card for the paper. It does the same for the model files the card names, and then looks for nlmixr2lib's value among them:
- **Exactly:** equal to the printed digits.
- **Under a unit factor:** minutes against hours, mass prefixes, or percent against a fraction, to within 0.5%.
- **As exp() of the value:** for a bare log-scale number.
- **As the fraction a logit stands for:** for a parameter named `logit...`, which nlmixr2lib writes as `log(p/(1-p))` (the script reads the odds) or as a bare log-odds, and stanpumpR prints as the fraction `p` (added 2026-11-23).

A match means the number was transcribed, not that it sits in the right place: a small integer or 0.75 can match by coincidence. A miss can come from:
- a different parameterisation: a rate constant where stanpumpR has a clearance, or a different reference patient;
- a conversion the script does not try;
- a covariate stanpumpR deliberately does not apply.

So every row is a lead for a person to read, never an automatic correction.

## Rerunning

Base R only. From the repository:

```bash
git clone --depth 1 https://github.com/nlmixr2/nlmixr2lib /tmp/nlmixr2lib
```

```bash
Rscript --vanilla data-raw/nlmixr2libCrossCheck.R /tmp/nlmixr2lib .
```

It takes about a minute and a half and rewrites every file here, `summary.md` included. `summary.md` records the nlmixr2lib version and commit, and stanpumpR's commit. The first run was against nlmixr2lib 0.3.2.9000 (e2b902f, 2026-11-06).
