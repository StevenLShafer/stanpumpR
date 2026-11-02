# Contributing to stanpumpR

stanpumpR is a collaborative research project: an R package and Shiny app that
simulates plasma and effect-site concentrations from published pharmacokinetic
models. Contributions of drugs, data sets and algorithms are welcome.

## Before you start

Contact Steven Shafer (steven.shafer@stanford.edu) before starting a new drug or a
substantial change, so that two people do not build the same model. For a small
fix to the documentation or the help pages, a pull request on its own is fine.

## Adding a drug

Follow [`docs/adding-a-drug.md`](../docs/adding-a-drug.md). A drug a user can dose
(one with a `Category` in `inst/extdata/drugDefaults_global.csv`) needs six things;
an entry without one, such as a metabolite or the carrier gases, needs fewer, as that
guide says:

1. the model, `R/drugs_<name>.R`;
2. its row in `inst/extdata/drugDefaults_global.csv`;
3. its test, `tests/testthat/test-drugs-<name>.R`;
4. its help narrative, `inst/help/drugs/<name>.md`;
5. its row in the pregnancy registry (`data-raw/pregnancyRegistry.R`);
6. its row in `inst/extdata/therapeuticRanges.csv`.

Every number in a model comes from a primary source that was actually read, with the
source cited in the model's header and `reference` string. Every model scales to body
size as [`docs/weight-adjustment.md`](../docs/weight-adjustment.md) describes, and stays
finite for any legal patient. [`AGENTS.md`](../AGENTS.md) collects the rest of the
house rules (renal covariates, age floors, pharmacogenetics, therapeutic ranges).

## Pull requests

- Run the tests before you push: `devtools::test()`. The models, the registries and the
  help pages are pinned by tests; a change that has none should add one.
- Edit the source, not generated output: a help page's generated tables come from the
  code, and `man/` comes from the roxygen comments (`devtools::document()`).
- Each pull request is checked by R CMD check on Linux, macOS and Windows, deployed to a
  test copy of the app, and reviewed automatically. Keep a pull request to one subject.

## Code of Conduct

Please note that stanpumpR is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By contributing to this project you agree to abide by
its terms.
