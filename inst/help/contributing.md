stanpumpR is a collaborative research project. Individuals interested in adding drugs, pharmacokinetic data sets, or new algorithms are encouraged to contact Steven Shafer (steven.shafer@stanford.edu). It is hoped that eventually each drug in the library will be maintained by an investigator who keeps its pharmacokinetics as up to date as possible.

## What a drug needs

A drug is a published pharmacokinetic model: volumes and clearances (or V1 and rate constants) for a three-compartment mammillary model, with whatever covariates the paper found, and a time to peak effect for the effect site. It also needs a typical concentration range for the shaded band and, for an opioid, a MEAC. If the published model has one or two compartments, set the missing clearances to zero.

The full procedure is in `docs/adding-a-drug.md` in the repository. In outline, four files:

1. **`R/drugs_<name>.R`**: a function of `(weight, height, age, sex)` returning the parameter list, `tPeak`, and a `reference` string ending in a PubMed or DOI URL, which the app turns into a link. Look at `R/drugs_fentanyl.R` for a simple model and `R/drugs_propofol.R` for a covariate model.
2. **`inst/extdata/drugDefaults_global.csv`**: one row with the concentration units, the units offered in the dose table, the default unit, a colour, the typical range, MEAC, the recovery threshold and the class.
3. **`tests/testthat/test-drugs-<name>.R`**: a unit test pinning the parameters at a reference patient with `expect_equal_rounded()`, so that a later edit cannot silently change them.
4. **`inst/help/drugs/<name>.md`**: the narrative for the drug's help page: the population the model was fitted in, the covariates, where it is extrapolated, and anything a user should know. The help tests require this file to exist.

The generated part of the drug's help page, its parameters at reference patients, its citation and its units, appears automatically from the first two files.

## Adding an event-dependent model

Return a named list of parameter sets, one per clinical event the model distinguishes, and list the event names in the function's `events` vector; the names must match `inst/extdata/eventDefaults.csv` with spaces removed. See [Events that change the kinetics](help:models/pk-events) and `R/drugs_dexmedetomidine.R`.

## Adding a teaching scenario

Add a `helpScenario()` call to `helpScenarios()` in `R/help-scenarios.R` and write `inst/help/scenarios/<id>.md`. The tests check every scenario against the drug library and run it through the engine.

## Adding a help page

Add a row to `helpStaticPages()` in `R/help-content.R` and write the Markdown file. See `inst/help/README.md`.

## Adding an R package

Add it to `DESCRIPTION` first, then `renv::install()` and `renv::snapshot()`, and commit `DESCRIPTION` and `renv.lock` together.

## Standards

- Every parameter traceable to a citation, or marked as a guess in the code and in the help.
- Every change covered by a test. `devtools::test()` must pass; the continuous-integration check runs it on every pull request.
- Pull requests are deployed to a test copy of the app for review before merging.
