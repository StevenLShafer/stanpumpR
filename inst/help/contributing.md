stanpumpR is a collaborative research project. Individuals interested in adding drugs, pharmacokinetic data sets, or new algorithms are encouraged to contact Steven Shafer (steven.shafer@stanford.edu). It is hoped that eventually each drug in the library will be maintained by an investigator who keeps its pharmacokinetics as up to date as possible.

## What a drug needs

A drug is a published pharmacokinetic model: volumes and clearances (or V1 and rate constants) for a three-compartment mammillary model, with whatever covariates the paper found, and a time to peak effect for the effect site. It also needs a typical concentration range for the shaded band and, for an opioid, a MEAC. If the published model has one or two compartments, set the missing clearances to zero.

The full procedure is in `docs/adding-a-drug.md` in the repository. In outline, four files and two registry rows:

1. **`R/drugs_<name>.R`**: a function of `(weight, height, age, sex, adjustToFFM = TRUE)` returning the parameter list, `tPeak`, and a `reference` string ending in a PubMed or DOI URL, which the app turns into a link. Body size scaling is required: the model either carries its own size covariate, as the Eleveld models do, or writes its parameters for the 70 kg reference adult and scales them with `pkSizeFactors()`, volumes by `$volume` and clearances by `$clearance` ([Scaling to fat-free mass](help:models/fat-free-mass)). Look at `R/drugs_fentanyl.R` for a simple model and `R/drugs_propofol.R` for a covariate model.
2. **`inst/extdata/drugDefaults_global.csv`**: one row with the concentration units, the units offered in the dose table, the default unit, a colour, the typical range, MEAC, the recovery threshold, the class, and the `Category` it is listed under in the menu the app opens with (one of `DRUG_CATEGORIES` in `R/constants.R`; blank only for a metabolite with no units of its own). An antibiotic's recovery threshold is its MIC for free drug, recorded in `antibioticMicTable()` in `R/antibioticThresholds.R`.
3. **`tests/testthat/test-drugs-<name>.R`**: a unit test pinning the parameters with `expect_equal_rounded()`, with the fat-free-mass switch off (the published parameters) and on (a patient away from the reference), so that a later edit cannot silently change them.
4. **`inst/help/drugs/<name>.md`**: the narrative for the drug's help page: the population the model was fitted in, the covariates, where it is extrapolated, and anything a user should know. The help tests require this file to exist.
5. **Its row in `data-raw/pregnancyRegistry.R`**, rebuilt into `inst/extdata/pregnancyRegistry.csv`: what the literature says of its pharmacokinetics in late pregnancy (`test-pregnancy.R` requires one row per drug).
6. **Its row in `inst/extdata/therapeuticRanges.csv`**: the therapeutic range its band draws, or why it has none (`test-therapeutic-ranges.R`).

The generated part of the drug's help page, its parameters at reference patients, its citation and its units, appears automatically from the first two files.

Some models need more, all described in `docs/adding-a-drug.md`:

- **A renal covariate.** A model with a creatinine-clearance or eGFR term adds `creatinine = NULL` to its signature and computes renal function with `R/renalFunction.R`. The app passes the Patient Profile's serum creatinine only to models that name it; left blank, it is an assumed normal creatinine for the patient's age and sex, and the drug's header and `reference` string must say so ([Covariates and body size](help:models/covariates)).
- **A pharmacogenetic covariate.** A model adds `cyp2d6`, `cyp2c19` or `cyp2b6` (a metaboliser phenotype), `cyp2c9` (a diplotype), or `vkorc1` or `cyp4f2` (a genotype) to its signature, with its default, and receives the Patient Profile's value.
- **Another Patient Profile covariate.** In the same way a model may name `creatinine` (serum creatinine; see `R/renalFunction.R`), `pregnant` (the late-pregnancy flag; see `docs/pregnancy.md`) or `gestationalAge` (weeks at birth, for a neonatal model).
- **A source the engine cannot represent directly.** Saturable binding or a reversible metabolite pair is reduced to the linear mammillary model, and the reduction is written down in the drug file's header (`R/drugs_cefazolin.R`, `R/drugs_hydrocortisone.R`, `R/drugs_prednisolone.R`). The exception is oral bioavailability that falls with the size of the dose, which the drug declares as an `oralSaturation` block and the engine applies to each oral dose ([Oral, intramuscular, intranasal and regional anesthesia doses](help:models/absorption); example `R/drugs_gabapentin.R`).
- **An active metabolite.** A `metabolite` block names the metabolite drug and how it is formed ([Active metabolites](help:models/metabolites); example `R/drugs_codeine.R`).

## Adding an event-dependent model

Return a named list of parameter sets, one per clinical event the model distinguishes, and list the event names in the function's `events` vector; the names must match `inst/extdata/eventDefaults.csv` with spaces removed. See [Events that change the kinetics](help:models/pk-events) and `R/drugs_dexmedetomidine.R`.

## Adding a teaching scenario

Add a `helpScenario()` call to `helpScenarios()` in `R/help-scenarios.R` and write `inst/help/scenarios/<id>.md`. The tests check every scenario against the drug library and run it through the engine.

## Adding a help page

Add a row to `helpStaticPages()` in `R/help-content.R` and write the Markdown file. See `inst/help/README.md`.

## Adding an R package

Add it to `DESCRIPTION` first, then `renv::install()` and `renv::snapshot()`, and commit `DESCRIPTION` and `renv.lock` together. A package that is not on CRAN is also listed under `Remotes:` in `DESCRIPTION` and installed with `renv::install("user/repo")`, so that the deployment can find it. A package needed only by an optional entry point, as `plumber` is by the two APIs, goes under `Suggests:` and is loaded with `requireNamespace()` where it is used.

## Standards

- Every parameter traceable to a citation, or marked as a guess in the code and in the help.
- Every change covered by a test. `devtools::test()` must pass; the continuous-integration check runs it on every pull request.
- Pull requests are deployed to a test copy of the app for review before merging. A merge to `master` reaches production only after the continuous-integration check has passed on it, and the deployment installs that exact commit; the installed commit is named in the [debug log](help:debug-mode).
