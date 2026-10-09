stanpumpR is an R package that contains a Shiny application. The source is at [github.com/StevenLShafer/stanpumpR](https://github.com/StevenLShafer/stanpumpR). This page is a map of what is there.

## Top level

| Path | What it is |
|---|---|
| `app.R` | One line, `stanpumpR::run_app()`: the deployment entry point |
| `DESCRIPTION`, `NAMESPACE` | Package metadata and exports |
| `R/` | All the code, in flat files (below) |
| `inst/extdata/` | The drug library (`drugDefaults_global.csv`), the event list (`eventDefaults.csv`) and the PowerPoint template |
| `inst/help/` | The hand-written pages of this help, as Markdown |
| `inst/www/` | The app's stylesheet and JavaScript |
| `inst/validation/` | The Gas Man validation record, the scenario drivers and their results |
| `tests/testthat/` | The unit tests, one file per drug and per source file |
| `vignettes/` | Two worked examples of the scripting interface |
| `docs/` | Developer documentation: architecture, adding a drug, the user's guide draft (not shipped in the package) |
| `renv.lock` | Pinned package versions for reproducible deployment |
| `Original Stanpump/` | The original STANPUMP C source and documentation, zipped |
| `.github/workflows/` | Continuous integration and deployment |

## The code in `R/`

**Shell.** `app_run.R` launches the app and merges `config.yml` over the defaults; `app_ui.R` is the whole interface; `app_server.R` is the whole reactive server; `app_globals.R` holds the initial tables, the list of inputs kept out of bookmarks, and the logger; `constants.R` the limits, units and defaults.

**The dose table and inputs.** `createHOT.R` builds the editable grid; `input-tables.R` validates and cleans the dose, event and target tables and enforces the gas rules; `validate-input.R` parses one time or one dose; `utils-time.R` converts between clock time and minutes.

**The pharmacokinetic engine.** `getDrugPK.R` turns covariates into rate constants, eigenvalues (`cube.R`), ke0 and coefficients; `simCpCe.R` simulates one drug's dose table, dispatching to `advanceClosedForm0.R` (intravenous, no events), `advanceClosedForm1.R` (with events) and `advanceClosedFormPO_IM_IN.R` (extravascular); `advanceState.R` and `convertState.R` carry state across boundaries; `calculateCe.R` the effect site; `drug-pipeline.R` loops over the drugs in the table; `simulateDrugsWithCovariates.R` is the exported entry point.

**The drug library.** `drugs_<name>.R`, one per drug: the covariate model and the citation. `drugAndEventDefaults.R` reads the CSVs. `lbmJames.R` is the lean body mass equation.

**Pharmacodynamics and derived quantities.** `modelInteraction.R` (propofol-opioid), `recoveryCalc.R` (time until threshold), `opioidMacInteraction.R`, `setLinetypes.R`.

**The inhaled-gas engine.** `gasProperties.R` (parameters and provenance), `advanceClosedFormGas.R` (the model and its matrix-exponential advance), `gasDrugEntries.R` (how gas results become plot series), `gasRecovery.R` (time until threshold for gases and MAC), `advanceGasManBaseline.R` (a transcription of Gas Man's own update, kept as the reference the engine is compared to).

**Output.** `simulationPlot.R` assembles the figure; `suggest.R` is Suggest Dosing; `sendSlide.R` builds and mails the PowerPoint slide.

**The help.** `help-content.R` (page registry, Markdown rendering, search), `help-drugs.R` (the generated drug pages), `help-scenarios.R` (the teaching scenarios and their loader), `help-ui.R` and `help-server.R`.

**Experimental.** `ig_absorption.R` is a drafted inverse-Gaussian absorption model, tracked but not wired in.

## Configuration

`config.yml` (copied from `config.yml.sample`) sets the title, the email credentials for the slide feature, and the debug level. Settings not given fall back to defaults in `constants.R`.

## Deployment

Pull requests are deployed automatically to a test copy of the app on shinyapps.io when labelled `DEPLOY`; merges to `master` deploy to production. The R-CMD-check workflow runs the test suite on Linux, macOS and Windows.

## Running it yourself

```r
# once
renv::restore()                       # install the pinned packages
file.copy("config.yml.sample", "config.yml")

# each time
devtools::load_all(".")
run_app()                             # add ?debug=1 to the URL for the log
devtools::test()                      # the test suite
```

The R version is recorded in the third line of `renv.lock`.

## Further developer documentation

- `docs/architecture.md`: the reactive pipeline in detail, the dose-table lifecycle, and known issues.
- `docs/adding-a-drug.md`: the step-by-step procedure, summarised under [Contributing](help:contributing).
- `AGENTS.md`: orientation for AI coding assistants working on the repository.
- `inst/help/README.md`: how to write help pages.
