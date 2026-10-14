stanpumpR is an R package that contains a Shiny application. The source is at [github.com/StevenLShafer/stanpumpR](https://github.com/StevenLShafer/stanpumpR). This page is a map of what is there.

## Top level

| Path | What it is |
|---|---|
| `app.R` | One line, `stanpumpR::run_app()`: the deployment entry point |
| `DESCRIPTION`, `NAMESPACE` | Package metadata and exports |
| `R/` | All the code, in flat files (below) |
| `inst/extdata/` | The drug library (`drugDefaults_global.csv`), the event list (`eventDefaults.csv`), the antiseizure registry (`antiseizureRegistry.csv`, `antiseizureParameters.csv`) and the PowerPoint template |
| `inst/help/` | The hand-written pages of this help, as Markdown |
| `inst/plumber/` | The simulation API's `plumber` file (`plumber.R`), which `run_api()` serves |
| `inst/api/` | The TCI API's deployment entry point (`entrypoint.R`, for Posit Connect) and its OpenAPI document (`openapi.json`) |
| `inst/www/` | The app's stylesheet and JavaScript, and the screenshots in this help |
| `inst/validation/` | The GasMan validation record, the scenario drivers and their results |
| `tests/testthat/` | The unit tests: one file per drug (a metabolite or prodrug is tested with its parent), and files for the engines, the inputs, the server and the help |
| `vignettes/` | Two worked examples of the scripting interface |
| `docs/` | Developer documentation: architecture, adding a drug, body-size scaling, the simulation and TCI APIs, notes on individual models, the user's guide draft (not shipped in the package) |
| `data-raw/` | Scripts that derive model parameters or data from published sources, such as the temazepam fit and the antiseizure registry (not shipped in the package) |
| `renv.lock` | Pinned package versions for reproducible deployment |
| `Original Stanpump/` | The original STANPUMP C source and documentation, zipped. Third-party library headers and drivers it was built with (Greenleaf CommLib, Ring Zero) are not included; `REMOVED.TXT` in the archive lists them |
| `.github/workflows/` | Continuous integration and deployment |

## The code in `R/`

**Shell.** `app_run.R` launches the app and merges `config.yml` over the defaults; `app_ui.R` is the whole interface; `app_server.R` is the whole reactive server; `app_globals.R` holds the initial tables, the list of inputs kept out of bookmarks, and the logger; `constants.R` the limits, units and defaults. `startup-drugs.R` is the menu of drugs the app opens with; `illicit-drugs.R` the opt-in that hides the illicit-drug research models; `server-helpers.R`, `utils-shiny-server.R`, `utils-shiny-ui.R` and `utils.R` small helpers.

**The dose table and inputs.** `createHOT.R` builds the editable grid; `input-tables.R` validates and cleans the dose, event and target tables and enforces the gas rules; `validate-input.R` parses one time or one dose; `routes.R` reads a dose's route from its unit; `scheduled.R` expands a `qd`, `bid`, `tid` or `qid` dose into its repeats; `utils-time.R` converts between clock time, the time units and minutes, and `utils-time-display.R` shows a time in the display unit.

**The pharmacokinetic engine.** `getDrugPK.R` turns covariates into rate constants, eigenvalues (`cube.R`), ke0 and coefficients; `simCpCe.R` simulates one drug's dose table, dispatching to `advanceClosedForm0.R` (intravenous, no events), `advanceClosedForm1.R` (with events) and `advanceClosedFormPO_IM_IN.R` (extravascular); `advanceState.R` and `convertState.R` carry state across boundaries; `calculateCe.R` the effect site; `simulationTimeGrid.R` chooses the simulated times; `oral-pulses.R` releases an extended-release product as timed fractions; `advanceMichaelisMenten.R` is the one numerical engine, for phenytoin's saturable elimination; `metaboliteCoefficients.R`, `advanceClosedFormMetabolite.R` and `mergeMetabolite.R` carry a parent's [active metabolite](help:models/metabolites); `drug-pipeline.R` loops over the drugs in the table, re-simulating only those whose inputs changed; `simulateDrugsWithCovariates.R` is the exported entry point.

**The drug library.** `drugs_<name>.R`, one per drug: the covariate model and the citation. `drugAndEventDefaults.R` reads the CSVs. `lbmJames.R` is the lean body mass equation; `pkSizeFactors.R` scales a model to fat-free mass; `renalFunction.R` estimates renal function from the entered or an assumed creatinine. `antibioticThresholds.R` holds the antibiotics' MICs, the source of their thresholds, and `antipsychotics.R` the antipsychotics' profiles: provenance, route basis and receptor-occupancy end points. `alternativeModels.R` holds the Schnider propofol and Minto remifentanil models that the *Minto / Schnider* box runs beside the library's ([The Minto and Schnider models](help:models/minto-schnider)).

**Pharmacodynamics and derived quantities.** `modelInteraction.R` (propofol-opioid), `recoveryCalc.R` (time until threshold), `recoveryStates.R` (time until threshold for a drug that arrives as a metabolite), `derivedSeries.R` (the common time base of the MEAC and interaction panels), `opioidMacInteraction.R`, `setLinetypes.R`.

**The inhaled-gas engine.** `gasProperties.R` (parameters and provenance), `advanceClosedFormGas.R` (the model and its matrix-exponential advance), `gasDrugEntries.R` (how gas results become plot series), `gasRecovery.R` (time until threshold for gases and MAC), `advanceGasManBaseline.R` (a transcription of GasMan's own update, kept as the reference the engine is compared to).

**Output.** `simulationPlot.R` assembles the figure; `suggest.R` is Suggest Dosing; `sendSlide.R` builds and mails the PowerPoint slide.

**Target-controlled infusion.** `tci.R` is the controller behind the *Plasma target* and *Effect site target* rows; `tci-api.R` is [the TCI API](help:tci-api)'s computation (`tciNextRate()`: the next rate for a real pump from its delivery history, pure R); `tci-api-server.R` wraps it in a `plumber` router (`tciApiRouter()`, `run_tci_api()`).

**The Simulation API.** `api.R` is [the Simulation API](help:simulation-api): `apiSimulate()` validates a request and runs it, `apiFormatResult()` writes the result as JSON, CSV, Excel or XML, and `run_api()` serves `inst/plumber/plumber.R`. `api-docs.R` renders its documentation, `inst/apidocs/api.md`, as the page the app serves at [stanpumpr.io/api](https://stanpumpr.io/api). `plumber`, which both web services use, is an optional dependency.

**The simulation API.** `api.R` is the dose-table-in, concentrations-out interface (`apiSimulate()`, `apiFormatResult()`, `apiWriteResult()`, `apiDrugCatalog()`) and `run_api()`, which serves it over HTTP with `plumber`; see [Driving the engine from R](help:scripting).

**The help.** `help-content.R` (page registry, Markdown rendering, search), `help-generated.R` (blocks of hand-written pages built from the code), `help-drugs.R` (the generated drug pages), `help-scenarios.R` (the teaching scenarios and their loader), `help-ui.R` and `help-server.R`.

**Package.** `stanpumpR-package.R` and `zzz.R` hold the package-level declarations.

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
- `inst/apidocs/api.md`: the Simulation API reference, published at [stanpumpr.io/api](https://stanpumpr.io/api) and summarised under [The Simulation API](help:simulation-api).
- `docs/weight-adjustment.md`: how the models are scaled to fat-free mass, summarised under [Scaling to fat-free mass](help:models/fat-free-mass).
- `docs/tci-api.md`: the TCI API reference, every field, unit token, bound and error code, summarised under [The TCI API](help:tci-api).
- `AGENTS.md`: orientation for AI coding assistants working on the repository.
- `inst/help/README.md`: how to write help pages.
