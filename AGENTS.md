# AGENTS.md — AI Agent Orientation

**stanpumpR** is an R package + Shiny app for PK/PD simulation. It calculates and plots predicted plasma ($C_p$) and effect-site ($C_e$) concentrations for IV and extravascular anesthetics using **analytical closed-form 3-compartment solutions** (no ODE solver). Stack/version details: `DESCRIPTION`. Entry point: `app.R` → `stanpumpR::run_app()`.

---

## Quick Commands

```r
devtools::load_all(".")   # Load package functions locally
run_app()                 # Launch Shiny app locally (add ?debug=1 to URL for profiler/log)
devtools::test()          # Run testthat unit test suite
devtools::document()      # Update NAMESPACE and man/ documentation
renv::restore()           # Restore pinned R package dependencies from renv.lock
```

First-time local setup: copy `config.yml.sample` → `config.yml`

---

## Architecture: Reactive Request-to-Render Pipeline (`R/app_server.R`)

*Full details available in `docs/architecture.md`*

1. **Inputs**: Patient covariates, dose grid (`rhandsontable`), clinical events, plot settings.
2. **Validate**: `doseTableClean()`, `eventTableClean()`, `testCovariates()`.
3. **Resolve PK**: `recalculatePK()` calls `getDrugPK()`, which converts covariates → micro rate constants → eigenvalues (`cube()`) → $k_{e0}$ (solved by `tPeakError()` + `CE()`) → exponential coefficients.
4. **Simulate**: `processdoseTable()` calls `simCpCe()`, which first expands any `Plasma target` / `Effect site target` rows into a TCI infusion schedule (`tci.R`, Shafer & Gregg 1992; returned as `$tci` for the rate panel and export), then dispatches to:
   - `advanceClosedForm0.R` (IV, standard PK)
   - `advanceClosedForm1.R` (time-varying PK with events)
   - `advanceClosedFormPO_IM_IN.R` (extravascular 1st-order absorption)
5. **Plot & Render**: `simulationPlot.R` generates `ggplot2` output.

`processdoseTable()` re-simulates only the drugs whose inputs changed: `drugs()` passes it the previous result as `cache`, and a drug whose `simulationKey()` (PK, dose rows, PK events, plot length, recovery switch) is unchanged reuses its own stored simulation. `foldMetabolites()` always re-runs on top.

**Dose table lifecycle**: edits land in a draft held by an `{undomanager}` (`doseTableHistory()`, with `doseTableDraft()` a thin reactive over its `$value`) that owns the undo/redo history; `Apply` commits it to the canonical `doseTable()`; `doseTableClean()` (cleaned via `cleanDoseTable()` in `input-tables.R`) is what the rest of the pipeline actually reads. Clicking the plot to add/edit a dose bypasses the draft and applies immediately, and any direct write to `doseTable()` resets the draft and clears the history.

**Testing/scripting entry point**: `simulateDrugsWithCovariates()` is the exported, Shiny-free API (loops `getDrugPK()` → `simCpCe()` per drug) — used by tests and vignettes to drive the PK/PD core without the app.

---

## Key Rules

- **Adding a drug** requires all four (full procedure: `docs/adding-a-drug.md`):
  1. `R/drugs_<name>.R` (covariate model function)
  2. `inst/extdata/drugDefaults_global.csv` (row with colors, units, MEAC)
  3. `tests/testthat/test-drugs-<name>.R` (unit test — pin values with `expect_equal_rounded()` from `tests/testthat/helpers.R`)
  4. `inst/help/drugs/<name>.md` (the narrative for the drug's generated help page; `test-help-drugs.R` fails without it)
- **Body size scaling is mandatory** (`docs/weight-adjustment.md`): a drug function is `<name>(weight, height, age, sex, adjustToFFM = TRUE)` and either carries its own size covariate (Eleveld-style) or scales its 70 kg reference parameters with `pkSizeFactors()` — volumes by `$volume`, clearances by `$clearance` — passing the `legacy*` factors that reproduce the published scaling when the switch is off. Tests pin both switch positions.
- **Renal covariates**: a model with a creatinine-clearance or eGFR term adds `creatinine = NULL` to its signature (getDrugPK passes the Patient Profile's serum creatinine only to models that name it) and computes renal function with `R/renalFunction.R`, passing `patientCreatinine(creatinine, sex)`. A blank field is NULL, which means an assumed normal creatinine for the patient's sex; the drug header and `reference` string must say so.
- **Non-linear or non-mammillary sources** (saturable binding, reversible metabolite pairs) must be reduced to the linear engine with the reduction documented in the drug header: see `R/drugs_cefazolin.R`, `R/drugs_hydrocortisone.R`, `R/drugs_prednisolone.R`.
- **Debug logging**: `outputComments()`, active when `?debug=1` is in the URL.
- **Deploy**: GitHub Actions — PRs auto-deploy to a test environment; merges to `master` deploy to production (shinyapps.io).
- **Adding an R package**: add to `DESCRIPTION` first, then `renv::install("pkg")` + `renv::snapshot()`, commit `DESCRIPTION` + `renv.lock` together. For a package that isn't on CRAN, also add it under `Remotes:` in `DESCRIPTION` (e.g. `daattali/undomanager`) and install with `renv::install("user/repo")`, so the deploy can resolve it.


## The Help Tab (`R/help-*.R`, `inst/help/`)

The in-app help is a `bslib::nav_panel("Help")` with a sidebar (search + contents) and one `uiOutput`. Pages are either Markdown in `inst/help/<id>.md` (rendered by `shiny::markdown()`, i.e. commonmark — no pandoc, no MathJax) or generated from the code: one page per drug (`help-drugs.R` runs the drug model at six reference patients), one per teaching scenario (`help-scenarios.R`), plus the drug index, scenario index and bibliography. `help-content.R` holds the registry (`helpStaticPages()`), the renderer and the search; `help-server.R` is called once from `app_server()`. Links between pages are `[text](help:page-id)`; `[text](scenario:id)` makes a button that calls `applyHelpScenario()`, which writes `doseTable()`/`eventTable()` and the inputs, then `bslib::nav_select()`s the Simulator. All clicks go through two delegated handlers in `inst/www/app.js` (`help_goto`, `help_scenario_load`), so the help adds no per-page inputs. `inst/help/README.md` is the authoring guide. Tests: `test-help-content.R`, `test-help-drugs.R`, `test-help-scenarios.R`.
