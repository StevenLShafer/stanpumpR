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
3. **Resolve PK**: `recalculatePK()` in `getDrugPK.R` converts covariates → micro rate constants → eigenvalues (`cube.R`) → $k_{e0}$ (`tPeakError.R`) → exponential coefficients.
4. **Simulate**: `simCpCe.R` dispatches to:
   - `advanceClosedForm0.R` (IV, standard PK)
   - `advanceClosedForm1.R` (time-varying PK with events)
   - `advanceClosedFormPO_IM_IN.R` (extravascular 1st-order absorption)
5. **Plot & Render**: `simulationPlot.R` generates `ggplot2` output.

`recalculatePK()` and `processdoseTable()` are *meant* to diff inputs and only re-simulate drugs that changed — see known issue below.

**Dose table lifecycle**: edits land in a draft (`doseTableDraft()`) with undo/redo; `Apply` commits it to the canonical `doseTable()`; `doseTableClean()` (cleaned via `cleanDT()`) is what the rest of the pipeline actually reads. Clicking the plot to add/edit a dose bypasses the draft and applies immediately.

**Testing/scripting entry point**: `simulateDrugsWithCovariates()` is the exported, Shiny-free API (loops `getDrugPK()` → `simCpCe()` per drug) — used by tests and vignettes to drive the PK/PD core without the app.

---

## Key Rules

- **Adding a drug** requires all four (full procedure: `docs/adding-a-drug.md`):
  1. `R/drugs_<name>.R` (covariate model function)
  2. `inst/extdata/drugDefaults_global.csv` (row with colors, units, MEAC)
  3. `tests/testthat/test-drugs-<name>.R` (unit test — pin values with `expect_equal_rounded()` from `tests/testthat/helpers.R`)
- **Debug logging**: `outputComments()`, active when `?debug=1` is in the URL.
- **Deploy**: GitHub Actions — PRs auto-deploy to a test environment; merges to `master` deploy to production (shinyapps.io).
- **Adding an R package**: add to `DESCRIPTION` first, then `renv::install("pkg")` + `renv::snapshot()`, commit `DESCRIPTION` + `renv.lock` together.

## Known Issue

The per-drug simulation cache doesn't persist: `recalculatePK()` resets `drugs[[drug]]$DT` to `NULL` on every touch, so `processdoseTable()`'s change-detection always compares against `NULL`. Result: every drug re-simulates on every reactive invalidation (any covariate/dose/event edit), not just the one that changed. The skip logic exists but has no state to skip against — treat this as a real bug, not expected behavior.
