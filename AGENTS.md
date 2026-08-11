# AGENTS.md — Antigravity & AI Agent Orientation & Work Log

This file provides orientation, architectural reference, and a shared work log for **Antigravity** and other AI coding assistants working on **stanpumpR** across different development environments.

---

## 1. Project Overview

**stanpumpR** is an open-source R package and Shiny web application for pharmacokinetic / pharmacodynamic (PK/PD) simulation. It calculates and plots predicted plasma ($C_p$) and effect-site ($C_e$) concentrations for IV and extravascular anesthetics using **analytical closed-form 3-compartment solutions** rather than numerical ODE solvers.

- **Language**: R (developed and tested on R 4.6.x, declared minimum `R >= 4.1`)
- **Framework**: Shiny + `bslib` (Bootstrap 5)
- **Package Management**: `renv` (`renv.lock`)
- **Testing**: `testthat` (`tests/testthat/`)
- **Main Entry Point**: `app.R` $\rightarrow$ `stanpumpR::run_app()`

---

## 2. Quick Commands

```r
devtools::load_all(".")   # Load package functions locally
run_app()                 # Launch Shiny app locally (add ?debug=1 to URL for profiler/log)
devtools::test()          # Run testthat unit test suite
renv::restore()           # Restore pinned R package dependencies from renv.lock
```

---

## 3. Architecture & Pipeline Quick Reference

### Reactive Request-to-Render Pipeline (`R/app_server.R`)
1. **Inputs**: Patient covariates, dose grid (`rhandsontable`), clinical events, plot settings.
2. **Validate**: `doseTableClean()`, `eventTableClean()`, `testCovariates()` in `server-helpers.R`.
3. **Resolve PK**: `recalculatePK()` in `getDrugPK.R` converts covariates $\rightarrow$ micro rate constants $\rightarrow$ eigenvalues (`cube.R`) $\rightarrow$ $k_{e0}$ (`tPeakError.R`) $\rightarrow$ exponential coefficients.
4. **Simulate**: `simCpCe.R` dispatches to:
   - `advanceClosedForm0.R` (IV, standard PK)
   - `advanceClosedForm1.R` (time-varying PK with events)
   - `advanceClosedFormPO_IM_IN.R` (extravascular 1st-order absorption)
5. **Plot & Render**: `simulationPlot.R` generates `ggplot2` output, results table, and dosing suggestions (`suggest.R`).

*Note: `recalculatePK()` and `processdoseTable()` perform smart diffing, re-simulating only drugs whose inputs have changed.*

---

## 4. Key Development Rules & Conventions

- **`Collate:` Order**: `DESCRIPTION` strictly defines file load order under `Collate:`. **Every** new `.R` file added to `R/` MUST be registered in `Collate:` or `R CMD build` will fail.
- **Drug Library Touchpoints**: Adding a drug requires:
  1. `R/drugs_<name>.R` (covariate model function)
  2. `inst/extdata/drugDefaults_global.csv` (row with colors, units, MEAC)
  3. `DESCRIPTION` (`Collate:` entry)
  4. `tests/testthat/test-drugs-<name>.R` (unit test)
- **Logging & Debugging**: Use `outputComments()` for debug logging (active when `?debug=1` is present).
- **Dependencies**: Always commit `DESCRIPTION` and `renv.lock` together when adding/updating R packages.

---

## 5. Work Log & Session History

*Use this section to record key changes, feature additions, or open items so AI agents have full context on any machine.*

### 2026-08-11
- Created `GEMINI.md` and `AGENTS.md` to establish cross-machine context and workflow guidelines for Antigravity.
- Verified workspace setup and reviewed core architecture (`docs/architecture.md`, `CLAUDE.md`, `DESCRIPTION`).
- Updated `.Rbuildignore` to ignore agent markdown files during `R CMD check`.
