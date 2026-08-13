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

*Note: `drugs()` rebuilds its per-drug list from `NULL` on every run, so `recalculatePK()` and `processdoseTable()` fully recompute every drug (PK + simulation) on any input change. `processdoseTable()` has a per-drug diff meant to skip unchanged drugs, but it is currently inert (`recalculatePK()` clears each drug's `$DT` right before it runs). What keeps the app fast is the closed-form solution, not incremental skipping.*

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
- Created `codex/security-hardening` from `master` after a clinical-environment security review.
- Added server-side input and bookmark validation, secure server bookmarks, production debug restrictions, and bounded simulation inputs.
- Disabled email by default, added recipient-domain and SMTP configuration, and moved exports into private per-send temporary directories with guaranteed cleanup.
- Overrode rhandsontable's vulnerable Handsontable 6.2.2 assets with 10.0.0 and documented its separate hospital/commercial licensing requirement.
- Pinned GitHub Actions, pinned production deployment to the triggering commit, and isolated preview deployment credentials from production.
- Security hardening committed on `codex/security-hardening` as `93f902a` (`Harden clinical deployment security`); 102 tests pass and all workflow YAML files parse.
- Extended email privacy hardening: visibly normalized ages 90+ to 89 before simulation/export/bookmarking, removed recipient-domain restrictions without persisting or logging recipient addresses, added a PHI warning to comments, kept the disabled email panel visible for configuration/testing, and labeled UI/email exports as simulations rather than patient records.
- SMTP username and app password are read only from `STANPUMPR_EMAIL_USERNAME` and `STANPUMPR_EMAIL_PASSWORD`; plaintext credentials in `config.yml` are no longer supported.
- Connect Cloud migration preparation: runtime configuration works without an untracked `config.yml`, production email settings can come entirely from managed variables, Java-based `mailR` was replaced with curl SMTP, the legacy shinyapps.io production workflow is manual-only, and `tools/write-connect-manifest.R` generates the required committed manifest.
- Consolidated documentation in `docs/security-hardening-changelog.md`, `docs/privacy-and-phi.md`, `docs/security-deployment.md`, and `docs/connect-cloud-deployment.md`, including implemented controls, verification status, operational requirements, and residual risks.

### 2026-08-12
- Changed the default `bookmark_mode` from `server` to `url` in `R/globalVariables.R` and in the shinyapps.io production deploy workflow (`.github/workflows/shiny-deploy-production.yaml`), fixing "server is not configured for saving sessions to disk" on hosts without disk-backed bookmark storage (shinyapps.io). Rationale: the URL carries no confidential data (recipient/comments/exact age are in `bookmarksToExclude`; ages 90+ are normalized to 89 before persistence).
- Re-verified URL injection is blocked against Shiny 1.14.0: URL bookmark state is decoded by `shiny:::RestoreContext$decodeStateQueryString` via `safeFromJSON` (jsonlite) with no `unserialize`/`eval`/`readRDS` on URL content (`readRDS` is only in the `server`/`_state_id_` path, keyed by an alphanumeric-validated id). The sole R code sink, `eval(call(drug, ...))` in `getDrugPK.R`, is gated on every path by `validateDoseTableInput()` (in both `onRestored` and `doseTableClean()`), which rejects drugs outside `drugDefaults$Drug`. `sex` is enum-validated; age/weight/height are range-validated; free-text (recipient/comments) is bookmark-excluded and `hover_info` HTML is `htmlEscape`d.
- Updated `config.yml.sample` and docs (`security-deployment.md`, `privacy-and-phi.md`, `connect-cloud-deployment.md`, `security-hardening-changelog.md`, `architecture.md`, `architecture.html`) to reflect URL as the default and `server` as a Connect Cloud option.
- Reverted the Handsontable 10.0.0 override (see 2026-08-11 entry) back to the MIT-licensed 6.2.2 bundled with `rhandsontable`, for licensing reasons (Handsontable 7.0+ requires a commercial license unsuitable for the hospital/free deployment). Removed `secureRHandsontable()` and `handsontablePatchedDependency()` from `R/shiny-utils.R` (call sites in `createHOT.R` and `app_server.R` now call `rhandsontable::rhandsontable()` directly), the `app_ui()` head dependency injection, the vendored `inst/www/handsontable-10.0.0/` assets, the `handsontable_license_key` config + its `app_run.R` validation + `config.yml.sample` line, and the now-inverted `tests/testthat/test-shiny-utils.R`. **Kept** the version-independent defense-in-depth: the `hookSanitize` / `hookFilterKeys` grid hooks (`addHotHooks()` + `inst/www/hot_funs.js`) and all server-side validators. Trade-off: reintroduces 6.2.2's client-side XSS exposure (< 8.2.0; CVE-2021-23446), mitigated but not eliminated by those hooks and validators.
