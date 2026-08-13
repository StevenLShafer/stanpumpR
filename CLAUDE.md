# CLAUDE.md

Orientation for Claude Code working in this repo. Human-facing docs live in
[`docs/architecture.html`](docs/architecture.html) (visual map) and
[`docs/architecture.md`](docs/architecture.md).

## Security/privacy audit handoff

The active work is on `codex/security-hardening`. Review the working tree, not only commit
`93f902a`; later email, PHI, and Connect Cloud changes are currently uncommitted.

Read these first, then verify them independently:

- `docs/security-hardening-changelog.md` — change and verification inventory.
- `docs/privacy-and-phi.md` — data-flow assessment and residual PHI paths.
- `docs/security-deployment.md` — secure defaults and external controls.
- `docs/connect-cloud-deployment.md` — manifest, variables, and acceptance procedure.
- `AGENTS.md` — chronological work log and repository rules.

Audit server-side input validation; bookmark exclusions and restoration; age normalization;
free-text and recipient PHI risks; SMTP TLS/MIME/header injection; temporary-file cleanup; secret,
log, and deployment exclusions; dependency consistency; pinned workflows; and startup without
`config.yml`. Confirm that the Connect Cloud manifest records the exact GitHub revision.

Known incomplete verification: the updated full `devtools::test()` suite and live Connect Cloud
acceptance test have not yet been completed, and `manifest.json` has not yet been generated.

## What this is

stanpumpR is a Shiny web app that simulates plasma and effect-site concentrations of
IV/oral anesthetics from a table of doses plus patient covariates. It is a standard **R
package** (not a bare Shiny script): the app is `app.R → stanpumpR::run_app()`.

Key architectural fact: concentrations are computed with **closed-form solutions** (sums of
exponentials from a 3-compartment model), never a numeric ODE solver. This is why edits
re-render fast.

## Running & testing

```r
devtools::load_all(".")
run_app()          # URL debug is disabled unless explicitly allowed in local configuration
devtools::test()   # testthat suite
```

`config.yml` is optional; secure built-in defaults apply when absent. SMTP credentials are read
only from `STANPUMPR_EMAIL_*` environment variables. Package versions are pinned with
**renv** — after pulling `renv.lock` changes run `renv::restore()`.

Developed and deployed on **R 4.6.1** with current CRAN package versions (all CRAN, no GitHub
pins). `DESCRIPTION` still declares the minimum as `R (>= 4.1)`, but CI only exercises the
current release and one prior (~4.5), so treat 4.6.x as the supported line.

## The reactive pipeline (all in `R/app_server.R`)

One dependency chain; Shiny re-runs only what an edit invalidates:

```
inputs (covariates, doseTableHTML, events, graph opts)
  → doseTableClean() / eventTableClean() / testCovariates()   # clean + validate
  → drugs()            # A: recalculatePK() → getDrugPK() per drug (covariates → coefficients)
                       # B: processdoseTable() → simCpCe() per drug (simulate)
  → simulationPlotRetval() = simulationPlot(...)   # build ggplot + result tables
  → output$PlotSimulation, output$hover_info, Suggest Dosing, email slide
```

`recalculatePK()` and `processdoseTable()` build up a per-drug list (each returns the modified
list) that `drugs()` feeds from one into the next. Note: `drugs()` is a plain `reactive()` that
starts each run with `newDrugs <- NULL`, so the list is **rebuilt from scratch on every
invalidation** — it is not cached across renders.

`processdoseTable()` contains a diff (`!identical(tempDT, drugs[[drug]]$DT) | …`) meant to skip a
drug whose dose/event subset is unchanged, and its header comment still claims it does. That skip
is currently **inert**: `drugs()` starts from `NULL` and `recalculatePK()` sets each drug's `$DT`
to `NULL` immediately before `processdoseTable()` runs, so the stored `$DT` is always `NULL` and
the diff is always TRUE. `recalculatePK()` has no skip logic of its own. Net effect: **every drug
is fully recomputed (PK + simulation) on every change to any input.** Reviving the skip would
require persisting the previous `drugs()` value across renders (e.g. via `isolate()`) and stopping
`recalculatePK()` from clearing `$DT` — a behavioral change; don't assume the diff works today.

## The PK/PD engine (`R/`)

- `getDrugPK.R` — covariates → micro rate constants → `cube.R` eigenvalues (lambda_1..3) →
  `ke0` (via `tPeakError.R` + `optimize`) → per-route closed-form coefficients.
- `simCpCe.R` — unit conversion, route classification, then dispatches to one of:
  - `advanceClosedForm0.R` (IV, no PK events)
  - `advanceClosedForm1.R` (time-varying PK via events)
  - `advanceClosedFormPO_IM_IN.R` (oral / IM / intranasal)
- `simulateDrugsWithCovariates.R` — exported, Shiny-free multi-drug API (used by vignettes/tests).
- `modelInteraction.R` — propofol × opioid response surface for the interaction facet.
- `ig_absorption.R` — **experimental, not yet integrated.** Closed-form Inverse Gaussian
  absorption model (WIP); not exported or wired into the engine. See its provenance header.

## Drug library — data as code

Each drug is `R/drugs_<name>.R` exporting `<name>(weight, height, age, sex)` that returns a
list with `PK` (v1..v3, cl1..cl3), `tPeak`, `MEAC`, `typical`, `upper/lowerTypical`,
`reference`. Metadata (units, color, ranges) is a row in
`inst/extdata/drugDefaults_global.csv`, loaded via memoised `getDrugDefaultsGlobal()`. The
drug list comes from that CSV's `Drug` column; functions are invoked by name via
`eval(call(drug, ...))`.

**To add a drug see [`docs/adding-a-drug.md`](docs/adding-a-drug.md).** Four touch points:
the `drugs_*.R` file, a CSV row, a `Collate:` entry in `DESCRIPTION`, and a
`test-drugs-*.R` test.

## Conventions

- Files are flat in `R/`; load order is the explicit `Collate:` list in `DESCRIPTION`. Because a
  `Collate:` field is present, **every** file in `R/` must be listed there — an omitted file makes
  `R CMD build` fail (not just warn). Add new files to `Collate:`, or run `devtools::document()`.
- **Comments:** favor generous commenting — explain intent and derivation, not only mechanics. For
  non-obvious, imported, or AI-drafted code, add a **provenance** header recording where it came
  from (which tool/model or human), the date, and whether it has been run/verified.
- Use `outputComments(...)` (not `cat`/`print`) for debug logging; wrap costly reactives in
  `profileCode(...)`.
- New user-facing inputs that should NOT be bookmarked go in `bookmarksToExclude`
  (`R/app_globals.R`).
- Times are stored as elapsed minutes internally; `clockTimeToDelta.R` / `deltaToClockTime.R`
  convert to/from wall-clock display.
- Commit `DESCRIPTION` **and** `renv.lock` together when adding a dependency.
