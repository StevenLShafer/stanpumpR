# CLAUDE.md

Orientation for Claude Code working in this repo. Human-facing docs live in
[`docs/architecture.html`](docs/architecture.html) (visual map) and
[`docs/architecture.md`](docs/architecture.md).

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
run_app()          # add ?debug=1 to the URL for the live log + profiler
devtools::test()   # testthat suite
```

Requires a `config.yml` (copy from `config.yml.sample`). Package versions are pinned with
**renv** — after pulling `renv.lock` changes run `renv::restore()`.

## The reactive pipeline (all in `R/app_server.R`)

One dependency chain; Shiny re-runs only what an edit invalidates:

```
inputs (covariates, doseTableHTML, events, graph opts)
  → doseTableClean() / eventTableClean() / testCovariates()   # clean + validate
  → drugs()            # A: recalculatePK() → getDrugPK() per drug (covariates → coefficients)
                       # B: processdoseTable() → simCpCe() per changed drug (simulate)
  → simulationPlotRetval() = simulationPlot(...)   # build ggplot + result tables
  → output$PlotSimulation, output$hover_info, Suggest Dosing, email slide
```

`recalculatePK()` and `processdoseTable()` both mutate a persistent per-drug list and **skip
unchanged drugs** — preserve that diffing behavior when editing them.

## The PK/PD engine (`R/`)

- `getDrugPK.R` — covariates → micro rate constants → `cube.R` eigenvalues (lambda_1..3) →
  `ke0` (via `tPeakError.R` + `optimize`) → per-route closed-form coefficients.
- `simCpCe.R` — unit conversion, route classification, then dispatches to one of:
  - `advanceClosedForm0.R` (IV, no PK events)
  - `advanceClosedForm1.R` (time-varying PK via events)
  - `advanceClosedFormPO_IM_IN.R` (oral / IM / intranasal)
- `simulateDrugsWithCovariates.R` — exported, Shiny-free multi-drug API (used by vignettes/tests).
- `modelInteraction.R` — propofol × opioid response surface for the interaction facet.

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

- Files are flat in `R/`; load order is the explicit `Collate:` list in `DESCRIPTION` — add
  new files there.
- Use `outputComments(...)` (not `cat`/`print`) for debug logging; wrap costly reactives in
  `profileCode(...)`.
- New user-facing inputs that should NOT be bookmarked go in `bookmarksToExclude`
  (`R/app_globals.R`).
- Times are stored as elapsed minutes internally; `clockTimeToDelta.R` / `deltaToClockTime.R`
  convert to/from wall-clock display.
- Commit `DESCRIPTION` **and** `renv.lock` together when adding a dependency.
