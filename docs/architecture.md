# stanpumpR — Architecture

stanpumpR is a Shiny web app that turns a table of drug doses plus a patient's covariates into
predicted **plasma** and **effect-site** concentration curves — using **closed-form**
pharmacokinetic solutions rather than a numeric ODE solver. It is packaged as a standard R
package with a golem-style `ui / server / run` split; the entry point is
`app.R → stanpumpR::run_app()`.

| | |
|---|---|
| Language | R (see `DESCRIPTION` for the supported versions) |
| Framework | Shiny + bslib (Bootstrap 5) |
| Structure | R package, golem-style `ui / server / run` split |
| Deps lock | renv (`renv.lock`) |
| Config | `config.yml` merged over `DEFAULT_CONFIG` |
| Deploy | shinyapps.io, via GitHub Actions on merge to `master` |

## The request → render pipeline

Everything is one reactive dependency chain. Any edit — a covariate, a
dose cell, a graph option — invalidates one link, and Shiny re-runs only what is downstream.
The heavy computation is the last two stages.

### 01 — Inputs

The sidebar groups inputs into a few categories — patient covariates, graph/display
options, optional extra plot facets, and the email-slide form.

The **dose table** is a `rhandsontable` editable table, with `Apply` / `Undo` / `Redo`
controls and a display toggle for elapsed-minutes vs. clock time. Its full edit → apply lifecycle
is covered in [The dose table lifecycle](#the-dose-table-lifecycle).

**Events** are not a permanently visible table — they're edited by clicking on the Events plot
(which only appears when "Events" is chosen in the "Additional Plots" section). Events are stored
in a single reactive called `eventTable()`. Unlike the dose table, there's no draft/undo/redo
layer for events.

The **plot** itself (built in step 06) is also an input source: clicking or double-clicking a
facet adds or edits a dose, and hovering shows a tooltip with additional information.

### 02 — Dose table

The dose table goes through a draft stage before it's committed, so a typo mid-edit never reaches
the simulator: edits land in a draft copy first, `Apply` commits the draft to the canonical
`doseTable()`, and `doseTableClean()` normalizes whatever `doseTable()` currently holds for
everything downstream. See [The dose table lifecycle](#the-dose-table-lifecycle) below for the
full mechanism.

### 03 — Resolve pharmacokinetics

`drugs()` is a reactive that gets re-calculated whenever the dose table changes (more specifically,
when `doseTableClean()` is updated). It runs `recalculatePK()`, which in turn runs `getDrugPK()` in a loop
for each drug in the doses table. This turns the patient's covariates into a full set of rate constants,
eigenvalues, and closed-form coefficients.

### 04 — Simulate concentrations

`processdoseTable()` is designed to diff each drug's doses against the cached result and
re-simulate only what changed — calling `simCpCe()`, which converts dose units, classifies
bolus / infusion / oral routes, and dispatches to the right closed-form solver — for drugs whose
slice actually changed. Output is a tidy time × site table per drug. See
[Known issue: the per-drug cache doesn't persist](#known-issue-the-per-drug-cache-doesnt-persist)
below — the diffing this step relies on doesn't currently have any state to diff against.

### 05 — Assemble the plot

`simulationPlotRetval()` is where the dose/event/PK state gathered above turns into a figure. It
pulls `doseTableClean()`, `eventTableClean()`, `drugs()`, and all the inputs on the page to stitch
every drug's curves into one `ggplot2` object.

### 06 — Render & interact

`output$PlotSimulation` renders the plot. Hover reports precise concentrations, click adds a dose,
double-click edits a drug.

## The dose table lifecycle

1. As a user types into the dose table, JavaScript hooks on the grid do real-time cleanup —
   fixing/converting times, dropping a row if its drug is removed, etc.
2. **`input$doseTableHTML`** fires after that JS-side cleanup completes, on every edit. The
   observer converts it to an R data frame with `hot_to_r()` and saves it as `doseTableDraft()`.
3. **Undo / redo** — pop/push `doseTableDraft()` against the undo/redo stacks. This only ever
   touches the draft; nothing downstream re-simulates yet.
4. **`input$dosetable_apply`** commits the pending edit: it copies `doseTableDraft()`'s value
   into `doseTable()`, the canonical reactive everything downstream reads from.
5. `doseTable()` can also be updated directly — by clicking on the plot and adding/editing/
   deleting a dose from the resulting modal — which bypasses the draft entirely and applies
   immediately, with no separate confirm step.
6. **`doseTableClean()`** is what the rest of the pipeline actually depends on. Whenever
   `doseTable()` changes, this reactive re-derives a cleaned copy via `cleanDT()` (coerce column
   types, drop incomplete rows, convert clock times to elapsed minutes).

## The computational core

stanpumpR never numerically integrates. Each drug is a mammillary 3-compartment model with an
effect-site link; disposition is solved analytically once per patient, then evaluated at every
time point as a sum of exponentials.

### A — Parameterize the patient (`getDrugPK.R`)

1. `eval(call(drug, weight, height, age, sex))` runs the drug's own covariate model (e.g. Eleveld
   for propofol, Kim/Eleveld-style models for remifentanil, etc.) → `v1..v3`, `cl1..cl3`,
   `tPeak`, `MEAC`.
2. Volumes & clearances → micro rate constants `k10, k12, k13, k21, k31`.
3. `cube()` solves the characteristic cubic → eigenvalues `lambda_1, lambda_2, lambda_3`.
4. `tPeakError()` + `optimize()` back-solve the effect-site rate `ke0` from time-to-peak-effect.
5. Precompute per-route (bolus / infusion / PO / IM / IN) exponential coefficients `p_coef_*`,
   `e_coef_*`.

### B — Advance the doses (`simCpCe.R`)

1. Reduce mg/mcg/ng, per-kg, per-hour doses to base units against the drug's concentration unit.
2. Classify each dose as `Bolus`, infusion, or `PO / IM / IN`.
3. Dispatch to a solver:
   - `advanceClosedForm0.R` — IV, no PK events
   - `advanceClosedForm1.R` — time-varying PK driven by events
   - `advanceClosedFormPO_IM_IN.R` — extravascular routes
4. Sum each dose's contribution over the exponential basis; `convertState.R` carries state
   across event boundaries.
5. Interpolate to an even grid (`equiSpace`), normalize to peak Cp/Ce, and scale against MEAC.

Output per drug: a tidy `Time · Plasma · Effect Site · Recovery` table plus `equiSpace` and `max`.

The exported, Shiny-free entry point for this whole path is `simulateDrugsWithCovariates()` — it
loops drugs, calls `getDrugPK()` → `simCpCe()`, and returns per-drug results. This is what the
vignettes and tests drive.

**Pharmacodynamics.** `modelInteraction()` computes a propofol × opioid response surface for the
optional interaction facet (`modelInteraction.R`, `CE.R`, `calculateCe.R`).

**Covariate helpers.** `lbmJames()` computes lean body mass; `recoveryCalc()` computes
time-to-threshold; `setLinetypes()` maps normalization + user choices to plasma/effect-site
linetypes.

## Drug library

Adding a drug involves adding one `R/drugs_<name>.R` file and one row in the defaults CSV — the
pattern the project is explicitly built to let outside investigators contribute to. See
**[adding-a-drug.md](adding-a-drug.md)** for the full procedure.

## Component catalog

Files are flat in `R/` and wired together by the `Collate:` order in `DESCRIPTION`.

**Shell — bootstrap & framework**
- `app.R` — one line, `stanpumpR::run_app()`; the deploy entry point.
- `app_run.R` — loads libraries, merges the local config file with default configurations, sets 
  the ggplot theme, mounts asset folders, launches the shiny app.
- `app_ui.R` — the entire UI: navbar, sidebar accordions, dose/event grids, plot card,
  debug panel.
- `app_server.R` — the entire reactive heart: every reactive, observer, output, and modal.
- `app_globals.R` — global variables used by the app: init tables, bookmark exclusion list,
  `outputComments()` logger.
- `globalVariables.R` — constants used in the app.

**Reactive glue — server helpers & UI widgets**
- `server-helpers.R` — `recalculatePK()`, `cleanDT()`, `checkNumericCovariates()`, reactive
  triggers, intro modal.
- `shiny-utils.R` — UI builders (`inputWithChoices`, `addHotHooks`, inline-input helpers).
- `createHOT.R` — builds the `rhandsontable` dose grid from the current table + drug colors.
- `processdoseTable.R` — per-drug diff-and-simulate driver (pipeline step 04).
- `validateDose.R`, `validateTime.R` — input guards for dose amounts and clock/elapsed times.

**PK/PD engine — the math core**
- `getDrugPK.R` — covariates → rate constants, eigenvalues, per-route coefficients.
- `cube.R` — solves the disposition cubic for `lambda_1..3`.
- `simCpCe.R` — single-drug simulation: units → route → solver dispatch.
- `advanceClosedForm0.R` / `advanceClosedForm1.R` / `advanceClosedFormPO_IM_IN.R` — the three
  closed-form solvers (IV, event-varying, extravascular).
- `advanceState.R`, `advanceStatePO.R`, `convertState.R` — carry compartment state across dose &
  event boundaries.
- `CE.R`, `calculateCe.R`, `tPeakError.R` — effect-site concentration and `ke0` fitting.
- `modelInteraction.R`, `recoveryCalc.R`, `lbmJames.R` — interaction surface, recovery
  thresholds, body-size scaling.
- `simulateDrugsWithCovariates.R` — exported multi-drug convenience API (no Shiny).
- `ig_absorption.R` — *experimental, tracked, not yet integrated.* Closed-form Inverse Gaussian
  absorption model; not exported or wired in (see its provenance header).

**Output — plot, dosing advisor & export**
- `simulationPlot.R` — assembles the composite `ggplot2` figure and its data tables.
- `setLinetypes.R` — maps normalization + user choices to plasma/effect linetypes.
- `suggest.R` — "Suggest Dosing", optimizes a regimen to hit a target effect-site concentration.
- `sendSlide.R` — renders an `officer` PowerPoint slide from `Template.pptx` and emails it via
  `emayili`.

**Util — time & misc**
- `clockTimeToDelta.R`, `deltaToClockTime.R`, `hourMinute.R` — convert between wall-clock
  procedure times and elapsed minutes.
- `utils.R`, `drugAndEventDefaults.R` — small shared helpers and the memoised defaults loaders.

## App features

- **Suggest Dosing** (`suggest.R`) — given a target drug and end time, optimizes bolus +
  infusion amounts to reach and hold a target concentration.
- **Email a slide** (`sendSlide.R`, `Template.pptx`) — builds a branded PPTX from the current
  simulation and mails it: plot, dose table, and a URL that reconstructs the exact state.
- **Editors & modals** (`app_server.R`) — in-app Drug Library and Drug Thresholds editors, plus
  click-to-add-dose / double-click-to-edit driven from plot coordinates.
- **URL bookmarking** (`app_globals.R`) — `enableBookmarking = "url"` encodes inputs into a
  shareable link; `bookmarksToExclude` keeps transient UI state out.
- **Debug & profiler** (`app_globals.R`, `app_server.R`) — `?debug=1` reveals a live log
  (`outputComments()`) and a per-reactive profiler (`profileCode()`).
- **Front-end assets** (`inst/www/`) — `app.css`, `app.js`, `hot_funs.js` (Handsontable
  copy/paste hooks and drug-default injection into the client).
- **Config** (`config.yml`, `app_run.R`) — environment-specific title, help link, and debug flag,
  merged over `DEFAULT_CONFIG` at launch.
- **Reproducibility** (`renv.lock`, `DESCRIPTION`) — `renv.lock` pins exact package versions so
  production matches local; deps declared in `DESCRIPTION`.
- **Tests / CI** (`tests/testthat/`, `.github/`) — ~40 test files (one per drug plus PK,
  plotting, and helper suites); R-CMD-check and shinyapps.io deploy run via GitHub Actions.

## Known issue: the per-drug cache doesn't persist

`drugs()` (step 03) is meant to keep a per-drug cache across reactive re-runs so that
`processdoseTable()` (step 04) can skip re-simulating drugs whose doses haven't changed. In the
current implementation it doesn't: `recalculatePK()` resets `drugs[[drug]]$DT` to `NULL` for
every drug it touches, and `drugs()` itself rebuilds its list from `NULL` on every invalidation
rather than holding it in a `reactiveVal`. So `processdoseTable()`'s `identical(tempDT,
drugs[[drug]]$DT)` check is always comparing against `NULL` — every drug in the table gets
re-simulated on every `drugs()` invalidation (a covariate edit, a dose edit, an event edit), not
just the one that changed. The skip logic is real code; it just has no persisted state to skip
against. Worth fixing or filing as an issue rather than treating as expected behavior.
