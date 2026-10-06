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
   observer converts it to an R data frame with `hot_to_r()` and records it via
   `doseTableHistory()$do()`.
3. **Undo / redo** — the draft and its history live in one `{undomanager}`,
   `doseTableHistory()`. `doseTableDraft()` is a thin reactive over its `$value`.
   `$do()` records an edit (pushing the previous draft onto the undo stack and discarding 
   any redo history), and `$undo()` / `$redo()` step through it. This only ever touches
   the draft; nothing downstream re-simulates yet.
4. **`input$dosetable_apply`** commits the pending edit: it copies `doseTableDraft()`'s value
   into `doseTable()`, the canonical reactive everything downstream reads from.
5. `doseTable()` can also be updated directly — by clicking on the plot and adding/editing/
   deleting a dose from the resulting modal, by the edit-doses and suggest-dosing dialogs, or by
   a URL restore — which bypasses the draft entirely and applies immediately, with no separate
   confirm step.
6. Whenever `doseTable()` changes by any route, the observer on it resets the draft to the newly
   committed table and **clears the undo/redo history**: once the canonical table has moved there
   is no earlier draft worth stepping back to. The time-mode switch clears the history explicitly
   instead of relying on that observer, because it assigns `doseTable()` a value that may equal
   what it already holds, and `reactiveVal` does not notify observers on a no-op assignment.
7. **`doseTableClean()`** is what the rest of the pipeline actually depends on. Whenever
   `doseTable()` changes, this reactive re-derives a cleaned copy via `cleanDoseTable()` (coerce column
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
4. `tPeakError()` + `CE()` + `optimize()` back-solve the effect-site rate `ke0` from
   time-to-peak-effect.
5. Precompute per-route (bolus / infusion / PO / IM / IN) exponential coefficients `p_coef_*`,
   `e_coef_*`.

### B — Advance the doses (`simCpCe.R`)

1. Reduce mg/mcg/ng, per-kg, per-hour doses to base units against the drug's concentration unit.
2. Classify each dose as `Bolus`, infusion, or `PO / IM / IN`.
3. Dispatch to a solver:
   - `advanceClosedForm0.R` — IV, no PK events
   - `advanceClosedForm1.R` — time-varying PK driven by events
   - `advanceClosedFormPO_IM_IN.R` — extravascular routes
   - `advanceClosedFormMetabolite.R` — a drug that forms an active metabolite
4. Sum each dose's contribution over the exponential basis; `convertState.R` carries state
   across event boundaries.
5. Interpolate to an even grid (`equiSpace`), normalize to peak Cp/Ce, and scale against MEAC.

Output per drug: a tidy `Time · Plasma · Effect Site · Recovery` table plus `equiSpace` and `max`.

The exported, Shiny-free entry point for this whole path is `simulateDrugsWithCovariates()` — it
loops drugs, calls `getDrugPK()` → `simCpCe()`, and returns per-drug results. This is what the
vignettes and tests drive.

**Active metabolites.** A drug may name another drug as its active metabolite. The parent's
plasma curve is convolved through the metabolite's own disposition
(`metaboliteCoefficients.R`), which leaves a sum of exponentials over the union of the two
drugs' eigenvalues — so the metabolite advances through the same `advanceState()` machinery,
with no solver of its own. An oral dose adds a second branch for metabolite formed during
first pass, which enters the metabolite's central compartment through the absorption step
rather than through the parent.

This is the one place where the per-drug independence the pipeline otherwise assumes breaks
down: a contribution crosses from one drug's entry into another's. So `foldMetabolites()`
(`mergeMetabolite.R`) runs **after** every drug has been simulated, adding each contribution
to the metabolite drug's own row and rebuilding that row from the sum. Each drug keeps its own
simulation in `wideOwn` and the folded total in `wide`, which is what makes re-folding safe.
A metabolite that was never given directly gets a row created for it.

A pure prodrug (`tPeak = 0`, hence `ke0 = 0`) has no effect site: its effect-site column is
`NA`, which the plot drops, and the effect appears on the metabolite's row instead. Codeine is
the worked example; see `R/drugs_codeine.R`.

**"Time until threshold" across a fold.** Concentrations superpose; recovery times do not. So
the merged row's `Recovery` is not built from the two `Recovery` columns — it is solved again
from the *state* underneath them. Each engine carries the effect site out as one amplitude per
eigenvalue (`recoveryStates.R`), `foldMetabolites()` carries every contributing set onto the
merged time line, concatenates the amplitudes, and hands `recoveryCalc()` one combined sum of
exponentials. That is exact, because the whole intravenous path is linear — which is why this
needs no jointly simulated washout, unlike the inhaled gases, whose uptake is coupled through a
shared alveolus (`gasCoupledRecovery()` in `gasRecovery.R`). Without it a patient given only
the parent saw no time at all for the opioid they actually had — 40 mg of oxycodone forms
oxymorphone past oxymorphone's own threshold, and the row showed nothing.

**Pharmacodynamics.** `modelInteraction()` computes a propofol × opioid response surface for the
optional interaction facet (`modelInteraction.R`, `calculateCe.R`).

**Covariate helpers.** `lbmJames()` computes lean body mass; `recoveryCalc()` computes
time-to-threshold; `setLinetypes()` maps normalization + user choices to plasma/effect-site
linetypes.

## Drug library

Adding a drug involves adding one `R/drugs_<name>.R` file and one row in the defaults CSV — the
pattern the project is explicitly built to let outside investigators contribute to. See
**[adding-a-drug.md](adding-a-drug.md)** for the full procedure.

## Component catalog

All files are flat in `R/`.

**Shell — bootstrap & framework**
- `app.R` — one line, `stanpumpR::run_app()`; the deploy entry point.
- `app_run.R` — loads libraries, merges the local config file with default configurations, sets 
  the ggplot theme, mounts asset folders, launches the shiny app.
- `app_ui.R` — the entire UI: navbar, sidebar accordions, dose/event grids, plot card,
  debug panel.
- `app_server.R` — the entire reactive heart: every reactive, observer, output, and modal.
- `app_globals.R` — global variables used by the app: init tables, bookmark exclusion list,
  `outputComments()` logger.
- `constants.R` — constants used in the app.
- `zzz.R` — defines `.sprglobals`, an environment that can hold any global variables that
  need to be shared across the UI and Server portions of the Shiny app.
- `stanpumpR-package.R` — roxygen package-level docs and `@importFrom` declarations.

**Reactive glue — server helpers & UI widgets**
- `drug-pipeline.R` — `recalculatePK()` (step 03) and `processdoseTable()` (step 04):
  per-drug diff-and-recompute drivers behind the `drugs()` reactive.
- `server-helpers.R` — functions used by the Shiny server that are not generalized.
- `utils-shiny-ui.R` — generic UI builders.
- `utils-shiny-server.R` — generic server functions.
- `createHOT.R` — builds the `rhandsontable` dose grid from the current table + drug colors.
- `input-tables.R` — **table-level** validation and cleaning for the dose / event / target grids.
- `validate-input.R` — **cell-level** guards: `validateDose()`, `validateTime()`. One value in,
  a clean string out, never errors.

**PK/PD engine — the math core**
- `getDrugPK.R` — covariates → rate constants, eigenvalues, per-route coefficients.
- `cube.R` — solves the disposition cubic for `lambda_1..3`.
- `simCpCe.R` — single-drug simulation: units → route → solver dispatch.
- `advanceClosedForm0.R` / `advanceClosedForm1.R` / `advanceClosedFormPO_IM_IN.R` /
  `advanceClosedFormMetabolite.R` — the closed-form solvers (IV, event-varying,
  extravascular, active metabolite).
- `metaboliteCoefficients.R` — convolves a parent's curve through a metabolite's disposition;
  also the parent/metabolite unit scaling. `mergeMetabolite.R` — folds each formed
  contribution into the metabolite drug's row once every drug has been simulated.
- `advanceState.R` (`advanceState()`, `advanceStatePO()`), `convertState.R` — carry compartment
  state across dose & event boundaries. `recoveryStates.R` — carries the effect site as one
  amplitude per eigenvalue, so that a drug receiving an active metabolite can have its time
  until threshold solved from the combined state.
- `calculateCe.R` — effect-site concentration from a plasma curve. The `ke0` fit itself
  (`tPeakError()`, `CE()`) lives inside `getDrugPK.R`.
- `modelInteraction.R`, `recoveryCalc.R`, `lbmJames.R` — interaction surface, recovery
  thresholds, body-size scaling.
- `simulateDrugsWithCovariates.R` — exported multi-drug convenience API (no Shiny).
- `ig_absorption.R` — *experimental, tracked, not yet integrated.* Closed-form Inverse Gaussian
  absorption model; not exported or wired in (see its provenance header).

**Output — plot, dosing advisor & export**
- `simulationPlot.R` — assembles the composite `ggplot2` figure and its data tables.
- `setLinetypes.R` — maps normalization + user choices to plasma/effect linetypes.
- `tci.R` — target-controlled infusion: turns "Plasma target" / "Effect site target" dose rows
  into the infusion schedule a TCI pump would run (Shafer & Gregg 1992, as in STANPUMP), using
  the same closed-form coefficients as the solvers. Called from `simCpCe()`.
- `suggest.R` — "Suggest Dosing", optimizes a regimen to hit a target effect-site concentration.
- `sendSlide.R` — renders an `officer` PowerPoint slide from `Template.pptx` and emails it via
  `emayili`.

**Util — time & misc**
- `utils-time.R` — several time-related utility functions.
- `utils.R` — generic helpers only (functions that don't know anything about doses/drugs/etc).
- `drugAndEventDefaults.R` — the memoised drug/event defaults loaders.

## App features

- **Target-controlled infusion** (`tci.R`) — a dose row with units `Plasma target` or
  `Effect site target` runs a simulated TCI pump: a rapid loading infusion sized to reach the
  target without overshoot, then a plasma hold. The controller re-plans every 10 s, hands off
  from effect-site to plasma control within 5% of the target (the effect-site solution is
  ill-conditioned at steady state and would alias), and stops on a target of 0 or a manual
  infusion row. Its rate rows never enter the dose table: `simCpCe()` returns them as `$tci`,
  `simulationPlot()` draws them as a per-drug rate panel with the loading dose written as a
  number, and `sendSlide()` merges them into the exported dose table.
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
- **Tests / CI** (`tests/testthat/`, `.github/`) — one test file per drug and per R file;
  R-CMD-check and shinyapps.io deploy run via GitHub Actions.

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
