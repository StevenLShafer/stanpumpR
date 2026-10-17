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
controls, and above it a collapsible *Time* panel (a `bslib::accordion`, closed by default, its
header showing the current settings through `output$timeSummary`) holding a *Time units* selector
(minutes, hours, days, weeks) and a display toggle for elapsed vs. clock time (clock for minutes
and hours only). Its full edit → apply lifecycle
is covered in [The dose table lifecycle](#the-dose-table-lifecycle), and the time units in
[Time units](#time-units).

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
eigenvalues, and closed-form coefficients. With the **Minto / Schnider** box ticked
(`input$mintoSchnider`), `recalculatePK(alternativeModels = TRUE)` also appends a *companion entry*
for each dosed drug that has an alternative model (`R/alternativeModels.R`: `propofol (Schnider)`,
`remifentanil (Minto)`), built through the same `getDrugPK()` from the alternative model's output
and marked with `$companionOf` and `$modelLabel`.

### 04 — Simulate concentrations

`processdoseTable()` re-simulates only the drugs whose inputs changed, calling `simCpCe()`, which
converts dose units, classifies bolus / infusion / oral routes, and dispatches to the right
closed-form solver. `drugs()` keeps the previous result in `ivSimulationCache` (a plain variable,
so reading it adds no reactive dependency) and passes it as `cache`. Each drug's own simulation is
stored with a key (`simulationKey()`: its resolved PK, its dose rows, its PK events when it has
more than one PK set, the plot length and the recovery switch); a drug whose key is unchanged
reuses its stored simulation. `foldMetabolites()` then runs on every call, so a metabolite drug's
plotted series always reflects its parents' current doses. Output is a tidy time × site table
per drug. A companion entry is simulated on its parent's dose rows, cached under its own name, and
never with the time until threshold, which stays on the library's model.

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
   is no earlier draft worth stepping back to. A change of time unit or display goes through
   `setDoseTable()`, which resets the draft and history itself instead of relying on that
   observer, because it may assign `doseTable()` a value that equals what it already holds, and
   `reactiveVal` does not notify observers on a no-op assignment.
7. **`doseTableClean()`** is what the rest of the pipeline actually depends on. Whenever
   `doseTable()` changes, this reactive re-derives a cleaned copy via `cleanDoseTable()` (coerce column
   types, drop incomplete rows) and converts the Time strings to minutes with
   `displayTimeToMinutes()`, reading them in `doseTableFormat()` (below).

## Time units

The engine, the event table, the scenarios, `input$maximum`'s values, the simulation cache key
and the exported Time columns are in **minutes** whatever the user picks; time units are entry
and display only (`R/utils-time.R`, the "Time units" block of `app_server()`).

- The dose table holds its Time strings **as typed**: a bare number is an offset in the unit
  (from 0, or from the procedure start in clock mode); an `H:MM` entry is a clock time in clock
  mode and elapsed hours and minutes otherwise, never scaled by the unit. So the strings mean
  something only together with the format they were typed in, `c(unit, mode)`, which
  `doseTableFormat()` records beside the table (and bookmarks save as `doseTableFormat`).
  Everything that reads or writes the strings (`doseTableClean()`, the grid, the add-dose,
  edit-doses, add-event, edit-events and Suggest Dosing dialogs) goes by that record; what is
  drawn (axis, hover, recovery labels) goes by the selectors.
- One observer on `input$timeUnits` / `input$timeMode` rewrites the table into the new format
  with `rebaseDoseTimes()` (ten significant digits, so every conversion and chain of conversions
  returns identical minutes, and the simulation cache is reused), then `setDoseTable()`, which
  writes the table and the record together. `plotInfo()` waits while the record and the
  selectors disagree, so nothing is simulated from a table read in the wrong unit.
- Scenarios and the long-term-drug prompt change the selectors through
  `timeApi$showTimeSettings()` and write the table with `timeApi$setDoseTable()` in the format
  the selectors will report, so the browser's echo finds nothing to convert. A restored
  bookmark's selectors are built by `app_ui()` (an old bookmark without a unit opens in days if
  its Max time was over a day), and `onRestored()` converts the saved table to them itself: the
  conversion observer does not run for restored inputs.
- The grid is stamped with its format (`createHOT(..., timeFormat)`); an edit from a grid drawn
  before a switch is converted on arrival.
- Max time choices are per unit (`MAX_TIMES` in `constants.R`); the plot is lengthened past the
  last dose only up to the unit's longest choice. TCI rows and inhaled agents are simulated only
  on plots of `ACUTE_MAX_PLOT_MINUTES` (a week) or less (`timeUnitViolation()`).

## The computational core

stanpumpR never numerically integrates. Each drug is a mammillary 3-compartment model with an
effect-site link; disposition is solved analytically once per patient, then evaluated at every
time point as a sum of exponentials.

### A — Parameterize the patient (`getDrugPK.R`)

1. `do.call(drug, covariates)` runs the drug's own covariate model with the four patient
   covariates passed by name, plus `adjustToFFM` when the drug function declares it (e.g. Eleveld
   for propofol, Kim/Eleveld-style models for remifentanil, etc.) → `v1..v3`, `cl1..cl3`,
   `tPeak`, `MEAC`.
2. Volumes & clearances → micro rate constants `k10, k12, k13, k21, k31`.
3. `cube()` solves the characteristic cubic → eigenvalues `lambda_1, lambda_2, lambda_3`.
4. `tPeakError()` + `CE()` + `optimize()` back-solve the effect-site rate `ke0` from
   time-to-peak-effect.
5. Precompute per-route (bolus / infusion / PO / SL / IM / IN) exponential coefficients `p_coef_*`,
   `e_coef_*`.

### B — Advance the doses (`simCpCe.R`)

1. Reduce mg/mcg/ng, per-kg, per-hour doses to base units against the drug's concentration unit.
2. Classify each dose as `Bolus`, infusion, or `PO / SL / IM / IN` (the route comes from the unit's suffix via `doseRoute()`, `R/routes.R`).
3. Dispatch to a solver:
   - `advanceClosedForm0.R` — IV, no PK events
   - `advanceClosedForm1.R` — time-varying PK driven by events, including extravascular doses
     (the absorption depot is carried as an amount, which a change in PK set does not touch)
   - `advanceClosedFormPO_IM_IN.R` — extravascular routes (PO, SL, IM, IN, and RA: a local
     anesthetic injected into tissue, absorbed first-order)
   - `advanceClosedFormMetabolite.R` — a drug that forms an active metabolite
4. Sum each dose's contribution over the exponential basis; `convertState.R` carries state
   across event boundaries.
5. Interpolate to an even grid (`equiSpace`), normalize to peak Cp/Ce, and scale against MEAC.

Output per drug: a tidy `Time · Plasma · Effect Site · Recovery` table plus `equiSpace` and `max`.

**The time line.** The closed-form engines are exact at any time, so the points they are asked
about decide only how the curve is drawn and what a run costs. `simulationTimeGrid()`
(`R/simulationTimeGrid.R`) builds the line for all four engines from their knots — every dose
time, the instant `PRE_DOSE_OFFSET` before a dose, the instant a lagged dose was given, any
event — and fills the gaps between them; no knot is ever moved or dropped, which is what keeps
`advanceStatesOnto()` exact. Up to a day (`GRID_LEGACY_MAXIMUM`) each gap gets the 41 geometric
offsets the engines always used, so those plots are unchanged point for point. Beyond a day the
fill starts at `maximum / GRID_FINE_POINTS` after each knot and no step is longer than
`maximum / GRID_UNIFORM_POINTS`, so a washout on a 52-week plot is drawn as a curve, not a
straight chord, and the cost is bounded by the number of doses rather than growing with the
plot (`R/constants.R` records how the two counts were chosen). `doseLines()` lays the doses
on the line. The line depends only on `maximum` in minutes. A drug timed on its plasma looks
for its threshold a week ahead, or as far as the plot runs if that is longer
(`recoveryHorizonPlasma()`).

The exported, Shiny-free entry point for this whole path is `simulateDrugsWithCovariates()` — it
loops drugs, calls `getDrugPK()` → `simCpCe()`, and returns per-drug results. This is what the
vignettes and tests drive.

**The window.** `simCpCe()` covers 0 to `maximum` and nothing else: a dose at or after
`maximum` is dropped before simulating (as the scheduled repeats and the TCI controller always
were), and a lagged extravascular dose's absorption knot past `maximum` is cut from the output
with the recovery states (`clipToWindow()`), so the series, `max` and the normalised curves
never see a peak outside the plot. The app lengthens the plot to the last dose first
(`plotInfo()`), so this matters there only on a plot already at its unit's longest.

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

**Covariate helpers.** `pkSizeFactors()` (`pkSizeFactors.R`) turns weight, height, age and sex
into the fat-free-mass multipliers that most drug models apply to their volumes and clearances
(`ffmAlSallami()` is the Al-Sallami 2015 fat-free mass; see `docs/weight-adjustment.md`);
it is also the lean-body-mass covariate of Aoyama's meloxicam model, in place of the James
equation it was published with. `lbmJames()` computes the James lean body mass, a parabola in
weight that falls past a BMI of 43 (men) or 36 (women), kept deliberately for the Schnider and
Minto companions so that they fail at the extremes of weight as the pumps do (scenario
`james-lean-body-mass`); `renalFunction.R` supplies
Cockcroft-Gault creatinine clearance and de-indexed CKD-EPI eGFR for the renally cleared
models (mannitol, the antibiotics, sugammadex, gabapentin, pregabalin), at the patient's serum creatinine or an
assumed normal one for age and sex when none is entered, with a child's creatinine put on the
adult scale for the equations fitted in adults (`adultEquivalentCreatinine()`); `pregnancy.R` holds the
Patient Profile's Pregnant flag (`checkPregnant()`, `pregnancyPossible()`, the review's findings per drug in
`pregnancyFindings()`), which `getDrugPK()` passes only to the eight models that name `pregnant`, each applying
the factor its header documents from Ansari 2016; `recoveryCalc()` computes
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
  until threshold solved from the combined state; the time-invariant solvers also read the
  effect-site concentration off it, exactly. A drug with no effect site carries its plasma
  amplitudes instead and is timed on its plasma (the antibiotics, against free drug at the MIC:
  `antibioticThresholds.R`).
- `calculateCe.R` — effect-site concentration approximated from a plasma curve; used only by
  the event-driven solver. The `ke0` fit itself
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
- `tci-api.R` — the TCI API's computation, pure R (no Shiny, no plumber): `tciNextRate()` turns
  a parsed request body (covariates, target, pump limits, delivery history) into the next-rate
  response, rebuilding the seven-amplitude model state from the history, projecting it through
  the declared latency, calling `tciRate()`, applying the pump's `maxRate` / `minRate` /
  `rateResolution` and predicting what follows; `tciApiDrugs()`, `tciApiModel()` and
  `tciApiHealth()` are the other endpoints' bodies. Validation signals the classed condition
  `tci_api_error`. Reference: `docs/tci-api.md`.
- `tci-api-server.R` — `tciApiRouter()` builds the Plumber router that serves `/tci/v1`
  (`health`, `drugs`, `drugs/{drug}/model`, `drugs/{drug}/rate`, `openapi.json`) with
  full-precision unboxed JSON and the API's error shape; `run_tci_api()` runs it. Plumber is a
  `Suggests` dependency guarded by `requireNamespace()`, and the router shares nothing with
  `run_app()`. `inst/api/entrypoint.R` (for `rsconnect::deployAPI()`) and the hand-written
  `inst/api/openapi.json` go with it.
- `scheduled.R` — scheduled doses: expands a `qd` / `bid` / `tid` / `qid` dose row into its
  repeats out to the end of the plot (`expandScheduledDoses()`, called first in `simCpCe()`), and
  `exportDoseTable()`, which merges the TCI rows and the repeats into the exported dose table.
- `alternativeModels.R` — the Minto / Schnider switch: Schnider's propofol and Minto's
  remifentanil models (as STANPUMP ran them), the companion-entry builder `recalculatePK()`
  calls, the companion's colour and name-to-parent mapping, and `alternativeModelProblems()`,
  which says when a model is not defined for the patient.
- `suggest.R` — "Suggest Dosing", optimizes a regimen to hit a target effect-site concentration.
- `sendSlide.R` — renders an `officer` PowerPoint deck from `Template.pptx` (the whole plot on
  the first slide, then one slide per panel from `panelPlots()`, which cuts every layer of the
  ggplot down to one facet) and emails it via `emayili`.

**Help — the Help tab**
- `help-content.R` — page registry (`helpStaticPages()`), Markdown rendering via `shiny::markdown()`,
  `help:`/`scenario:` link rewriting, heading ids and table of contents, full-text search.
- `help-investigators.R` — the investigators named in bold on `inst/help/investigators.md`: the registry,
  the links to them in every page and citation (`helpLinkInvestigators()`), and the pop-up about each
  (`helpInvestigatorModal()`, shown on `input$investigator_show`).
- `help-drugs.R` — pages generated from the drug library: each drug model run at six reference
  patients, with its citation, units and thresholds; the drug index; the bibliography.
- `help-scenarios.R` — the teaching-scenario registry (`helpScenarios()`), its validator, the
  generated scenario pages, and `applyHelpScenario()`, which loads one into the simulator.
- `help-ui.R`, `help-server.R` — the nav panel and the server module (`helpServer()`), called
  once from `app_server()`. Content lives in `inst/help/` (see `inst/help/README.md`).

**Util — time & misc**
- `utils-time.R` — clock-time helpers and the time-unit conversions (`displayTimeToMinutes()`,
  `minutesToDisplayTime()`, `rebaseDoseTimes()`, the Max time choices); see [Time units](#time-units).
- `utils.R` — generic helpers only (functions that don't know anything about doses/drugs/etc).
- `drugAndEventDefaults.R` — the memoised drug/event defaults loaders.
- `utils-time-display.R` — writes the engine's minutes in the display time unit, never converting
  the data: the x-axis labels and title, the hover readout (interpolated at the hovered time in the
  drug's full series, Cp for a drug with no effect site), each panel's time-until-threshold labels
  (min / h / d / wk, chosen per panel) and the `Time (<unit>)` column added to the exported sheets.

## App features

- **Target-controlled infusion** (`tci.R`) — a dose row with units `Plasma target` or
  `Effect site target` runs a simulated TCI pump: a rapid loading infusion sized to reach the
  target without overshoot, then a plasma hold. The controller re-plans every 10 s, hands off
  from effect-site to plasma control within 5% of the target (the effect-site solution is
  ill-conditioned at steady state and would alias), and stops on a target of 0 or a manual
  infusion row. Its rate rows never enter the dose table: `simCpCe()` returns them as `$tci`,
  `simulationPlot()` draws them as a per-drug rate panel with the loading dose written as a
  number, and `sendSlide()` merges them into the exported dose table.
- **Minto / Schnider** (`alternativeModels.R`) — a Graph Options box, enabled while propofol or
  remifentanil is in the dose table, that runs Schnider's 1998 propofol model and Minto's 1997
  remifentanil model (the models in most TCI pumps, written as the original STANPUMP ran them)
  beside the library's Eleveld models. Each is a *companion entry* of the drug list
  (`recalculatePK(alternativeModels = TRUE)`), simulated by `processdoseTable()` on the parent's
  doses without recovery, and drawn by `simulationPlot()` as a second line in the parent's panel
  in a derived colour with its own legend entry; under TCI the companion's pump is a second line
  on the parent's rate panel with its loading dose labelled by model. Companions are kept out of
  the MEAC sum, the interaction panel and the opioid-MAC interaction (a second remifentanil line
  is not a second opioid); the hover reports both models and a click goes to the parent. Minto's
  ke0 is the paper's function of age, so `getDrugPK()` accepts a model with `ke0` and no `tPeak`
  and solves the time to peak effect from it. A model whose covariate terms give a non-positive
  volume or clearance for the patient (a James lean body mass far enough below zero to take
  Minto's V1 negative, about -16 kg at age 40; a small child on Schnider) is not built, and the app says so (`alternativeModelProblems()`). Help:
  `inst/help/models/minto-schnider.md`, with a generated parameter table; scenarios
  `eleveld-vs-minto-schnider`, `tci-eleveld-vs-minto-schnider` and `james-lean-body-mass`; tests `test-minto-schnider.R`.
- **The TCI API** (`tci-api.R`, `tci-api-server.R`, `inst/api/`) — a stateless HTTP API under
  `/tci/v1` that returns the next infusion rate for one TCI drug in one patient from the
  controller's record of what the pump has delivered (the STANPUMP principle: the model state is
  re-derived from the delivery record on every request, so nothing is lost and every request is
  a reproducible record). It uses the same `tciRate()` as the TCI rows, and adds the pump's
  `maxRate` / `minRate` / `rateResolution`, a projection through the declared latency, a bounded
  segment with a `fallback` rate and a hard stop at `holdUntil`, and predictions (peak, time to
  target, decrement time, syringe empty). It is not deployed with the Shiny app (shinyapps.io
  does not host Plumber): `run_tci_api()` or `rsconnect::deployAPI()` serve it. The normative
  reference is `docs/tci-api.md`.
- **Scheduled doses** (`scheduled.R`) — a bolus, PO, SL, IM or IN unit with a frequency suffix
  (`mg PO bid`) gives the dose at the entered time and then every 24 / 12 / 8 / 6 h (qd / bid /
  tid / qid) until the end of the X axis. A scheduled dose of 0 for the same route stops the
  sequence; a later non-zero one replaces it. Like the TCI rows, the repeats never enter the dose
  table: `simCpCe()` returns them as `$scheduled` and `sendSlide()` merges them into the export.
  The frequencies are offered per drug in `drugDefaults_global.csv`.
- **Dose-dependent oral absorption** (`oralSaturationFraction()` in `routes.R`) — a drug whose
  oral bioavailability depends on the dose returns an `oralSaturation` block, and `simCpCe()`
  scales each oral dose before the engine runs: by `1 - Imax × D / (ID50 + D)` when absorption
  saturates (gabapentin), by `D / (D50 + D)` when it rises with dose (`form = "rising"`,
  sertraline), or by `(D / Dref)^exponent` to carry an empirical dose power on apparent
  clearance (`form = "power"`, paroxetine). Each dose is then an ordinary input, so the engines
  stay linear; dependence shared between overlapping doses is not represented.
- **Suggest Dosing** (`suggest.R`) — given a target drug and end time, optimizes bolus +
  infusion amounts to reach and hold a target concentration. The fit is over the effect-site
  concentration alone, evaluated as a sum of each row's unit-dose curve (the engine is linear);
  every rate change is inside the window, and the regimen ends with one zero-rate row at the end
  time. The dialog offers only `suggestDrugChoices()`: an effect site and IV bolus and infusion
  units.
- **Email a slide** (`sendSlide.R`, `Template.pptx`) — builds a branded PPTX from the current
  simulation and mails it: the plot on the first slide and each panel on a slide of its own,
  the dose table, and a URL that reconstructs the exact state.
- **Editors & modals** (`app_server.R`) — in-app Drug Library and Drug Thresholds editors, plus
  click-to-add-dose / double-click-to-edit driven from plot coordinates.
- **URL bookmarking** (`app_globals.R`) — `enableBookmarking = "url"` encodes inputs into a
  shareable link; `bookmarksToExclude` keeps transient UI state out.
- **Deep links** (`deep-links.R`) — `?scenario=<id>` loads a teaching scenario and
  `?help=<page-id>` opens the Help tab on a page, both read once at startup and both skipping
  the startup drug menu; see `inst/help/sharing.md`.
- **Debug & profiler** (`app_globals.R`, `app_server.R`) — `?debug=1` reveals a live log
  (`outputComments()`) and a per-reactive profiler (`profileCode()`).
- **Front-end assets** (`inst/www/`) — `app.css`, `app.js`, `hot_funs.js` (Handsontable
  copy/paste hooks and drug-default injection into the client).
- **Help** (`R/help-*.R`, `inst/help/`) — the in-app help: Markdown pages plus pages generated
  from the drug library and the teaching scenarios, with search and one-click scenario loading.
- **Config** (`config.yml`, `app_run.R`) — environment-specific title and debug flag,
  merged over `DEFAULT_CONFIG` at launch.
- **Reproducibility** (`renv.lock`, `DESCRIPTION`) — `renv.lock` pins exact package versions so
  production matches local; deps declared in `DESCRIPTION`.
- **Tests / CI** (`tests/testthat/`, `.github/`) — one test file per drug and per R file;
  R-CMD-check and shinyapps.io deploy run via GitHub Actions. The production deploy starts
  only when R-CMD-check has passed on the pushed commit and installs that commit by SHA;
  the deploy writes that SHA into `config.yml` as `commit`, and the app reports the version and
  commit (`stanpumpRBuild()`) in the debug log and the Source link's tooltip.
