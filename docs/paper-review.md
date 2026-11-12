# Single Paper Review

A reader or peer reviewer uploads one pharmacokinetic manuscript, Claude reads
it, stanpumpR checks the model it describes, and the paper's drug can then be
simulated. This is the scaled-back first version (Claude Code,
2026-10-30). It is a tab of stanpumpR, **Single Paper Review**, between
Simulator and Help (Shafer, 2026-11-01), and also runs as an app of its own.

Run it locally:

```r
devtools::load_all(".")
Sys.setenv(ANTHROPIC_API_KEY = "...")    # or anthropic_api_key in config.yml
shiny::runApp(run_app())                 # the tab
shiny::runApp(run_paper_review_app())    # the app on its own
```

Without a key on the server the tab asks the user for their own (it is held
only in that session) and keeps uploading off until one is entered; the
example still works. A deployment without `anthropic_api_key` therefore sends
nothing unless a user brings a key.

**Try the example** works without a key and sends nothing: it loads
`inst/paper-review/example-extraction.json`, a fictional paper with a
deliberate misprint.

Privacy: `docs/paper-review-privacy.md` (draft for legal review). User-facing
help: `inst/paper-review/help.md`, shown in the app's "Privacy and help" tab.

## How it works

| Step | Code | What it does |
|---|---|---|
| Upload | `paperReviewReadUpload()` | Reads the PDF into the session's memory and deletes Shiny's temporary file |
| Extract | `paperReviewExtract()`, `R/paper-review-api.R` | One streamed call to the Anthropic Messages API, `claude-opus-5-5`, adaptive thinking at effort `high`, the PDF inline as base64, the JSON schema (`paperReviewSchema()`, `R/paper-review-schema.R`) in the instructions and the reply checked on return (`paperReviewValidate()`). No Files API, no caching, server-side refusal fallback on |
| Review | `paperReviewRun()`, `R/paper-review-checks.R` | The checks below, all in R |
| Report | `paperReviewReportTags()`, `R/paper-review-report.R` | HTML; every finding cites where in the paper it comes from and says whether it was computed or is Claude's reading |
| Simulate | `paperReviewDrug()`, `paperReviewSimulate()`, `R/paper-review-model.R` | Builds the paper's drug in memory (a model list and a one-row defaults frame, never a `drugs_*.R` file or CSV row) and runs it through `getDrugPK(model = ...)` and `simCpCe()` |
| Discard | `paperReviewDiscard()` | On request and at session end |

### The extraction

The schema is closed wherever R has to compute: parameter roles (`CL`, `V1`,
`Q2`, `k10`, `ka`, `F`, `tlag`, `ke0`, `tPeak`, ...), units (a fixed list that
`paperReviewUnit()` converts), covariates (weight, height, age, sex, BMI,
James LBM, Al-Sallami FFM are evaluated; renal and other covariates are
recorded and held at reference) and covariate forms (power, linear,
exponential, category multiplier; anything else is copied as printed and not
evaluated). **Nothing Claude returns is ever evaluated as code.** Each value
is kept as printed (`value_text`) as well as a number, so a recomputed value
can be judged against the printed precision.

The manuscript is treated as data: the instructions tell Claude to ignore any
text in it that addresses the model (a manuscript is untrusted input).

### The checks

- **Identifiability** (`paperReviewIdentifiability()`), after
  `docs/antihypertensives.md` section 4. A model with *n* compartments has 2*n*
  disposition parameters. If the paper prints them (volumes and clearances, or
  V1 and micro constants), it is closed directly. Otherwise the derived routes
  are tried: one compartment from CL or V with the half-life; two from CL, V1
  and both half-lives (`twoCompartmentFromCentralVolume()`), CL, Vss and both
  half-lives (`twoCompartmentFromHalfLives()`), or the biexponential
  (`twoCompartmentFromBiexponential()`, new: the two-exponential counterpart of
  the existing triexponential helper); three from the triexponential
  (`threeCompartmentFromTriexponential()`). A closed route is then solved again
  at every corner of its inputs' printed uncertainty (3^k corners, ±1 SE; an
  assumed ±5% where no SE is printed, which the report says): a corner with no
  admissible model, or a parameter spreading more than twofold, makes it
  "formally closed but ill-conditioned", the esmolol case. CL, Vss and the
  terminal half-life alone are reported as underdetermined, the hydralazine
  case.
- **Internal consistency** (`paperReviewConsistency()`): every printed derived
  quantity is recomputed from the printed parameters through `getDrugPK()`
  (eigenvalues by `cube()`, coefficients, ke0), `effectSitePeakTime()` and,
  for Cmax and tmax, `simCpCe()`; AUC(0-inf) as the bioavailable dose over CL.
  A value within its printed rounding or 1% "matches", within 5% is "close",
  within 20% "differs", beyond that "disagrees". Cmax and tmax printed as means
  of individuals are compared with the typical subject's curve and flagged as
  such.
- **Covariates** (`paperReviewCovariateChecks()`): each equation is evaluated
  for every legal patient (age 0-90, 3-200 kg, 50-200 cm, both sexes); a
  multiplier that is not finite and positive is an error ("no infinity, no
  division by zero"). A negative power of age is flagged under the age-floor
  rule. Reference values are checked against the studied range.
- **Assumptions and the library's rules** (`paperReviewAssumptionChecks()`):
  dose basis (salt or base), bioavailability fitted to another study's
  exposure (an error), apparent parameters, non-linear elimination or
  absorption the in-memory simulator cannot represent, volumes read as anatomy
  ("only clearance is real"), and every reported difference of two means
  tested with `sqrt(se1^2 + se2^2)`, with the implied correlation when the
  paper prints the difference's own SE ("a difference of two measured numbers
  is a fraught measurement").
- **Figures** (`paperReviewFigureChecks()`): each concentration figure's
  regimen is simulated for the reference patient and its digitised points
  compared: median prediction error and median absolute prediction error,
  with the caveat, on every figure, that the points are Claude's approximate
  reading and that a mean of individuals is not the typical subject.
- **Claude's reading**: the reviewer notes Claude returns, shown separately
  and labelled as not computed.

The **reference patient** is each covariate's printed reference value, else
the population's central value, else stanpumpR's default (70 kg, 170 cm, 40
years, male); the report says which for each.

### Testing

`devtools::test()` never touches the network: `test-paper-review-{api,checks,model,privacy}.R`
run against fixtures whose answers come from an independent `eigen()`
calculation (`tests/testthat/fixtures/make-paper-review-fixtures.R`).

`test-paper-review-live.R` calls the real API and is **opt-in**: it is
skipped unless a key is set and `STANPUMPR_LIVE_API=1`. It sends no real
manuscript. It draws a one-page PDF of the fictional exampleol paper and
checks that Claude's extraction reaches the review intact, catching the
misprinted half-life. First run, 2026-10-30: passed, 1.3 minutes, 9,743
input and 9,095 output tokens (about $0.22 at list price). A real paper is
sent only when Shafer supplies it (Open decision 6).

**How long a review takes** (the user's estimates are in
`inst/paper-review/help.md`, *How long a review takes*, and the busy banner
in `paperReviewHeader()`; revise both together as reviews are timed):
- the one-page exampleol paper: 1.3 minutes (2026-10-30, above);
- Devin's five live reviews (2026-11-08): a second visitor waited 169 s
  behind one, so that review took at least three minutes; the others were
  not timed;
- Hallik 2020 (PMID 31657867; 11 pages, one PK model and six PKPD models,
  five parameter tables), on production with the operator's key
  (2026-11-10): still being read when the browser stopped watching at
  20 minutes, so it likely reached `PAPER_REVIEW_TIMEOUT_SEC` (1200 s)
  without a review; its outcome and tokens are in the usage email and the
  server log. The same upload with an invalid key returned 401 in 2 s, so
  the background path on shinyapps.io works and the time was Claude's.

The first two live attempts were refused, which is why the schema is sent
in the instructions rather than as a structured output
(`output_config.format`): the API allows at most 16 nullable or union-typed
fields (this schema had 42), and once those were removed it still refused
the schema because "the compiled grammar is too large". The reply is
checked on return instead (`paperReviewValidate()`).

### Extravascular routes, and parameters from the curves

Asked for by Shafer on 2026-10-30, after the first real test paper printed
F, the mean absorption time and tmax for two oral
formulations of a prodrug, but no ka: "Other routes should be captured and
simulated as well", and "the app should consider deriving parameters from
displayed curves when possible". `R/paper-review-absorption.R`:

- **Every extravascular regimen gets its own absorption**
  (`paperReviewAbsorptions()`), so a solution and a capsule each keep their
  own F and ka, and the simulator uses the selected regimen's. First order is
  assumed throughout, and the report says so.
  - **ka**, in this order: printed; ln 2 / a printed absorption half-life;
    1 / (MAT − tlag); 1 / (the oral MRT − the model's Vss/CL − tlag); fitted
    to the regimen's digitised points; solved so that the model's tmax equals
    the printed one.
  - **F**: printed; else the paper's own dose-normalised oral AUC over its
    intravenous one (a crossover ratio from the same study, which satisfies
    "bioavailability is read, never fitted to another study's exposure");
    else 1 for apparent parameters; else fitted with ka to the points.
  - **The dose basis** is `analyte_fraction` on the dose, taken only from the
    paper's own statement (a prodrug salt dose stated to be equimolar to so
    much of the analyte). It rides on the
    bioavailability, so a dose is entered as the paper doses it.
- **No double counting.** A printed quantity an absorption was derived from
  (the AUC behind F, the tmax or MRT behind ka) is marked "used to build the
  model" in the consistency table, not "checked". A figure series the
  absorption was fitted to is set aside in the figure check. When ka is
  printed and the regimen also has points, the fit is reported as a
  cross-check.
- **Exponentials from an intravenous curve** (`paperReviewFitExponentials()`):
  1, 2 and 3 exponentials are fitted, on log concentrations, to each
  intravenous series (a bolus, or a short infusion with its infusion points
  dropped). The report says which the lowest AIC prefers against the model's
  compartments, as a lead rather than a finding, since the points are read
  by eye. When the printed numbers do not close the model, the fitted
  exponentials close it through the same helpers (verdict
  `derived_from_curve`, reported as an approximation).

### Several models in one paper, the tables' rounding, and decrement times

From the Duzitol editorial (Shafer and Stanski, Anesthesiology
1992;76:327-30), whose Table 1 gives three models side by side (a
fictional drug in normal subjects and in hepatic failure, and propofol) as
three half-lives, metabolic clearance, V1 and Vdss. Shafer, 2026-11-01:
"I think your math was wrong on the Duzitol paper", with the spreadsheets
he built the models with.

- **Several models.** Every parameter, derived quantity, regimen and
  figure series carries a `group` (the model it belongs to; empty when the
  paper has one). `paperReviewRun()` reviews each model on its own items
  (`paperReviewGroupExtraction()`), and the paper's assumptions, claims and
  Claude's notes once. A parameter printed with different values is a
  conflict only within one model. The simulator has a Model selector.
- **Three compartments from CL, V1, Vss and the three half-lives**
  (`threeCompartmentFromHalfLives()`), Shafer's spreadsheet algebra,
  reproduced to every printed digit (derivation below).
- **The rounding of the printed inputs.** A derived quantity that does not
  match at the model's centre is recomputed at every corner of the inputs'
  rounding (each printed input at half a unit of its last digit either way,
  2^k corners) and reported "matches within rounding" when the printed
  value lies inside that range. The editorial's steady-state 50% decrement
  times, 41, 10 and 20 min, lie inside the 39.1-43.6, 9.6-10.8 and
  17.3-26.8 min that Table 1's rounding allows, though propofol's centre is
  21.8. The conditioning check likewise uses the printed rounding when a
  paper prints no standard error, rather than an arbitrary 5%. A "matches
  within rounding" counts as compared (it once did not, which printed
  "8 of 6 compared").
- **A printed decrement time with no duration is a steady-state one**, the
  limit after an infusion of unlimited length, and the table says so with
  how long a TCI infusion takes to come within 5% of it
  (`paperReviewSteadyStateNote()`). The limit can be far off: hepatic
  failure's 9.9 min is 8.2 min after 10 h and is approached only after 20 h.
- **Decrement times assume TCI** (Shafer: "calculating decrement times
  requires an assumption of TCI administration, maintaining a steady
  concentration in the plasma ... or the effect site"). For a plasma
  target, `heldInfusionDecrementTime()` (`R/held-infusion.R`, the library's
  one copy, which the Duzitol help page also uses) holds C1 constant, so
  each peripheral concentration rises as C(1 − e^(−k21·T)), and lets the
  amounts decay freely from there, with `cube()`'s exponents; it is held to
  `simCpCe()`'s own plasma-target TCI by `test-held-infusion.R`. For an
  effect-site target, `paperReviewEffectSiteDecrementTime()` runs
  stanpumpR's own effect-site TCI through `simCpCe()`. A duration of `Inf`
  is the steady state. A figure of decrement time against infusion
  duration (the editorial's Figure 1) is a `decrement_time` figure and is
  checked point by point.

#### The derivation behind the spreadsheet

Shafer derived this decades ago and asked for it to be written down. For a
three-compartment mammillary model with micro constants k10, k12, k13, k21,
k31, the exponents λ1, λ2, λ3 are the roots of the characteristic
polynomial s³ + a2 s² + a1 s + a0, whose coefficients are the elementary
symmetric functions of the λs:

    a2 = λ1 + λ2 + λ3          = k10 + k12 + k13 + k21 + k31
    a1 = λ1λ2 + λ1λ3 + λ2λ3    = k10 k21 + k10 k31 + k21 k31 + k12 k31 + k13 k21
    a0 = λ1 λ2 λ3              = k10 k21 k31

The table gives k10 = CL/V1, the three λ = ln 2 / t½, and Vss, which in a
mammillary model is V1 (1 + k12/k21 + k13/k31). Four unknowns remain: k12,
k13, k21, k31.

1. **From a0**, the product of the return rates:
   k21 k31 = λ1λ2λ3 / k10 = **g0**.
2. **From Vss**, Vss/V1 − 1 = k12/k21 + k13/k31 = (k12 k31 + k13 k21) / (k21 k31), so
   k12 k31 + k13 k21 = (Vss/V1 − 1) g0 = **g1**. (The spreadsheet computes this
   twice, as g1 and as h; they are the same number.)
3. **From a1**, which contains both: a1 = k10 (k21 + k31) + k21 k31 + (k12 k31 + k13 k21),
   so the sum of the return rates is k21 + k31 = (a1 − g0 − g1) / k10 = **f**.
4. **k21 and k31** therefore have sum f and product g0: they are the two
   roots of x² − f x + g0 = 0, (f ± √(f² − 4 g0)) / 2, the larger taken as
   k21 so that compartment 2 is the faster. A real model needs f² > 4 g0.
5. **From a2**, k12 + k13 = a2 − k10 − k21 − k31 = **i**.
6. **k12 and k13** solve the linear pair k12 + k13 = i and k12 k31 + k13 k21 = g1:
   substituting k12 = i − k13 gives **k13 = (g1 − i k31) / (k21 − k31)**, and k12 = i − k13.
7. **The coefficients** of the unit-bolus plasma response are the residues
   of its Laplace transform, (s + k21)(s + k31) / (V1 (s + λ1)(s + λ2)(s + λ3)):
   Cj = (k21 − λj)(k31 − λj) / (V1 ∏m≠j (λm − λj)), the spreadsheet's C_1, C_2, C_3.

The model is admissible when the discriminant is positive and k12, k13,
k21, k31 all come out positive; otherwise the six printed numbers describe
no three-compartment model (`threeCompartmentFromHalfLives()` refuses
them). The two-compartment case is the same argument with one return rate:
k21 = λ1λ2 / k10 and k12 = λ1 + λ2 − k10 − k21, and Vss then over-determines
the model rather than being needed, which is why two compartments close
from CL, V1 and both half-lives (`twoCompartmentFromCentralVolume()`).

### Every parameter of the model (Convert.xls)

Shafer's Convert.xls (last updated 12/02) closes a one-, two- or
three-compartment model from five sets of inputs, one tab each, and lists
every other parameter. The review closes a model by the same five routes:

| Tab | Inputs | Review route |
|---|---|---|
| 1 | volumes and clearances | printed |
| 2 | V1 and the micro rate constants | printed |
| 3 | V1, Vdss, CL and the half-lives | `threeCompartmentFromHalfLives()`, `twoCompartmentFromCentralVolume()` |
| 4 | coefficients and exponents | `threeCompartmentFromTriexponential()`, `twoCompartmentFromBiexponential()` |
| 5 | V1, the exponents, k21 and k31 | `compartmentsFromExponentsAndRates()` (`R/paper-review-parameters.R`) |

The fractional coefficients A(i)/ΣA(i), often printed as percents, never
close a model: they are always derived parameters, never primary ones
(Shafer, 2026-11-01). They sum to 1, so they are redundant, and without V1
they lose the scale. The consistency table compares a printed percent
coefficient with the model's own A(i)/ΣA(i). `test-paper-review-parameters.R` pins every
tab's own example to the digits the spreadsheet computes.

Once the model is complete, `paperReviewPKTable()` lists what each tab
lists: V1..Vn and Vdss, the clearances, the micro rate constants, the
exponents and half-lives, the coefficients of the unit disposition function
(per litre), the fractional coefficients (%), and ke0, its half-life and the
time to peak effect where there is an effect site. The report prints it for
each model at the reference patient. The simulator prints it for the patient
entered, in the selected time unit: the covariates act on the volumes and
clearances (Shafer's preference, and how the papers apply them), and
everything else follows from those.

A decrement-time figure read by eye is judged against the reading's
precision as well as by relative error. A curve read to ±3 min is off by
about 3 min wherever it is read, so a short decrement time can carry a large
relative error that says nothing about the model. The Duzitol editorial's
30% curve, 2.5 to 12.5 min, came out −31% that way. The precision is Claude's
`reading_precision`, else a ± in its digitisation note, else 2% of the
printed axis (`y_axis_max`, else an axis range such as "(0–50 min)" in the
digitisation note), else 2% of the largest value read anywhere on the
figure. A figure Claude extracts as one entry per curve ("Figure 3 (10%)",
"(20%)"...) shares its axes, so it shares the precision, and so do the
curves of the paper's other models (Figure 1 plots three: each model's part
of the extraction keeps the whole figure as `paperFigures`). Measured on
one curve alone, the 10% curve, near zero on a 0–120 min axis, got ±0.04 min
and failed, and Figure 1's three curves got ±0.82, ±0.2 and ±0.38 min. Once
the precision is known it decides the verdict (median distance within one
precision: ok; within three: concern), not the median relative error: a
median error of 19% passed the propofol curve, every point of which lay 1–3
min above the model. A point also agrees when the model's curve reaches its value
within the precision of the duration read (`x_axis_max`, else 2% of the
longest duration read). That is what a steep curve needs: the 80% and 90%
curves leave the axis almost at once, so a slight misreading of the
duration is a large one of the decrement time.

A concentration figure normalized to its own peak (`y_normalization`
`percent_of_peak` or `fraction_of_peak`) does not depend on the dose,
because compartmental models are linear in dose, and Shafer uses such
figures for that reason. The model's curve is divided by its own maximum the
same way. The curve is the figure's regimen when it names one that can be
simulated, otherwise a unit intravenous bolus, at the plasma or the effect
site (`concentration_site`). Dividing by the maximum rather than by the sum
of the coefficients matters at the effect site, whose peak after a bolus is
later and far lower than the plasma's.

Each figure finding's title names the curves it compares. One figure with
three models' curves is checked once per model, and "Figure 1: reproduces"
beside "Figure 1: does not reproduce" read as a contradiction.

### What the in-memory simulator does not do (yet)

It is linear, with first-order absorption: no Michaelis-Menten elimination, zero-order or
transit absorption, metabolite, or PD beyond an effect site (each is flagged
in the report when the paper has it). Renal and other covariates are held at
their reference. It is a small simulator of its own, not the full stanpumpR
UI: putting the paper's drug into `app_server()` needs seams in
`recalculatePK()`, `processdoseTable()` and `simulationPlot()`, which read the
library by drug name, and that is deferred with the rest of the master
integration.

## Touches to shared code

Everything else is in new files. These are the seams in shared code:

1. `R/getDrugPK.R`: the unknown-drug check is skipped when a `model` is
   supplied (`if (is.null(model) && !drug %in% drugList)`). One line; every
   existing caller passes either no model or a model for a library drug, so
   nothing else changes.
2. `NAMESPACE`: `export(run_paper_review_app)`, and its `man/` page.
3. `DESCRIPTION`: `httr2 (>= 1.1.0)` in `Suggests` (the streaming
   functions `paperReviewExtract()` uses arrived in 1.1.0), used only by this optional entry
   point and guarded with `requireNamespace()` (the rule `AGENTS.md` gives for
   plumber). It is already in the renv library as a dependency of other
   packages, so `renv.lock` is unchanged.
4. `R/app_ui.R`: `paperReviewNavPanel()` between the Simulator panel and
   `helpNavPanel()`. The review's own three tabs (Review, Simulator, Privacy
   and help) sit under it, its inputs in the namespace `paperReview`
   (`PAPER_REVIEW_MODULE_ID`).
5. `R/app_server.R`: `paperReviewModuleServer()` beside `helpServer()`, and
   `onBookmark()` excludes every `paperReview-` input from the bookmark, so
   nothing of a reviewed paper (its model names, the patient and doses
   simulated) reaches the URL; the exclusion is set before every
   `doBookmark()`, which reads it before calling `onBookmark()`. The review's computations run with
   stanpumpR's comments off (`paperReviewSilently()`), so nothing reaches
   the `?debug=` log either.
6. `R/app_run.R`: `shiny.maxRequestSize` raised to
   `PAPER_REVIEW_MAX_UPLOAD_BYTES` (the 20 MB PDF limit plus room for the
   request's encoding), since Shiny refuses anything over 5 MB before the app
   sees it. The option is global, so the simulator's own uploads may be as
   large.

New files: `R/paper-review-{schema,api,model,absorption,checks,report,app}.R`,
`inst/paper-review/{example-extraction.json,help.md}`,
`tests/testthat/test-paper-review-*.R`, `tests/testthat/fixtures/`,
`docs/paper-review.md`, `docs/paper-review-privacy.md`. The help page is in
`inst/paper-review/`, not `inst/help/`, because every page under `inst/help/`
must be registered in `helpStaticPages()` (`test-help-content.R`), which would
be another seam.


## Deferred (Shafer, 2026-10-30)

- Linking to the paper archive, and overlaying the uploaded model on library
  drug models and archived papers.
- ~~Merging into master~~: done, as a tab of stanpumpR (Shafer, 2026-11-01).
- ~~Bring-your-own-key~~: done (2026-11-10), see "Setting up the API key".
- ~~Running the API call off the main R thread~~: done (2026-11-10), before
  the key was deployed. The request is built in the app's process, sent and
  streamed in a short-lived background R process (`mirai`, through
  `paperReviewExtractAsync()` and the server's `ExtendedTask`), and read back
  in the app's process; `paperReviewTransport()`, the part that runs in the
  background, calls nothing of stanpumpR (tested). The background process
  is given `PAPER_REVIEW_BACKGROUND_TIMEOUT_SEC`, and is started with the
  app's library paths, without which it waits for ever for a package it
  cannot load.

## Devin's stress test (2026-11-08)

Devin (Cognition AI), for Charles Minto, ran master `a64eb29` on its own machine with Charles's own key (five live reviews, the key revoked after), fuzzed 400 extractions offline, and read the code. Every claim was checked before anything changed.

| Item | Verdict | Change |
|---|---|---|
| 1. With no key, a visitor uploads and consents before learning that only the example runs | Confirmed: the key was looked up only on Review | The tab says so at the top, and the consent box, uploads and Review stay off |
| 2. A review blocks every session on its R process; no per-visitor limit | Confirmed by its measurement (a second visitor waited 169 s); already the reason for *Deferred* above | The token counts are now shown after a review. The call moved off the main thread on 2026-11-10, before the key was deployed |
| 3. False errors when a paper tabulates values at other covariates (18 of 19 in a paediatric paper) | Confirmed: every derived quantity was compared at the reference patient | `derived_quantities` carry `at` (weight, height, age, sex); the check rebuilds the model there, and such a value never closes or fits the model at the reference |
| 4. One malformed field loses the paid review (175 of 400 fuzzed) | Reproduced exactly: 225 of 400 rendered | `paperReviewConform()` conforms every field to the schema; five checks read an absent or out-of-choice field unguarded; each check runs on its own. 1,600 fuzzed extractions over four seeds all render, none needing the last guard |
| A 2,000-fold figure miss reported as the paper's error | A misreading is likelier | A miss beyond tenfold is a concern saying so |
| A non-PK document gets a full review shell | Confirmed | One line and Claude's notes |
| "0.0 MB" for a 30 kB upload | Confirmed | kB below 1 MB |

The reference patient Devin called "a mixture" (70 kg, the printed allometric reference, with the population's 110 cm and 6.5 years) is by design: each covariate is taken at the value at which the paper's parameters are its typical values. It is not a patient to simulate, which is why the report gives the source of each number.

## Setting up the API key

**Current setup (Shafer, 2026-11-10):** "I want the Single Paper Review to
access my personal Anthropic API key for now. I have added the key to the
GitHub repository secrets. Please revise Single Paper Review to default to my
own key. Please track usage and let me know how often it is used. I'll
probably force users to use their own keys after 200 uses of my key."

So there are two keys:
- **The operator's key**, every user's default. `paperReviewApiKey()` looks
  for it in the environment variables `STANPUMPR_ANTHROPIC_API_KEY` and
  `ANTHROPIC_API_KEY`, in that order, then `anthropic_api_key` in
  `config.yml`, unless `paper_review_shared_key: false` is set there.
- **A user's own key**, entered in the tab ("Use your own Anthropic API
  key"), which wins when it is entered. It is held only in that session's
  input, sent only to Anthropic in the request header, never stored, logged
  or bookmarked, and the review is then billed to the user's account.
  `paperReviewOwnKey()` accepts only text shaped like a key (`sk-ant-`), so
  a mistyped key is said to be wrong and never sent.

**Counting the operator's uses** (`R/paper-review-usage.R`). Every review
sent with the operator's key that reached the API is reported, with only the
coarse metadata `docs/paper-review-privacy.md` section 1a allows (the time,
the token counts, the duration, the outcome code):
- one line on the server log, `stanpumpR Single Paper Review: Time: ...`
  (`rsconnect::showLogs()`);
- one email to the operator, sent from a background process, whose subject is
  always **"stanpumpR Single Paper Review: shared key used"**: searching the
  mailbox for that subject counts the uses. It goes to
  `paper_review_usage_email` in `config.yml`, or else to the app's own mail
  account (`email_username`), and is not sent without the mail credentials.

The count the app itself prints is for its R process only: shinyapps.io keeps
no disk, and the process restarts when the app sleeps or is redeployed. The
mailbox, and the Anthropic Console's usage page for the key, are the running
totals.

**The limit** (Shafer, 2026-11-10: stop after 200 papers, so that free access
cannot be used to run up thousands of dollars). Before each review with the
operator's key, `paperReviewSharedKeyAdmit()` counts the usage emails in the
mail account's **Sent folder** over IMAP (`[Gmail]/Sent Mail`, searched for the
subject above, from a background process), which survives restarts and
redeploys and is shared by every process, and adds the reviews this process
has started whose email has not yet gone. Past
`PAPER_REVIEW_SHARED_KEY_LIMIT` (200; `paper_review_shared_key_limit` in
`config.yml` changes it) the key is not used: the user is asked for their own,
and that R process offers the key to no new session. A count that cannot be
read refuses the key too (the limit fails closed), so an IMAP outage means
users bring their own keys until it passes. Without the mail credentials
there is no mailbox to count, and the limit holds per R process only. What
it cannot see: another process's reviews still in progress, so a burst of
simultaneous reviews on several processes can pass the limit by a few. Do not
delete the usage emails from the Sent folder, which is the count; to allow
more reviews, raise the limit. **Set a monthly spend limit on the key's
workspace in the Anthropic Console as well**: that ceiling holds whatever
happens in the app. A review costs about $0.80 at effort `high`, and at most
about $2.75 (128,000 output tokens).

**On shinyapps.io** (`.github/workflows/shiny-deploy-production.yaml`):
the production deploy writes `anthropic_api_key` into the deployed
`config.yml` from the first of these secrets that is set:
`PAPER_REVIEW_ANTHROPIC_API_KEY`, `STANPUMPR_ANTHROPIC_API_KEY`,
`ANTHROPIC_API_KEY` (a repository secret or one of the `production`
environment). Two optional Actions **variables** (Settings → Secrets and
variables → Actions → Variables):
- `PAPER_REVIEW_USAGE_EMAIL`: where each use is reported (the limit counts the
  sending account's Sent folder, wherever the emails are delivered);
- `PAPER_REVIEW_SHARED_KEY`: set it to `false` to withdraw the operator's
  key (users must then enter their own), then re-run the production deploy
  (Actions → Deployment Production → Run workflow).

The run's summary says whether the shared key was set, never its value. The
key then sits in the app's bundle on Posit's servers, as the email password
already does. The PR preview is never given it (`shiny-pr-2-deploy.yaml`
runs a PR's own code), so a preview reviews only with a key a user enters.
Revoking the key in the Console stops it everywhere; make a new key, update
the secret and redeploy to rotate it.

**Locally**, put the key in the user-level R environment file, not in the
project (this repository does not git-ignore a project `.Renviron`):
`file.edit("~/.Renviron")`, add `STANPUMPR_ANTHROPIC_API_KEY=<the key>` (or
keep the existing `ANTHROPIC_API_KEY`), restart R, and check without printing
it: `nzchar(Sys.getenv("STANPUMPR_ANTHROPIC_API_KEY"))`. Alternatively put
`anthropic_api_key: "<the key>"` in the worktree's git-ignored `config.yml`.

**A separate key for this app** is still worth making before wider use, so
that its spend shows apart from IntegrityAnalysis's and it can be revoked on
its own: in the Anthropic Console, a workspace for this app (for example
"stanpumpR Paper Review"), which can carry its own spend limit, and a key in
it named for it. Check in the same account's settings that the organisation
has not opted in to sharing data for training and is not in the Development
Partner Program: Shafer's rule, "Claude is not allowed to learn from my
models", depends on it (`docs/paper-review-privacy.md` section 1a).

## Open decisions

Questions only Shafer can answer, written here as they are asked
(`AGENTS.md`, session coordination, rule 2). Each is `[decision]` until
answered, then `[decided]` with his words.

Shafer, 2026-10-30, relayed by the parent session: "I have a "professional"
account at shinyapps.io. That provides sufficient bandwidth for the
foreseeable future. I'll look into the privacy issues further. Claude is not
allowed to learn from my models. Claude is allowed to see coarse metadata."

1. `[decided]` **An API key: yes.** Shafer, 2026-10-30, relayed by the parent
   session: "Yes. It is used by IntegrityAnalysis. Not sure where it is." He
   has an Anthropic Console account, the one his IntegrityAnalysis app
   already bills to. A **separate key for this app**, named for it, would let
   its spend be seen and the key be revoked on its own; the steps are in
   "Setting up the API key" below. No session looks for
   the IntegrityAnalysis key or reads it from that app's files or
   environment.
   **Current setup (2026-10-30):** Shafer: "C:\Users\steve\OneDrive\Documents\.Renviron
   is where you will find my API key". That file is R's user environment file
   on his machine (R's HOME), so R loads `ANTHROPIC_API_KEY` at startup and
   the app needs nothing more for local testing. It is **the same key
   IntegrityAnalysis uses**, so the two apps' usage shows together in the
   Console. **Sharing it is Shafer's deliberate choice during development**:
   "That's fine for now. Both are in development, with low usage."
   (2026-10-30, relayed by the parent session.) A separate key is something
   to revisit before wider use, not a blocker. Sessions never open, print,
   copy or edit that file, and never
   write the key into the worktree, `config.yml`, a test, a fixture, a log
   or a commit; to confirm it loaded they check only
   `nzchar(Sys.getenv("ANTHROPIC_API_KEY"))`.
   At list price a review costs roughly the manuscript's tokens at $4 per
   million in and the extraction and thinking at $20 per million out: a
   20-page paper is perhaps 40-80 thousand input tokens and 10-30 thousand
   output, so on the order of $0.50-$1 a review. Not measured; to be measured
   on the first real paper.
2. `[decision]` **Zero data retention, and privacy generally.** Request ZDR
   from Anthropic for the organisation the key belongs to, given peer-review
   use? Without it, Anthropic keeps inputs and outputs for up to 30 days
   (longer if flagged). ZDR covers inline PDFs on the Messages API, which is
   all this app uses. Also open under this heading: `inference_geo` (`"us"`
   keeps inference in the United States at 1.1x the price; there is no EU
   option). (Asked 2026-10-30. Still open: Shafer is looking into the privacy
   issues himself, so `docs/paper-review-privacy.md` stays a draft, kept
   accurate rather than final.)
3. `[decided]` **Hosting: shinyapps.io, on Shafer's professional account**
   (2026-10-30, quoted above), whose bandwidth he judges sufficient for the
   foreseeable future. What that means for the design:
   - No reliance on persistent disk, which shinyapps.io does not keep across
     instances. The app needs none: uploads, plots and downloads use
     temporary files that are deleted at once (the upload deletion is
     tested), and everything else is in session memory.
   - The API key never goes in the code or in git. shinyapps.io has no
     environment-variable setting (rsconnect's `envVars` is "currently
     supported by Connect only"), so on the server the key arrives in the
     deployed `config.yml` as `anthropic_api_key`, the way the existing
     deploy already passes the email password; locally it comes from
     `ANTHROPIC_API_KEY`. See "Setting up the API key" below.
   - The host keeps the app's stdout and stderr as its logs; the app writes
     nothing about a paper to either (tested).
   - One R process serves several sessions on shinyapps.io, so the
     minutes-long API call runs in a background process (Deferred, above;
     done 2026-11-10).
4. `[decided]` **Training: "Claude is not allowed to learn from my models.
   Claude is allowed to see coarse metadata."** (2026-10-30, quoted above.)
   Recorded as a requirement on the deployment in
   `docs/paper-review-privacy.md` section 1a: the paper's content and the
   extracted model must not be used for training, and "coarse metadata" is
   defined there as request counts, token usage, timing and error class,
   never content, file names, drug names or parameter values. That
   definition is for Shafer to confirm or narrow.
5. `[decision]` **Downloadable report.** May the user download the review
   report (it then leaves the app, on the user's own machine)? The prototype
   offers an HTML download, switched by `paper_review_download` in
   `config.yml` (on by default for local evaluation); nothing is kept
   server-side either way. (Asked 2026-10-30.)
6. `[decided]` **Test papers: Shafer supplies them.** "No problem. I've got
   lots of papers for testing." (2026-10-30, relayed by the parent session.)
   For each live test the session asks him for a paper in its own session,
   and he uploads it or points at a file. **None is taken from
   `C:\dev\stanpumpRPapers`**: the archive stays off-limits for this work,
   even though it holds many papers. Each test paper is then treated like
   any upload: read into memory, its bytes dropped once the extraction
   returns, nothing written to disk, logged, digested or committed, and the
   extraction and review discarded when the test ends.
   **Updated 2026-11-10:** Shafer, asking for a live test of the deployed
   key, "use papers in my stanpumpRPapers in my Google Drive", so a session
   may now take a test paper from the archive on his request (read-only,
   through the Google Drive connector, downloaded to the session's
   scratchpad and never committed, as `AGENTS.md` says of the archive).
7. `[decided]` **Nothing held on the server** (Shafer, 2026-11-10): "I am
   uncomfortable with leaving a finished review on the server, because I
   believe that 'single paper review' will be used to review submissions to
   peer reviewed journals. I don't want those papers, or the review, held on
   the server. The documentation should affirm the 'nothing held' policy in
   unambiguous language." Asked after the first live review on production
   outlasted the page that requested it, when keeping a finished review on
   the server for the user to collect was proposed as a fix and rejected. A
   review that finishes after its session ended is dropped unread; the
   download is the only copy that outlives a session. Recorded as a
   requirement in `docs/paper-review-privacy.md` section 1a, and said to
   users in `inst/paper-review/help.md` (*Nothing is kept on the server*).
8. `[decision]` **The effort level, and whether users choose it** (asked
   2026-11-10). The app runs Claude Opus 5.5 at effort `high` (Shafer: "I
   care more about getting this right than getting fast answers. Also, I am
   not constrained by cost. However, my defaults are not necessarily the
   right defaults for a program that is publicly available, or for a program
   where I'm footing the initial bill for test users."). He pictures a
   slider from "fast, inexpensive, less accurate" to "slow, expensive, more
   accurate". To be decided on the calibration in
   `tools/paper-review-calibration/` (20 library papers scored against their
   digest cards at all five effort levels): the default level, whether the
   choice is offered and how each level is described, and the app's
   64,000-token and 20-minute limits.

   **The calibration has run** (2026-11-10; `docs/paper-review-calibration.md`:
   18 of the 20 papers, the two scans being too large for the Drive
   connector, 126 reviews, about $135). Its recommendation: keep `high`, the
   lowest level that reads every modern paper's model in full and never
   failed (median 3.5 minutes, longest 7, about $0.78); if a choice is
   offered, offer only `medium` beside it (median 2.2 minutes, about $0.54,
   the same extraction on a paper with one model, less on a paper with
   several), since `low` returned invalid JSON in one review in six and
   `xhigh` and `max` found nothing more while running past the 64,000-token
   limit (a third of `xhigh` runs, every `max` run); raise `max_tokens` to
   128,000 (`high` used up to 56,348); keep the 20-minute limit. The same
   request that stalled on production for Hallik 2020 took 2.4 and 3.1
   minutes here, so the stall was not the model's reading time. Still for
   Shafer: whether to offer the choice at all.
   The calibration also found that most error findings on these verified
   papers are false alarms from the checks themselves (a group that is not a
   model checked as one, conditions the `at` patient cannot carry, decrement
   times without their infusion, an individual compared with a group's
   model), which no choice of effort cures.

   **Decided** (Shafer, 2026-11-10): `max_tokens` raised to 128,000 and
   `high` kept as the default (`PAPER_REVIEW_MAX_TOKENS`,
   `PAPER_REVIEW_EFFORT` in `R/paper-review-api.R`). Still open: whether to
   offer the user a choice of effort.
