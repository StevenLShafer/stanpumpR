# Single Paper Review

A reader or peer reviewer uploads one pharmacokinetic manuscript, Claude reads
it, stanpumpR checks the model it describes, and the paper's drug can then be
simulated. This is the scaled-back first version, on branch
`claude/single-paper-review-app-cbf738`, kept off master at Steven L.
Shafer's instruction while master moves fast. (Claude Code, 2026-10-30.)

Run it locally:

```r
devtools::load_all(".")
Sys.setenv(ANTHROPIC_API_KEY = "...")    # or anthropic_api_key in config.yml
shiny::runApp(run_paper_review_app())
```

**Try the example** works without a key and sends nothing: it loads
`inst/paper-review/example-extraction.json`, a fictional paper with a
deliberate misprint.

Privacy: `docs/paper-review-privacy.md` (draft for legal review). User-facing
help: `inst/paper-review/help.md`, shown in the app's "Privacy and help" tab.

## How it works

| Step | Code | What it does |
|---|---|---|
| Upload | `paperReviewReadUpload()` | Reads the PDF into the session's memory and deletes Shiny's temporary file |
| Extract | `paperReviewExtract()`, `R/paper-review-api.R` | One streamed call to the Anthropic Messages API, `claude-opus-5-5`, adaptive thinking at effort `high`, the PDF inline as base64, a JSON-schema structured output (`paperReviewSchema()`, `R/paper-review-schema.R`). No Files API, no caching, server-side refusal fallback on |
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

### What the in-memory simulator does not do (yet)

It is linear and first-order: no Michaelis-Menten elimination, zero-order or
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
3. `DESCRIPTION`: `httr2` in `Suggests`, used only by this optional entry
   point and guarded with `requireNamespace()` (the rule `AGENTS.md` gives for
   plumber). It is already in the renv library as a dependency of other
   packages, so `renv.lock` is unchanged.

New files: `R/paper-review-{schema,api,model,checks,report,app}.R`,
`inst/paper-review/{example-extraction.json,help.md}`,
`tests/testthat/test-paper-review-*.R`, `tests/testthat/fixtures/`,
`docs/paper-review.md`, `docs/paper-review-privacy.md`. The help page is in
`inst/paper-review/`, not `inst/help/`, because every page under `inst/help/`
must be registered in `helpStaticPages()` (`test-help-content.R`), which would
be another seam.


## Deferred (Shafer, 2026-10-30)

- Linking to the paper archive, and overlaying the uploaded model on library
  drug models and archived papers.
- Merging into master, and a PR (a PR auto-deploys a test environment).
- Bring-your-own-key (the seam is `paperReviewApiKey(sessionKey)`).
- Running the API call off the main R thread: today it blocks that R process
  for the minutes the extraction takes, which a local prototype tolerates and a
  shared deployment does not (Shiny `ExtendedTask` with `mirai` or `future`).

## Open decisions

Questions only Shafer can answer, written here as they are asked
(`AGENTS.md`, session coordination, rule 2). Each is `[decision]` until
answered, then `[decided]` with his words.

Shafer, 2026-10-30, relayed by the parent session: "I have a "professional"
account at shinyapps.io. That provides sufficient bandwidth for the
foreseeable future. I'll look into the privacy issues further. Claude is not
allowed to learn from my models. Claude is allowed to see coarse metadata."

1. `[decision]` **An API key.** A deployed app cannot run on a Claude Code
   subscription allowance; it needs an Anthropic Console account and an API
   key billed per token. Do you have one to use for this? (Asked 2026-10-30.)
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
   - The API key reaches the app as an environment variable set at deploy
     time (`ANTHROPIC_API_KEY`), never in the bundle's code or in git. How it
     is delivered to shinyapps.io is to be settled at the first deploy and
     written here.
   - The host keeps the app's stdout and stderr as its logs; the app writes
     nothing about a paper to either (tested).
   - One R process serves several sessions on shinyapps.io, so the
     minutes-long API call must move off the main thread (Deferred, above)
     before real use there.
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
6. `[decision]` **A real paper to test against.** Which open-access PK paper
   should the first live run use? (Asked 2026-10-30.)
