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

The first two live attempts were refused, which is why the schema is sent
in the instructions rather than as a structured output
(`output_config.format`): the API allows at most 16 nullable or union-typed
fields (this schema had 42), and once those were removed it still refused
the schema because "the compiled grammar is too large". The reply is
checked on return instead (`paperReviewValidate()`).

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

## Setting up the API key

For Shafer to do; no session handles the key itself. During development the
app deliberately shares IntegrityAnalysis's `ANTHROPIC_API_KEY` (Open
decision 1), which needs none of these steps; they are for when a separate
key is wanted, before wider use.
`paperReviewApiKey()` looks in this order: a key held in the session (the
bring-your-own-key seam, unused today), then the environment variables
`STANPUMPR_ANTHROPIC_API_KEY` and `ANTHROPIC_API_KEY`, in that order, then
`anthropic_api_key` in `config.yml`.

**1. Create a key for this app only.**
1. Sign in to the Anthropic Console with the account IntegrityAnalysis bills
   to.
2. Optional but worth it: create a workspace for this app (for example
   "stanpumpR Paper Review"), so its usage is reported separately and can
   carry its own spend limit.
3. Under API keys, create a key in that workspace named, for example,
   `stanpumpR-paper-review`. The Console shows it once: copy it straight into
   step 2 or 3, and nowhere else (not into an email, a chat, or a file in a
   repository).
4. In the same account's settings, check that the organisation has not opted
   in to sharing data for training and is not in the Development Partner
   Program: Shafer's rule, "Claude is not allowed to learn from my models",
   depends on it (`docs/paper-review-privacy.md` section 1a).

**2. Use it locally.** Put it in your user-level R environment file, not in
the project (this repository does not git-ignore a project `.Renviron`),
under its own name so that it sits beside IntegrityAnalysis's
`ANTHROPIC_API_KEY` without replacing it. The app takes
`STANPUMPR_ANTHROPIC_API_KEY` first and falls back to `ANTHROPIC_API_KEY`,
which is what it uses today.
1. In R, `file.edit("~/.Renviron")`.
2. Add the line `STANPUMPR_ANTHROPIC_API_KEY=<the key>` and save. Leave the
   existing `ANTHROPIC_API_KEY` line alone.
3. Restart R. Check it is set without printing it:
   `nzchar(Sys.getenv("STANPUMPR_ANTHROPIC_API_KEY"))` should be `TRUE`.
4. Run the app as in "Run it locally" above. (Alternatively put
   `anthropic_api_key: "<the key>"` in the worktree's `config.yml`, which is
   git-ignored.)

**3. Use it on shinyapps.io.** shinyapps.io has no environment variables, so
the key goes into the deployed `config.yml`:
- *From GitHub Actions* (the way stanpumpR itself deploys): add a repository
  secret, for example `PAPER_REVIEW_ANTHROPIC_API_KEY`, under Settings →
  Secrets and variables → Actions. The paper-review deploy workflow (not yet
  written) writes it into the `config.yml` it deploys as `anthropic_api_key`,
  exactly as `shiny-deploy-production.yaml` writes `email_password`.
- *By hand from your machine*: deploy with `config.yml` (git-ignored, holding
  `anthropic_api_key`) in `appFiles`.

Either way the key then sits in the app's bundle on Posit's servers, as the
email password already does. Revoking it in the Console stops it everywhere;
make a new key and redeploy to rotate it.

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
6. `[decided]` **Test papers: Shafer supplies them.** "No problem. I've got
   lots of papers for testing." (2026-10-30, relayed by the parent session.)
   For each live test the session asks him for a paper in its own session,
   and he uploads it or points at a file. **None is taken from
   `C:\dev\stanpumpRPapers`**: the archive stays off-limits for this work,
   even though it holds many papers. Each test paper is then treated like
   any upload: read into memory, its bytes dropped once the extraction
   returns, nothing written to disk, logged, digested or committed, and the
   extraction and review discarded when the test ends.
