# Single Paper Review: calibration of effort against accuracy, time and cost

Run 2026-11-10 by Claude Code at Shafer's request, from the tooling in
`tools/paper-review-calibration/` (its `README.md` gives the design and the
steps). This is the evidence for open decision 8 in `docs/paper-review.md`:
the effort level the app runs at, whether users choose it, and the app's
64,000-token and 20-minute limits.

The question: as Claude's effort (`output_config.effort`) rises from `low` to
`max`, how do the accuracy of the extraction, the time a review takes and its
cost change, on papers whose models stanpumpR already holds?

## What was run

- **Papers**: 18 of the 20 in `papers.csv`. Schnider 1998 (PMID 9605675,
  10 MB) and Minto 1997 (PMID 9009935, 11 MB) were left out: the Google Drive
  connector refuses a file over 10 MB, and the archive holds no smaller copy.
  They are the two scanned papers, so **a scanned paper is the one kind this
  calibration did not test**. The run resumes with them (`CALIB_SKIP` empty)
  once their PDFs can be fetched. That leaves 12 modern population analyses
  and 6 papers from 1969-1993.
- **Runs**: every paper at every level once, then `medium` and `high` a second
  time: 126 runs (of the design's 140). The app's own request
  (`paperReviewRequestBody()`, Claude Opus 5.5, adaptive thinking), sent
  through the app's own transport (`paperReviewTransport()`, in a background
  process), with `max_tokens` 128,000 and an hour's time limit so that each
  level shows what it needs.
- **Cost**: $123.00 for the 126 recorded runs, at $4 / $20 per million input /
  output tokens. About $12 more was spent on eight requests lost in flight
  (four when the first launch was moved to a detached process, four `max`
  runs when the cloud container restarted), and $0.27 on the smoke test.
  Total about $135.
- **Scoring** (`score.R`): how many of the numbers on each paper's digest card
  (`papers/digests/<stem>.md`, read from the PDF and verified against the
  library) appear in the extraction's structured fields; the review's
  findings; time, tokens and cost. The counts were then read: the misses, the
  error findings and a sample of the concerns, against the cards and the PDFs
  (below).

The results and the extractions are in the session's scratchpad, not in the
repository: they hold the papers' content.

## Results by effort level

| | low | medium | high | xhigh | max |
|---|---|---|---|---|---|
| Runs | 18 | 36 | 36 | 18 | 18 |
| Reviews returned | **15** (3 unparseable JSON) | 36 | 36 | 18 | **12** (6 cut off at 128,000 tokens) |
| Card numbers found (all) | 0.83 | 0.86 | 0.89 | 0.89 | 0.87 |
| Card numbers found (main text) | 0.87 | 0.93 | 0.96 | 0.96 | 0.98 |
| Known problems in the papers found | 9 of 14 (64%) | 27 of 32 (84%) | 30 of 32 (94%) | 15 of 16 (94%) | 9 of 9 (of the 12 that finished) |
| Time, median | 43 s | 2.2 min | 3.5 min | 7.5 min | 15.8 min |
| Time, 90th percentile / longest | 52 s / 60 s | 3.2 / 4.0 min | 5.6 / 7.0 min | 11.9 / 13.1 min | 18.5 / 18.7 min |
| Output tokens, median / most | 5,506 / 7,583 | 17,007 / 32,723 | 26,605 / 56,348 | 52,909 / 98,183 | 115,019 / 128,000 |
| Cost per review, mean / most | $0.30 / $0.43 | $0.54 / $0.88 | $0.78 / $1.26 | $1.40 / $2.13 | $2.51 / $2.87 |
| Would fail the app's limits (64,000 tokens, 20 min) | 0 | 0 | 0 | **6 of 18** (tokens) | **18 of 18** (tokens) |
| Error / concern findings per review | 2.5 / 8.3 | 3.4 / 16.8 | 4.8 / 19.2 | 8.8 / 26.1 | 4.2 / 20.0 |

Notes on the rows:

- **Card numbers found (main text)** leaves out the 31 card values that come
  from a supplement the run never saw (only the main PDF is sent): all 16 of
  GHB's model parameters (Chen 2020 prints them only in Table S4, a Word file
  the app could not take anyway), 9 of rivaroxaban's and 6 of azilsartan's.
  It is the fairer measure. The `max` value is over the 12 runs that
  finished, so it flatters `max`.
- **Known problems found**: 16 problems in 11 of the papers that their cards
  already record or that were confirmed against the PDF while reading these
  runs (below), each counted as found when a run's review names it at any
  severity. The `low` and `max` denominators are smaller because their failed
  runs are left out.
- **Input** is the same at every level (median 45,676 tokens, 32,531-76,213),
  about $0.18 of each review.
- **Time is output.** Within a level, the time correlates with the output
  tokens at r = 0.99 (about 125 tokens a second at every level), with the
  number of models the paper reports at 0.53 and its parameters at 0.49, and
  hardly with its pages (0.16) or its size in tokens (0.14).

### Run to run

Two runs of the same paper at the same level agree closely on what was
extracted and less on what was said about it:

- **Card numbers found**: identical in 16 of 18 papers at `medium` and 16 of 18
  at `high`; the mean difference was 0.013 at both.
- **Time**: the second run took 0.73-1.40 times the first at `medium` and
  0.76-1.48 at `high`; cost differed by at most $0.19 and $0.26.
- **Findings**: the number of error findings differed by 1.2 per paper on
  average at `medium` and 2.8 at `high` (concerns 3.9 and 5.5). The same
  problem is often found in both runs but given a different severity:
  milrinone's inverted haemodynamic targets were an error in four runs, a
  concern in two and a note in one. **The severity of a finding about Claude's
  reading is Claude's judgment and varies from run to run; its substance
  mostly does not.**

### By kind of paper

| | low | medium | high | xhigh | max |
|---|---|---|---|---|---|
| Modern (12), main-text numbers found | 0.86 | 0.94 | 0.99 | 1.00 | 1.00 |
| Older (6), main-text numbers found | 0.87 | 0.90 | 0.90 | 0.90 | 0.92 |
| One model (6), main-text numbers found | 0.95 | 0.99 | 0.99 | 1.00 | 1.00 |
| Several models (12), main-text numbers found | 0.85 | 0.90 | 0.95 | 0.95 | 0.96 |
| One model, mean output / time | 5,400 / 39 s | 14,900 / 1.9 min | 23,500 / 3.1 min | 44,000 / 6.1 min | 110,000 / 15.2 min |
| Several models, mean output / time | 5,700 / 44 s | 18,500 / 2.4 min | 32,100 / 4.2 min | 67,100 / 9.1 min | 106,800 / 14.6 min |

("Several models" is a paper whose `high` extraction names more than one
group: per-patient or per-dose fits, enantiomers, populations, or a PK model
with several PD models.) The older papers' flat 0.90 is not a misreading that
effort fails to cure: it is the same three matching artifacts at every level
(below). For a modern paper with one model, `medium` already finds
everything on the card; the papers with several models are where `high`
earns its time.

## What the misses were

Every card value not found in a structured field, read against the card and
the PDF:

| Kind | Where | Levels |
|---|---|---|
| **Supplement values** (31): the run sends only the main PDF | GHB, rivaroxaban, azilsartan | all |
| **Variability left out**: IIV, IOV and residual-error variances not extracted | azilsartan (9), acetaminophen (3), sugammadex (1); Eleveld's residual errors once at `medium` | `low` (and one `medium` run) |
| **PD parameters left out by choice**: Hallik 2020's six PD models (EC50s, slopes). At `medium` Claude wrote that the PD parameters "are not entered as PK parameters", a reading of the schema; at `high` and above it entered them | Hallik | `low`, both `medium` runs |
| **Printed scale kept**: a table headed "k, 10² min⁻¹" recorded as printed (68.4 rather than 0.684 /min) with unit `other` | atropine (Hinderling 1985), 8 rate constants | every level |
| **Matching artifacts**: a percentage recorded as 30 where the card has 0.30; a population median or reference weight recorded in the population description rather than a covariate field | vasopressin (2), rivaroxaban (3), nicardipine (1) | every level |

Only the second and third rows are effort-dependent, and neither is a
misreading of a number: at `low` and `medium` Claude leaves things out. The
score counts absences, not wrong values; reading the error findings (below)
found no misread number, but two misattributions at `high`: an atropine
subject's own V_P and CL filed under its dose group, whose model is built from
the group's means, and dexmedetomidine's pre-bypass clearance taken as the
model's clearance where the printed comparison is post-operative. The
printed-scale case is the one that matters to the user
beyond the score: a value in unit `other` cannot be used by the checks, so
atropine's model goes unchecked at every level.

## What the error findings were

A review's error findings on a verified paper are either a real problem in the
paper or a false alarm. Reading the distinct error findings of the `high`
runs (both replicates, all 18 papers) against the cards and PDFs, with the
other levels classed by the same kinds:

**Claude's reading of the paper** (145 of the 544 errors, 27%): misprints and
contradictions Claude reports from reading the text. **Every one checked was
true.** They include the problems the cards already record (verapamil patient
2's Vc printed 92.72 for 9.272 L; nicardipine subject 3's CL 0.833 for 0.883 and
the abstract's unexplained 0.275 L/kg; azilsartan's BMI equation, which gives
a zero volume at the reference BMI; methadone's V3 weight term with no
reference weight) and new ones confirmed on the PDF while reading these runs:
in Hinderling 1985 (atropine) the dose-group means of fT do not match the
individual values (0.93 and 0.88 against the printed 0.87 and 0.90), the
2150 µg mean t½β is 129 min against the printed 121, and subject C's
k^B_PM1 + k^B_PM2 (4.12 + 3.08) does not equal its k^A_PM1 (6.30); Eleveld
2017's final PD equation has Ce50^γ in the numerator, the opposite of its
Methods; Hallik 2020 prints an SE of 44.5 for a CL of 41.2; Chen 2020's Tmax
medians lie below their own ranges; Zuppa 2019 (dexmedetomidine) calls the
allometric 1240 a linear-model value and lists V2pre twice; phenobarbital's
675 samples in the text are 677 in Table 1. Most are editorial; three
(verapamil's Vc, azilsartan's BMI term, Eleveld's equation) would mislead
anyone implementing the model.

**False alarms from the checks** (the rest), in five kinds, all in
stanpumpR's checks rather than in Claude's reading:

1. **A group that is not a model is checked as one** (most of the 135
   "The paper does not close the n-compartment model" errors, a quarter of
   all errors). Claude
   names a group for every analysis a paper reports (a noncompartmental
   analysis, a base model before covariates, a PD model, an interim model),
   and each is put through the identifiability checks.
2. **A quantity printed under conditions the check cannot represent**: the
   `at` patient carries weight, height, age and sex, so exenatide's clearance
   at a stated eGFR and concentration, phenobarbital's CL/F with valproate,
   rocuronium's ke0 under sevoflurane, and dexmedetomidine's post-operative
   clearance (the model has three phases: before, on and after bypass) are
   each compared with the reference model and "disagree".
3. **Decrement times without their infusion**: milrinone's context-sensitive
   half-times for infusions of 45 to 600 minutes compared with the decrement
   after an infusion of unlimited length (+178% to +793%). Claude recorded the
   durations, in the value's text, at `medium`; the check does not read them.
4. **An individual's value against the group's model**: per-patient half-lives
   and volumes in verapamil, hydralazine and atropine compared with the model
   built from the group's mean parameters, which is not that patient's model;
   and verapamil's mean CL compared with the CL computed from the mean Vc and
   mean k10, which a misprinted Vc inflates.
5. **An effect site where the model has none**: Hallik 2020 has an effect
   compartment for heart rate only, and each of its other PD models is
   reported as "an effect site but no ke0". Rivaroxaban's power of age on V1
   is reported as not finite at age 0, the library's rule for a drug model,
   which an adult population model need not meet.

`xhigh` produces the most error findings (8.8 a review) because it extracts
more of each paper's per-patient and per-group detail, which feeds kinds 1
and 4; its extra findings are not extra problems found.

These false alarms are the largest defect the calibration found, and none of
them is cured by effort. They are recorded here for a separate change to the
checks (`R/paper-review-checks.R`): skip identifiability for a group with no
disposition parameters; compare a quantity printed under covariates the `at`
patient cannot carry as a concern, not an error; read the infusion duration
of a decrement time; and compare a per-individual quantity only with that
individual's own parameters.

## Concerns

The concerns (8 to 26 a review) were read by kind rather than one by one: the
twenty commonest titles cover two fifths of the 2,130. Most say what the review
could not check rather than claim a problem: a covariate equation in a form
the review cannot evaluate (the commonest), a figure that could not be
compared, a concentration or volume unit it cannot convert, a dose basis or
reference value the paper does not state, a volume described as an
anatomical space. The last three are the paper's own omissions or wording and
were true where checked (methadone's dose basis, the salt-or-base question in
several papers). None was a misreading.

## Failures

- **`low`: 3 of 18 reviews (17%) were not valid JSON** (Eleveld, exenatide,
  GHB): a stray `}`, a trailing comma. The app sends its schema in the
  instructions, not as an enforced structured output (which the API refused
  for this schema), so it would fail these reviews the same way. No other
  level produced invalid JSON.
- **`max`: 6 of 18 runs (33%) used all 128,000 output tokens and stopped
  incomplete** (sugammadex, dexmedetomidine, methadone, verapamil, hydralazine,
  atropine), and all 18 used more than the app's 64,000. `max` cannot be
  offered within any limit the API allows.
- **`xhigh`: 6 of 18 used more than 64,000 tokens**; at the calibration's
  128,000 all finished, the longest in 13 minutes.
- **`high`: none.** The most output was 56,348 tokens (verapamil, 88% of the
  app's limit), the longest 7.0 minutes.

### The production review that did not finish

The calibration's limits were set because a production review of Hallik 2020
at `high` ran past 20 minutes on 2026-11-10. Here, the same paper at the same
level, sent with the same request through the same background transport, took
2.4 and 3.1 minutes (18,052 and 22,439 output tokens). **The model's reading
time does not explain the production failure.** Its cause is still to be
found in the production log and the usage email: a stalled stream, the
shinyapps.io worker, or the background process.

## Recommendation (for decision 8)

1. **Keep `high` as the default** (decided: Shafer, 2026-11-10). It is the lowest level that reads every
   modern paper's model in full (0.99 of the main-text card numbers, against
   0.94 at `medium`), finds 94% of the known problems (84% at `medium`), and
   never failed: median 3.5 minutes, longest 7, about $0.78 a review.
   `xhigh` found nothing more (0.96 and 94%), took twice as long, cost 1.8
   times as much and needed more than 64,000 tokens on a third of the papers;
   `max` is out of reach.
2. **Offer `medium` as the one alternative, if a choice is offered.** It is
   the step down that holds: 1.6 times faster (median 2.2, longest 4.0
   minutes) and 30% cheaper ($0.54), with the same extraction on a modern
   paper with one model. What it loses is detail on papers with several
   models (Hallik's PD models) and the finer arithmetic checks of a
   per-patient table (atropine). Do not offer `low` (one review in six fails
   outright) or `xhigh` and `max` (no gain; over the limits). A slider of five
   positions would offer three that should not be chosen; a choice of two
   is honest. What it could say:
   - **Standard** (`high`): "Reads every table and model in full. About 2 to
     7 minutes; about $0.80."
   - **Quick** (`medium`): "Reads the main model fully but may leave out
     secondary models and some per-patient detail. About 1.5 to 4 minutes;
     about $0.55."
   For a public deployment on the operator's key the cost difference is
   $0.24 a review: at the planned 200 shared-key reviews, about $48. That is
   too small to justify a lower default; the case for `medium` is time,
   not money.
3. **Raise `max_tokens` from 64,000 to 128,000** (done: Shafer, 2026-11-10). `high` used up to 56,348
   (88% of the limit) on an eight-page paper with twelve per-patient fits; a
   paper with more models would exceed it and fail with nothing to show. The
   limit costs nothing unless it is used, and a review that needs the tokens
   is worth finishing. (At 125 tokens a second, 128,000 tokens is 17
   minutes, inside the 20-minute limit.)
4. **Keep the 20-minute limit.** No `high` run came near it (the longest was 7
   minutes), so it bounds only a stalled request, which is what the production
   failure looks like.
5. **Fix the false alarms** (above): the review's errors on these verified
   papers are now mostly the checks' own. This matters more to a peer reviewer
   than any choice of effort. (Done the same day: *After the fixes*, below.)
6. **Run the two scanned papers** when their PDFs can be fetched, before the
   help says anything specific about scans.

## After the fixes

Shafer asked the same day for the false alarms to be fixed (commit "the
checks' false alarms fixed", PR #710). The checks now:

- report a group with no disposition parameter of its own (a
  noncompartmental analysis, a PD model, a model-building step) as having no
  model to check, not as failing to close one; and a group with no effect site
  of its own as such, when another group holds the ke0;
- treat one group spelled two ways (R(−) and R(-)) as one;
- report as a concern, not an error, a derived quantity printed under
  conditions the `at` patient cannot carry (a new field, `conditions`), one
  that is a mean or median of individuals' values, and one individual's value
  beside another's from a model with no covariate that separates them;
- compare a decrement time at the infusion it was printed after (a new
  field, `infusion_minutes`, or a length written beside the value);
- report a covariate term that fails only outside the population studied as a
  concern;

and the prompt asks for a power of ten printed in a column header to be
applied to the value.

Measured on the twelve papers the false alarms came from, at `high`:

| | Runs | Main-text card numbers found | Error findings per review | Concerns | Median time | Cost |
|---|---|---|---|---|---|---|
| The calibration's runs, old checks | 24 | 0.96 | 6.3 | 21.3 | 4.1 min | $0.83 |
| The same extractions, new checks | 24 | 0.96 | 3.9 | 22.0 | 4.1 min | $0.83 |
| New runs, new checks and prompt | 12 | **1.00** | **2.1** | 26.5 | 4.4 min | $0.87 |

On all 126 calibration extractions the new checks alone take the error
findings from 544 to 381; the new fields need a new extraction, which the
twelve new runs are. Atropine's rate constants are now recorded in /min
(its main-text recall rises from 0.62 to 1.00), Hallik 2020 keeps no error
of its seven groups' "missing ke0", and milrinone's decrement times are each
compared at their own infusion.

What remains of the 25 error findings in the twelve new runs is almost all
true: 16 are Claude's reading notes, and every one checked was true (including three
new ones: exenatide's Table 2 lists Vp twice; Henthorn 2026 prints
"0.026 ± 002"; phenobarbital's text gives the typical CL/F with valproate as
0.18 L/h where its own model gives 0.236 × 0.711 = 0.168). The rest are the
methadone paper, whose missing reference weight for V3 makes any model built
from it disagree with its own figures and half-lives (correctly an error),
its EDDP metabolite models (whose printed quantities do not close a model),
GHB, whose main text prints no model parameter at all (they are in a
supplement the app cannot take, a Word file: also correctly an error), and
two of verapamil's per-patient curves (patient 2's is the misprinted Vc).

## Reproducing it

`tools/paper-review-calibration/README.md`. `score.R` writes `scores.csv`,
`summary.csv`, `misses.csv` and `findings.csv`; it now also reports the card
values from a supplement (`key_supplement`, `recall_main`) and the misses
present at a power of ten (`found_scaled`). Two fixes were needed during the
run: `score.R` matched the minus sign with a literal character class that R
rejects in a C locale, and it counted the findings of a paper with several
models twice (`review$findings` already gathers every model's). The known
problems were matched by regular expressions over the findings and checked by
hand where a pattern was loose.

(Claude Code, 2026-11-10.)
