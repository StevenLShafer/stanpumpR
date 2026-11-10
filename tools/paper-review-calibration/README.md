# Single Paper Review calibration

Shafer (2026-11-10): "Let's test the single paper review ... selecting 20
papers whose PK are already in stanpumpR. This is really an important
contribution that stanpumpR will make to the peer review process, so I want to
get it right. Also, this calibration may help us present options to the user
... a slider that ranges from 'fast, inexpensive, less accurate' on the left to
'slow, expensive, more accurate' on the right. I picked the 'high' effort opus
5.5 ... because I care more about getting this right than getting fast answers.
Also, I am not constrained by cost. However, my defaults are not necessarily the
right defaults for a program that is publicly available, or for a program where
I'm footing the initial bill for test users."

The question: how accuracy, time and cost change with Claude's effort level
(`output_config.effort`: low, medium, high, xhigh, max) on the app's own
extraction, so that the default can be chosen on evidence and the levels a user
may choose can be described honestly. Open decision 8 in `docs/paper-review.md`.

## The design

- **Papers**: the 20 in `papers.csv`, each the source of a model in the library,
  with a digest card in `papers/digests/` whose `parameters` block was read from
  the PDF and checked against stanpumpR (`verification: matches`). 14 modern
  population analyses and 6 older papers (1969-1993), from a one-compartment
  bioassay to a seven-model neonatal PK/PD paper, two of them 10-11 MB scans.
- **Runs**: every paper at every effort level once (100), then `medium` and `high`
  again (40), to measure run-to-run variation. About $140-200 and 5-6 hours at
  4 reviews at a time (estimates: no review has yet been timed at every level).
- **The app's own request** (`paperReviewRequestBody()`), except that
  `max_tokens` is 128,000 and the time limit an hour, so that each level is
  measured by what it needs rather than cut off by the app's 64,000 tokens and
  20 minutes; `score.R` reports which runs the app's limits would have failed.
- **Scoring** (`score.R`): how many of the card's numbers the extraction
  reproduces in its structured fields, the review's findings by severity, time,
  tokens and cost. The counts are a screen, not the verdict: every miss and
  every error or concern finding is read against the PDF and classed as
  Claude's misreading, a matching artifact (a number found under another
  role, a card value the paper prints differently), or a real problem in the
  paper.

## Running it (a new cloud session)

The key is the environment variable `STANPUMPR_ANTHROPIC_API_KEY`, set by
Shafer in the Default environment's settings; Anthropic hosts cannot take a
network secret. Never print it: check it with
`Rscript -e 'nzchar(Sys.getenv("STANPUMPR_ANTHROPIC_API_KEY"))'`.

1. `Rscript -e 'renv::restore(prompt = FALSE)'` (mirai is in the lock).
2. Download the 20 PDFs from the archive with the Google Drive connector
   (`download_file_content` on each `drive_id` in `papers.csv`; the result is
   saved to a tool-results file:
   `jq -r '.content' <file> | base64 -d > <scratchpad>/calib/pdfs/<stem>.pdf`),
   and check each size against Drive's `fileSize`. The PDFs and the results stay
   in the scratchpad and are never committed (`AGENTS.md`, the archive).
3. Smoke test, one paper at `low` (cents):
   `CALIB_PLAN=smoke CALIB_PDFS=<pdfs> CALIB_RESULTS=<results> Rscript tools/paper-review-calibration/run.R`
4. The full run, in the background (it resumes where it stopped):
   `CALIB_PDFS=<pdfs> CALIB_RESULTS=<results> Rscript tools/paper-review-calibration/run.R > <results>/run.log 2>&1`
5. `CALIB_RESULTS=<results> Rscript tools/paper-review-calibration/score.R`, then
   read `misses.csv` and the error and concern findings against the PDFs.
6. Write `docs/paper-review-calibration.md`: per level, the accuracy (and its
   run-to-run spread from the replicates), time, output tokens, cost and the
   runs the app's limits would fail; the same by kind of paper (modern or
   classic, one model or several); the misreadings by kind; and a
   recommendation for the default effort, the app's token and time limits, and
   what a choice offered to the user should say about each level. Then update
   decision 8, the time estimates in `inst/paper-review/help.md` and the busy
   banner (`paperReviewHeader()`).
7. Tell Shafer to delete the variable and revoke the key.

(Claude Code, 2026-11-10.)
