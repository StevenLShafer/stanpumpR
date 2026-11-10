# Single Paper Review

> **Draft.** This page, and the privacy statement in it, are a draft awaiting
> review. Do not upload a manuscript you are not entitled to share until it
> is final.

## What it does

1. You upload one pharmacokinetic manuscript (a PDF), and optionally its
   supplement.
2. When you press **Review the paper**, the manuscript is sent to Anthropic's
   Claude, which reads it and returns the model it describes: the structure,
   every parameter with where it was printed, the covariate equations, the
   study population, the dosing regimens, the derived quantities the paper
   reports, and approximately digitised points from its concentration
   figures.
3. stanpumpR then checks that model and writes a review report:
   - **Identifiability**: do the printed numbers determine the model, and
     are they stable to their own standard errors?
   - **Internal consistency**: recomputed from the printed parameters, do the
     paper's half-lives, coefficients, Vss, AUC and time to peak effect agree
     with what it prints?
   - **Covariates**: does each covariate equation stay finite and positive
     for every patient stanpumpR accepts?
   - **Assumptions**: dose basis, how bioavailability was obtained, and
     differences of means tested against their own scatter.
   - **Figures**: does the model reproduce the paper's own figures?
4. The **Simulator** tab runs the paper's model, and only that drug, for any
   patient and dose you enter.

Every finding says where in the paper it comes from, and whether stanpumpR
computed it or it is Claude's reading. **Claude can misread a table or a
figure.** Check each finding in the paper before relying on it. The figure
points in particular are read approximately.

**Try the example** shows the report and the simulator for a fictional paper,
without sending anything anywhere.

## Nothing is kept on the server

**stanpumpR's server never stores your manuscript, its supplement, the model
Claude extracts from it, or the review.** They are not written to disk, to a
database, to a log or to a backup, and they are not kept after your session
ends. While you work they exist only in the server's memory, for your browser
session alone, and no one else can open them. When you close the page, or
your session ends for any other reason, they are gone, and no one, you
included, can get them back.

This is deliberate. Single Paper Review is meant for papers under peer review,
which are confidential, so the server is built to hold nothing of them.

What follows from it:

- **Keep this page open until the review appears.** If you close the page,
  or the connection is lost, while Claude is reading, the review is lost. It
  is not kept for you to collect later. A review made with stanpumpR's own
  key is paid for whether or not you see it.
- **To keep a review, download it** (**Download the report**). That copy is
  on your own computer, and it is yours to protect.

The two things recorded about a review are described below: a count kept for
the operator when stanpumpR's own key is used (never the content), and
Anthropic's own handling of the request. Neither is a copy of your paper or
your review on stanpumpR's server.

## How long a review takes

Claude reads the whole paper, finds every model in it, records each parameter
with where it was printed, reads the derived quantities, the regimens and the
points of the concentration figures, and only then hands the result to
stanpumpR, whose checks take seconds. Almost all of the time is Claude's
reading, and it depends on the paper far more than on its length:

| The paper | Expect |
|---|---|
| One model, with its tables | about 2 to 4 minutes |
| Several models (more than one population, drug, enantiomer or endpoint), or a table of each patient's own fit | about 2 to 7 minutes |

**The app waits at most 20 minutes.** A review that takes longer ends with a
message saying it did not finish, and no review.

These times come from a calibration on 18 published papers whose models
stanpumpR already holds, from 7 to 19 pages and from 1969 to 2026, each
reviewed twice (November 2026): the median was 3.5 minutes and the longest 7.
The time depends on how much there is to record, the number of models and
parameters, and hardly at all on the number of pages. Scanned papers were not
yet part of it, so a scan may take longer.

What makes a paper harder to read:

- **More than one model.** A paper that fits the same drug to several
  endpoints or populations has a model for each, and each is read in full.
- **Tables and figures.** Every parameter table is read cell by cell, and
  every concentration figure is digitised point by point.
- **The PDF itself.** A scanned page is an image that must be read like a
  figure; equations set as images, multi-column layouts and tables split
  across pages all take longer and are more often misread.
- **A supplement**, which adds its own tables and figures.

To shorten a review, upload the supplement only when the model's parameters
are in it, and prefer the publisher's PDF to a scan.

## What happens to your manuscript

- The file you upload is read into the memory of your browser session on the
  server, and the temporary file the web server made is deleted at once. It
  is never written anywhere else, and its name is not kept.
- Nothing can be uploaded until you tick the confirmation box. The paper is
  sent to Anthropic only when you then press **Review the paper**, once, over an encrypted connection, as part of a
  single request. The app does not use Anthropic's file storage.
- As soon as Claude's answer returns, the manuscript itself is removed from
  the app. What remains is the extracted model and the review.
- Those are removed when you press **Discard the paper now**, or when you
  close the page or your session ends, whichever is first. Nothing is kept
  after that (see **Nothing is kept on the server** above).
- The app keeps no log of manuscripts or of what was extracted, and adds
  nothing to stanpumpR's paper archive or drug library. When a review uses
  stanpumpR's own Anthropic key, the operator is told that a review was made,
  when, its token use, how long it took and whether it succeeded; never its
  content.
- You may use your own Anthropic API key instead (**Use your own Anthropic
  API key**). It is held only in your session, sent only to Anthropic with
  your review, never saved or logged, and the review is then billed to your
  Anthropic account; nothing about it is reported to the operator.
- Your manuscript and the model extracted from it may not be used to train
  any AI model. That is the operator's requirement. On Anthropic's side it
  rests on Anthropic's commercial terms, which bar training on API content
  unless the customer opts in; the operator does not.
- If you download the report, that copy is yours, on your computer. It is
  the only copy of the review that exists anywhere after your session ends.

### What the app cannot promise

- **Anthropic keeps a copy for a time.** Anthropic acts as a data processor.
  Under its published policy it deletes API inputs and outputs within 30 days,
  keeps them longer if its automated safety systems flag them, and does not
  train its models on them. Whether the operator has a zero-data-retention
  agreement with Anthropic is stated here when it is decided. Deleting the
  paper from this app does not delete Anthropic's copy.
- **The server's memory is released, not erased.** After a discard the
  manuscript can no longer be reached by the app, but the memory it occupied
  is reused by the computer in time rather than overwritten at once.
- **The hosting provider**, shinyapps.io (Posit), runs the machine the app
  runs on, under its own terms.

### Before you upload a manuscript under review

Many journals forbid reviewers to upload a confidential manuscript to an AI
service. Check your journal's policy, and the authors' consent where it
applies. You are responsible for having the right to share the manuscript.

The full statement, with Anthropic's terms cited, is
`docs/paper-review-privacy.md` in the stanpumpR source.
