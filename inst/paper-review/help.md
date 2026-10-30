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

## What happens to your manuscript

- The file you upload is read into the memory of your browser session on the
  server, and the temporary file the web server made is deleted at once. It
  is never written anywhere else, and its name is not kept.
- It is sent to Anthropic only when you tick the confirmation box and press
  **Review the paper**, once, over an encrypted connection, as part of a
  single request. The app does not use Anthropic's file storage.
- As soon as Claude's answer returns, the manuscript itself is removed from
  the app. What remains is the extracted model and the review.
- Those are removed when you press **Discard the paper now**, or when you
  close the page or your session ends, whichever is first.
- The app keeps no log of manuscripts or of what was extracted, and adds
  nothing to stanpumpR's paper archive or drug library.
- If you download the report, that copy is yours, on your computer.

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
- **The hosting provider** runs the machine the app runs on.

### Before you upload a manuscript under review

Many journals forbid reviewers to upload a confidential manuscript to an AI
service. Check your journal's policy, and the authors' consent where it
applies. You are responsible for having the right to share the manuscript.

The full statement, with Anthropic's terms cited, is
`docs/paper-review-privacy.md` in the stanpumpR source.
