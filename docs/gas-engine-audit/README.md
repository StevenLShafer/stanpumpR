# External audit of the inhaled-gas engine

Steven L. Shafer asked on 2026-11-23 for two independent external audits of the gas engine, one by Gemini and one by ChatGPT, covering six areas:

- whether stanpumpR's GasMan restatement matches GasMan's released code
- patient covariates
- the physiology sliders, and temperature's effect on MAC in particular
- endobronchial intubation
- how opioids, hypnotics, ketamine and benzodiazepines change MAC
- what newer data should change a model that began with James Philip's GasMan about 30 years ago

Ted Eger's chapter 8, "The Evolution of Pharmacokinetics" (2017 draft, edited by Dr Shafer), is the reference for the phenomena the model must capture or consciously leave out.

The reports come back as Markdown files and are uploaded to Claude. Claude validates them before anything changes: under the AGENTS.md rule on adversarial review by another LLM, their findings are leads, not findings.

## Files

| File | What it is |
|---|---|
| `PROMPT.md` | The prompt. Paste it unchanged into each auditor. |
| `make-bundle.sh` | Builds the attachment from pinned commits: stanpumpR `c90c33e` and GasMan `d3a2dd3`. |
| `chatgpt-2026-11-23-assessment.md` | The validation of ChatGPT's report of 2026-11-23, and what PR #858 changed. |
| `gemini-2026-11-23-assessment.md` | The validation of Gemini's report of 2026-11-23, and what the PR after it changed. |

## Running it

1. **Build the attachment** (or use the copy Claude sent with this pull request). The chapter is a manuscript on Dr Shafer's Drive. It is not in the repository, so pass its PDF to the script to include it:

   ```bash
   docs/gas-engine-audit/make-bundle.sh ~/gas-audit ~/Downloads/Eger-Chapter-8-Evolution-of-Pharmacokinetics-2017-07-29.pdf
   ```

   This writes `~/gas-audit/stanpumpR-gas-audit-c90c33e.zip`. The script reads stanpumpR from git at the pinned commit, whatever is checked out. It fetches GasMan from GitHub unless `GASMAN_REPO` points to a local clone.

2. **Gemini.** Start a new conversation, using the model with the longest context and with code execution if offered. Connect Google Workspace so that it can open stanpumpRPapers. Attach the zip. If the zip is refused, attach `CORE-NUMBERED.txt`, `INDEX.md` and the chapter PDF from the unzipped folder instead. Paste `PROMPT.md`.

3. **ChatGPT.** Start a new conversation with code execution, which it needs to unzip the attachment and run `grep -n`, and the Google Drive connector. Attach the zip and paste `PROMPT.md`.

4. **Keep the two independent.** Do not show either one the other's report. Agreement between independent readers is the useful signal.

5. **Save each reply** under the name the prompt asks for: `gas-engine-audit-gemini-<date>.md` and `gas-engine-audit-chatgpt-<date>.md`. If an auditor ran out of room part way, ask it to continue in the same conversation and join the parts into one file.

6. **Papers.** Add to stanpumpRPapers the papers each report lists under *Papers to add*. Section 9 of `PROMPT.md` lists those already known to be missing.

## Validating the reports

Upload both reports to a Claude Code session on this repository and ask Claude to validate the gas-engine audits. Claude will then:

1. **Check every finding at the pinned commit.** Confirm that each finding's quoted line exists at `c90c33e` and says what the auditor says it does. Discard those that do not, and say so.
2. **Re-run every numerical claim** with the package, and read every cited paper on its PDF in stanpumpRPapers.
3. **Classify each finding** as confirmed, partly confirmed, or not reproduced. Note where the two auditors agree. Look beside each confirmed finding for a larger defect, as earlier audits found there.
4. **Write up the outcome.** List the confirmed defects for fixing, the evidence-based changes as proposals, and every REOPENS DECISION item as a question for Dr Shafer, recorded where the answer will live before it is asked. Nothing that departs from GasMan, or from a recorded decision, is implemented without his decision.
5. **Put changes in their own pull requests**, each with its tests. Record a paper that is still to come in `papers/pending-work.csv`.
