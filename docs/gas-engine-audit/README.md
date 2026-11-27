# External audit of the inhaled-gas engine

Two rounds. **Round 1** (2026-11-23, stanpumpR `c90c33e`) asked what newer science should change the model; both reports were validated, fixed and decided. **Round 2** (prompt written 2026-11-26, stanpumpR `8c1ea1b`) is a verification audit before James Philip, GasMan's author, is asked to review the engine: whether what was built since round 1 is right, and whether the code, help pages, validation record and statements about GasMan are ready for him to read. Round 2 is described at the end of this page.

## Round 1

Steven L. Shafer asked on 2026-11-23 for two independent external audits of the gas engine, one by Gemini and one by ChatGPT, covering six areas:

- whether stanpumpR's GasMan restatement matches GasMan's released code
- patient covariates
- the physiology sliders, and temperature's effect on MAC in particular
- endobronchial intubation
- how opioids, hypnotics, ketamine and benzodiazepines change MAC
- what newer data should change a model that began with James Philip's GasMan about 30 years ago

Ted Eger's chapter 8, "The Evolution of Pharmacokinetics" (2017 draft, edited by Dr Shafer), is the reference for the phenomena the model must capture or consciously leave out.

The reports come back as Markdown files and are uploaded to Claude. Claude validates them before anything changes: under the AGENTS.md rule on adversarial review by another LLM, their findings are leads, not findings.

### Files

| File | What it is |
|---|---|
| `PROMPT.md` | The round-1 prompt. Paste it unchanged into each auditor. |
| `PROMPT-2.md` | The round-2 prompt. Paste it unchanged into each auditor. |
| `make-bundle.sh` | Builds the attachment from pinned commits: for round 1 (`ROUND=1`) stanpumpR `c90c33e`, for round 2 (the default) stanpumpR `8c1ea1b`; GasMan `d3a2dd3` for both. |
| `chatgpt-2026-11-23-assessment.md` | The validation of ChatGPT's report of 2026-11-23, and what PR #858 changed. |
| `gemini-2026-11-23-assessment.md` | The validation of Gemini's report of 2026-11-23, and what the PR after it changed, with Dr Shafer's ten decisions of 2026-11-25. |
| `chatgpt-2026-11-26-round2-assessment.md` | The validation of ChatGPT's round-2 report of 2026-11-26, and what changed. |

### Running it

1. **Build the attachment** (or use the copy Claude sent with this pull request). The chapter is a manuscript on Dr Shafer's Drive. It is not in the repository, so pass its PDF to the script to include it:

   ```bash
   ROUND=1 docs/gas-engine-audit/make-bundle.sh ~/gas-audit ~/Downloads/Eger-Chapter-8-Evolution-of-Pharmacokinetics-2017-07-29.pdf
   ```

   This writes `~/gas-audit/stanpumpR-gas-audit-c90c33e.zip`. The script reads stanpumpR from git at the pinned commit, whatever is checked out. It fetches GasMan from GitHub unless `GASMAN_REPO` points to a local clone.

2. **Gemini.** Start a new conversation, using the model with the longest context and with code execution if offered. Connect Google Workspace so that it can open stanpumpRPapers. Attach the zip. If the zip is refused, attach `CORE-NUMBERED.txt`, `INDEX.md` and the chapter PDF from the unzipped folder instead. Paste `PROMPT.md`.

3. **ChatGPT.** Start a new conversation with code execution, which it needs to unzip the attachment and run `grep -n`, and the Google Drive connector. Attach the zip and paste `PROMPT.md`.

4. **Keep the two independent.** Do not show either one the other's report. Agreement between independent readers is the useful signal.

5. **Save each reply** under the name the prompt asks for: `gas-engine-audit-gemini-<date>.md` and `gas-engine-audit-chatgpt-<date>.md`. If an auditor ran out of room part way, ask it to continue in the same conversation and join the parts into one file.

6. **Papers.** Add to stanpumpRPapers the papers each report lists under *Papers to add*. Section 9 of `PROMPT.md` lists those already known to be missing.

### Validating the reports

Upload both reports to a Claude Code session on this repository and ask Claude to validate the gas-engine audits. Claude will then:

1. **Check every finding at the pinned commit.** Confirm that each finding's quoted line exists at `c90c33e` and says what the auditor says it does. Discard those that do not, and say so.
2. **Re-run every numerical claim** with the package, and read every cited paper on its PDF in stanpumpRPapers.
3. **Classify each finding** as confirmed, partly confirmed, or not reproduced. Note where the two auditors agree. Look beside each confirmed finding for a larger defect, as earlier audits found there.
4. **Write up the outcome.** List the confirmed defects for fixing, the evidence-based changes as proposals, and every REOPENS DECISION item as a question for Dr Shafer, recorded where the answer will live before it is asked. Nothing that departs from GasMan, or from a recorded decision, is implemented without his decision.
5. **Put changes in their own pull requests**, each with its tests. Record a paper that is still to come in `papers/pending-work.csv`.

## Round 2

Before Dr Shafer asks James Philip to review the gas engine, he wants it checked again. Round 2 (`PROMPT-2.md`) is a verification audit of stanpumpR at `8c1ea1b`, the master after the six pull requests that carried out his decisions of 2026-11-25 (#907, #931, #938, #939, #940, #941). It asks whether the fixes of round 1 and the features since are right, whether each gas is conserved when they are combined, whether every number and claim on the help pages is borne out by the code, whether everything stanpumpR says about GasMan is true at GasMan's pinned commit (including the two GasMan defects `inst/validation/VALIDATION.md` describes, which are to be sent to its maintainers), and what GasMan's author would object to. It is not a literature review, and it ends with a verdict: ready for Dr Philip, ready after named blocking items, or not ready.

### Running it

1. **Check retention first.** The bundle holds only code and the repository's own text, but an auditor connected to stanpumpRPapers can open the archive's papers, which are copyrighted. Before connecting the Drive or attaching anything from the archive, confirm on the account in use that the provider does not retain uploads or train on them (Dr Shafer, 2026-11-21). The prompt reminds the auditor; that is not the control.
2. **The attachment.** Use the copy Claude sent with the pull request that added `PROMPT-2.md` (`stanpumpR-gas-audit-r2-8c1ea1b.zip`, about 2 MB), or build it:

   ```bash
   docs/gas-engine-audit/make-bundle.sh ~/gas-audit
   ```

   This writes `~/gas-audit/stanpumpR-gas-audit-r2-8c1ea1b.zip`. The Eger chapter is not needed this round.
3. **ChatGPT.** A new conversation with code execution (to unzip and run `grep -n`). Attach the zip and paste `PROMPT-2.md`. The Drive connector is optional this round; the papers it might need are Weber 2021, the Al-Sallami fat-free mass paper and Mapleson 1996.
4. **Gemini.** A new conversation with the longest-context model, code execution if offered. Attach the zip, or, if it is refused, `CORE-NUMBERED.txt`, `SUPPLEMENT-NUMBERED.txt`, `CHANGES-SINCE-ROUND-1.diff` and `INDEX.md` from the unzipped folder (the supplement holds `AGENTS.md`, `R/app_server.R`, `R/viewerRun.R` and the round-1 assessments, which the prompt needs; CodeRabbit on #942). Paste `PROMPT-2.md`.
5. **Keep the two independent**, as in round 1.
6. **Save each reply** as `gas-engine-audit-round2-<auditor>-<date>.md`. If an auditor stops for room, ask it to continue in the same conversation and join the parts.

### Validating the reports

Start a **new** Claude Code session on this repository (not the session that wrote the prompt: a fresh session has its whole context free, and everything it needs is in the repository), upload the reports, and ask it to validate the round-2 gas-engine audits following this page. It will do what round 1's validation did, at `8c1ea1b`: confirm each quoted line, re-run each number, classify each finding, look beside each for a larger defect, write `docs/gas-engine-audit/<auditor>-<date>-round2-assessment.md`, put each fix in its own pull request with its tests, and record every question for Dr Shafer where its answer will live before asking it. It also gives its own answer to the prompt's question "Ready for James Philip?" once the fixes are in.

Already known when the prompt was written, and fixed in the pull request that added it: `gas-differences.md` said its table listed eleven differences after #941 had added a twelfth row.
