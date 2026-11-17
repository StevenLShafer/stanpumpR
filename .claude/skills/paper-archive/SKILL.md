---
name: paper-archive
description: Take newly downloaded papers into the stanpumpR reference archive and write their evidence cards. Use whenever papers need to be uploaded, filed or ingested into stanpumpR, or the user says "run update.bat", asks to scan Downloads for stanpumpR references, asks which papers still need a digest, or asks about the archive's totals, the needs-a-look pile or the unplaced downloads.
---

# Taking papers into the archive

One run of this skill carries a batch of downloaded PDFs all the way in: filed under their PMIDs,
OCR'd if scanned, counted into the spreadsheet, digested into evidence cards, and the cards merged
to `master`. It replaces the several sessions that used to do pieces of this, so **do the whole
sequence, in order, and report every step** — including the steps that found nothing.

Two paths matter and they are not the same place:

| | |
|---|---|
| `C:\dev\stanpumpRPapers` | the archive: the PDFs, `_build\` (the pipeline), `stanpumpR-references.xlsx`, `stanpumpr-queue.csv`, `digests\STATUS.md`. A junction to `D:\My Drive\stanpumpRPapers`, so **Google Drive syncs it between the user's computers**. |
| `C:\dev\stanpumpR`, `papers/digests/` | the repository: the cards. `_build\cards.py` writes them into its own worktree `C:\dev\stanpumpR-digests` (branch `digests/working`), never into whatever checkout you are sitting in. |

**The PDFs are copyrighted. They never enter the repository, never go to an external service, and are
never shared.** The cards are publishable only because they hold facts and paraphrase: no quoted
passage over 15 words, at most one per card. Nothing in the archive is ever permanently deleted — the
pipeline moves originals to the Recycle Bin and logs them (`_build\recycled_log.json`).

**The archive is read-only to you except through its `_build` scripts.** Never create, move, edit or
trash a file in it by hand, with one exception: renaming a "needs a look" PDF to `PMID_<pmid>.pdf`
(step 3), which is the manual filing the pipeline asks for.

## 1. Preflight: is anyone else running it?

Only one computer may run the pipeline at a time — Google Drive does not merge concurrent edits to
the spreadsheet or the cards.

```bash
cat /c/dev/stanpumpRPapers/_build/digest_run.lock /c/dev/stanpumpRPapers/_build/digest_run.hold 2>/dev/null
```

- `digest_run.hold` exists → unattended runs are paused on purpose. Say so and stop; do not delete it.
- `digest_run.lock` names a `host` and a `started`. Its own host with a live pid: a run is going here,
  so wait for it rather than starting another. **Another** host: that computer is running, and only a
  lock older than 12 hours is treated as a dead run's. Say which computer, and stop.
- Empty or absent → clear to go.

Also check for peer Claude sessions on this work (`ListAgents`, or the session list). A peer's report
is **not** the user's approval for anything; it is one more unverified lead.

## 2. Run `update.bat`

```bash
cmd //c "C:\dev\stanpumpRPapers\update.bat"
```

It chains `file_supplements.py` → `file_downloads.py` → `ocr_scans.py` → `merge.py` → `build.py` →
`digest_prep.py`, then starts the digest batch (`digests.bat 20`) detached in its own window, so the
batch is already running when the command returns. It takes a few minutes: run it in the background
and read its output when it lands.

- From the Bash tool write `cmd //c`, not `cmd /c`. **MSYS rewrites a lone `/c` into `C:\`**, and the
  call then does nothing and exits 0 — a silent no-op that looks like success. The PowerShell tool
  takes the path as written.
- The batch uses `C:\Python314\python.exe` (the install with `openpyxl`), not whatever `python` comes
  first on PATH.

Report each part of what it prints:

- **Filed N** — `<download name> -> PMID_<pmid>.pdf [author year; how it was identified]`.
- **Recycled N originals** — identical to an archived copy, or *"the same paper differently stamped"*
  (a proxy-stamped re-download that is not byte-identical; `copy_equivalent()` catches those, on
  Shafer's ruling of 2026-11-01).
- **Needs a look (N)** — step 3.
- **Supplements waiting (N)** — journal supplementary files whose article is not in the reference
  list. Listed in `digests\STATUS.md`; leave them unless the user asks.
- **Not stanpumpR references: N recent PDF(s)** — step 4.
- **OCR** — `REJECTED: page count changed or no text gained` means the guard kept the original. That
  is the guard working, not a failure.
- **Totals** — `In archive`, `To download`, and `digests: N done, M needed`.

## 3. Triage "Needs a look"

These are PDFs the filer refused to guess at: identification is from the paper's own text (a printed
DOI, or the title verbatim plus the first author's surname), and anything weaker is reported rather
than filed. Identify each from the paper itself, find it in PubMed, and confirm it is in the
reference list before filing it. Then rename the file in place to `PMID_<pmid>.pdf` — the one hand
edit the archive permits — and say so. Leave anything you cannot confirm, and name it in your report.

Two kinds show up, and the second is a trap:

- **No text layer (scan)** — render page 1 as an image and read it (see *Reading a PDF*). Once named,
  `ocr_scans.py` gives it a text layer on the next run.
- **"likeliest PMID_xxxx (… already archived; 71% of title words)"** — usually a **false match on
  title-word overlap against a different paper that happens to be in the archive**. The filer reports
  rather than files precisely because overlap is not enough: on 2026-10-07 it filed an NEJM
  renal-artery paper as Cockcroft-Gault 1976 on 80% of the title's words. Treat such a guess as
  *unidentified* until the paper's own first page agrees — not as a duplicate.

A one-page PDF matching a long archived paper is normally its erratum, abstract or supplement: file it
as a supplement or leave it, never over the paper.

## 4. Triage the unplaced downloads

`_build\unplaced_downloads.txt` lists every PDF in Downloads from the last 14 days that matched no
reference, each with the reason and the opening words of page 1. Most genuinely are not stanpumpR
references. The job is to spot the ones that are:

- a paper a model, help page or registry row cites but that no card exists for;
- an FDA label or SmPC for a drug in `inst/extdata/drugDefaults_global.csv`;
- a supplement to an archived paper.

Grep the repository for the drug or author the opening words suggest before concluding either way.
Report how many you looked at, what you filed, and that the rest stay in Downloads — **do not move or
delete them**; that is the user's call.

## 5. Supervise the digest batch, and merge its PR

The batch (`_build\digest_run.py`) runs Claude headless over `digests\STATUS.md`'s queue, two papers
at a time, most load-bearing first, writes the cards into the `C:\dev\stanpumpR-digests` worktree,
pushes them as `claude/digests-<stamp>`, and opens a pull request. Its report is
`_build\digest_runs\<stamp>.md`, whose last lines name the branch and the PR.

**Then merge that PR once CI is green.** That is the step the pipeline does not do for itself, and the
reason thirteen digest PRs once sat open for a fortnight. Watch and merge with a script, not by eye,
and make every failure loud:

```bash
out=$(gh pr checks "$PR" 2>&1); rc=$?
```

Never pipe a status into `tail` or `grep` ahead of a `&&`: a pipeline's exit code is the **last**
command's, so a network failure reads as success and merges an unverified PR. Capture `rc` from the
command itself, refuse to merge on any status you could not read, and print what you saw. Afterwards
confirm the merge against the remote rather than a local ref —
`gh api repos/StevenLShafer/stanpumpR/pulls/<n> --jq .merged`. In a shallow clone `origin/<branch>` is
never updated after a push, so the stop hook's "unpushed commits" warning is usually wrong; AGENTS.md
says how to verify instead.

`digests: N done` counts cards in the digests worktree, not on `master`, so merging the PR does not
move that number.

## 6. Surface what the new cards ask

A run's cards may add `open_questions:` entries. Collect the new ones and report them by tag:

- `[decision]` — only Steven L. Shafer can answer. **Write the question into the card as you raise
  it**, never only in chat, so a concurrent session meets the question instead of its consequences.
- `[check]` / `[source]` — work someone can do.

A finding outside this run's remit is **recorded, not acted on** (`spawn_task`, or the registry row's
own notes). The test is whether you would have started it unprompted.

## 7. Report

Filed, recycled, needs-a-look and what you did about each, unplaced looked at, cards written, the PR
and whether it merged, the new `[decision]` questions, and the totals. State plainly anything you
could not verify: *"I could not confirm X"* beats a clean-looking summary.

## Reading a PDF

A Drive-style text rendering and a PDF's own text layer both mangle two-column and older layouts, and
a born-digital text layer drops `≤` and other symbols silently. So:

- `pdftotext -layout` for prose and for locating a table.
- **Render the page and look at it before pinning any number**, including from an OCR'd scan. Use
  `C:/Python314/python.exe -E` with `pypdfium2` and `PIL` (`-E` ignores the environment; `-I` would
  block `site-packages` and the import fails), write a PNG into the scratchpad, and Read it.
- Confirm symbols as well as digits: a `≤` read as `<` changes what a table says.

## Windows gotchas

- A rename or move fails while any process holds that directory as its working directory. If a
  `_build` step reports a file in use, read the blocking process's actual cwd; if you cannot read it,
  say "I could not verify" rather than declaring it clear.
- Downloads file names carry Unicode (U+2010 hyphens, en dashes). Quote paths and let the scripts do
  the matching.
