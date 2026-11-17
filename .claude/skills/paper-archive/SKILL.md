---
name: paper-archive
description: Take newly downloaded papers into the stanpumpR reference archive and write their evidence cards. Use whenever papers need to be uploaded, filed or ingested into stanpumpR, or the user says "run update.bat", asks to scan Downloads for stanpumpR references, asks which papers still need a digest or which a model is still waiting for, or asks about the archive's totals, the needs-a-look pile or the unplaced downloads.
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

**The PDFs are copyrighted. They never enter the repository and are never published or passed to a
third party** (AGENTS.md, *Read-only, and the papers stay out of the repository*): a PDF or its
extracted text goes in the scratchpad and is never committed. Reading one *is* the work — you read
them, and so does the digest batch — so the rule is about distribution, not about whether a model
sees the text. The cards are publishable only because they hold facts and paraphrase: no quoted
passage over 15 words, at most one per card. Nothing in the archive is ever permanently deleted — the
pipeline moves originals to the Recycle Bin and logs them (`_build\recycled_log.json`).

**The archive is read-only to you except through its `_build` scripts.** Never create, move, edit or
trash a file in it by hand, with one exception: renaming a "needs a look" PDF to `PMID_<pmid>.pdf`
(step 4), which is the manual filing the pipeline asks for.

## 1. Preflight: is anyone else running it?

Only one computer may run the pipeline at a time — Google Drive does not merge concurrent edits to
the spreadsheet or the cards.

```bash
B=/c/dev/stanpumpRPapers/_build
for f in digest_run.hold digest_run.lock; do
  if [ -e "$B/$f" ]; then echo "$f EXISTS:"; cat "$B/$f"; echo; else echo "$f absent"; fi
done
```

Test **existence**, not contents: `digest_run.py` stops on `os.path.exists(HOLD)`, so an empty hold
file holds the run just as a worded one does, and a bare `cat` would print nothing and read as clear.

- **`digest_run.hold` must be ABSENT to proceed.** Any hold file stops the run — empty, worded, or
  whitespace — because `digest_run.py` tests only `os.path.exists`. It is paused on purpose: say so,
  stop, and do not delete it. An empty one is not permission; it is a pause whose note was left out.
- `digest_run.lock` names a `host` and a `started`. Its own host with a live pid: a run is going here,
  so wait for it rather than starting another. **Another** host, less than 12 hours old: that computer
  is running — say which, and stop. Another host's and older than that: `digest_run.py` takes it for
  a dead run's and clears it, so it is no bar, and neither are you. An empty or unparseable lock is
  the one file where emptiness means nothing — `digest_run.py` reads it as `{}`, deletes it and goes
  on — so treat it as no lock at all, whichever host wrote it.
- Clear to go only when the hold file is absent **and** the lock is one of the four `digest_run.py`
  itself clears: absent; unreadable (empty or not JSON, read as `{}`); this host's with no live pid;
  or another host's and more than 12 hours old. Anything else — this host's with a live pid, another
  host's and recent — means stop.

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
- **Needs a look (N)** — step 4.
- **Supplements waiting (N)** — journal supplementary files whose article is not in the reference
  list. Listed in `digests\STATUS.md`; leave them unless the user asks.
- **Not stanpumpR references: N recent PDF(s)** — step 5.
- **OCR** — `REJECTED: page count changed or no text gained` means the guard kept the original. That
  is the guard working, not a failure.
- **Papers a model is waiting for** — the arrivals report, step 3.
- **Totals** — `In archive`, `To download`, and `digests: N done, M needed`.

## 3. Close the loop: the arrivals report

`_build\arrivals.py` is the last thing `update.bat` runs, and it is the only step that connects a
filed PDF back to the model that asked for it. **Never skip it.** The archive's pull side has always
worked — a registry row names the paper it wants by PMID, usually with the question it should answer,
and the card lands in `papers/digests/` beside the code. What never existed was the push: nothing
noticed that a paper somebody asked for weeks ago was already sitting in the archive. When this was
first run (2026-11-16) **41 of the 60 `to_obtain` PMIDs were already filed and 14 of those had no
card**, and nobody had been told.

Run it alone at any time: `C:/Python314/python.exe -E C:\dev\stanpumpRPapers\_build\arrivals.py`
(`--no-fetch` offline, `--all` to list what is still to obtain). It reads the registries from
`origin/master`, not from a working tree, because several checkouts live on this machine and any of
them may be on a branch. It reads only the `to_obtain` column: a PMID under `source` is the paper a
row was *built* from, not a request.

Four kinds come back, and three of them are work:

- **Arrived, not yet digested** — it is in the digest queue; the batch will reach it. Nothing to do
  beyond noticing, except to say so, because the person who asked may be waiting.
- **Digested, but the row still says it is waiting** — *this is the one that rots.* The card exists,
  so the question was answered, and the registry still advertises the paper as missing. Read the card
  and update the row's `evidence_read` / `readiness` / `to_obtain`. Report these individually.
- **A card written from a non-published edition whose published PDF is now here** — an accepted
  manuscript superseded. Re-digest, or at least read what the AAM's card records that it could not.
- **Still to obtain** — counted only; `stanpumpr-queue.csv` already tracks those.

Acting on these is a model change and so belongs to the drug's own procedure (`docs/adding-a-drug.md`)
— but a row that stays stale is how a paper the user went to the trouble of ordering never reaches the
model that needed it, which is precisely what this report exists to prevent.

## 4. Triage "Needs a look"

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

## 5. Triage the unplaced downloads

`_build\unplaced_downloads.txt` lists every PDF in Downloads from the last 14 days that matched no
reference, each with the reason and the opening words of page 1. Most genuinely are not stanpumpR
references. The job is to spot the ones that are:

- a paper a model, help page or registry row cites but that no card exists for;
- an FDA label or SmPC for a drug in `inst/extdata/drugDefaults_global.csv`;
- a supplement to an archived paper.

Grep the repository for the drug or author the opening words suggest before concluding either way.
Report how many you looked at, what you filed, and that the rest stay in Downloads — **do not move or
delete them**; that is the user's call.

## 6. Supervise the digest batch, and merge its PR

The batch (`_build\digest_run.py`) runs Claude headless over `digests\STATUS.md`'s queue, two papers
at a time, most load-bearing first, writes the cards into the `C:\dev\stanpumpR-digests` worktree,
pushes them as `claude/digests-<stamp>`, and opens a pull request. Its report is
`_build\digest_runs\<stamp>.md`, whose last lines name the branch and the PR.

**Then merge that PR once CI is green.** That is the step the pipeline does not do for itself, and the
reason thirteen digest PRs once sat open for a fortnight. Do step 7's read of the run's new
`open_questions:` before merging — and if you EDIT a card, remember the branch was pushed before you
read it: `commit_and_push()` deliberately leaves alone anything the run did not write, so your edit
sits uncommitted on the worktree's own `digests/working` and is in no PR at all. Commit it onto the
run's pushed branch and push, then confirm it in `gh pr diff` before merging. Watch and merge with a
script, not by eye, and make every failure loud:

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

## 7. Surface what the new cards ask

A run's cards may add `open_questions:` entries. **Read them on the branch, before step 6 merges it**,
because a question you add to a card after the merge is not in the merged PR and so reaches nobody.
Collect the new ones and report them by tag:

- `[decision]` — only Steven L. Shafer can answer. **Write the question into the card as you raise
  it**, never only in chat, so a concurrent session meets the question instead of its consequences.
  An edit you make here is uncommitted in the digests worktree and so is in no PR: commit and push it
  onto the run's branch and check `gh pr diff` before merging, or, if the PR is already merged, open
  its own follow-up PR. An edit left in the worktree is invisible to the other computers, which is the
  whole reason the cards live in the repository.
- `[check]` / `[source]` — work someone can do.

A finding outside this run's remit is **recorded, not acted on** (`spawn_task`, or the registry row's
own notes). The test is whether you would have started it unprompted.

## 8. Report

Filed, recycled, needs-a-look and what you did about each, unplaced looked at, **the arrivals report
drug by drug**, cards written, the PR and whether it merged, the new `[decision]` questions, and the
totals. State plainly anything you could not verify: *"I could not confirm X"* beats a clean-looking
summary.

Lead with the arrivals, not the counts. The user orders these papers because a model needs them, so
*"Kanto 1981 arrived — `vasoactiveRegistry.csv` wants it to confirm labetalol's dose basis and assay"*
is the sentence they are waiting for; *"filed 15, 1,295 in archive"* is bookkeeping.

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
