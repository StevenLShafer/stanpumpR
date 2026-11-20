# The paper archive's pipeline

These are the scripts that file, extract, index and digest the source papers. They are the
**version-controlled record**; the copies that **actually run** live beside the papers, in the
archive's own `_build` folder (`papers/README.md` says where the archive is and how a session
reads it).

## Why there are two copies

Most of these scripts find the archive as the parent of their own directory, and the archive
cannot live in this repository: the PDFs are copyrighted, and keeping them out is the reason the
digests can be published at all. So the scripts have to sit beside the papers to run.

Until 2026-11-18 that meant they existed **only** on the Drive, with no history, no review and no
backup — three of them were changed that day, and nothing recorded what they had been. Hence this
folder.

## Keeping the two in step

`check_in_step.py` compares them and names anything that differs, is in the archive but not
committed, or is committed but missing from the archive. `update.bat` runs it last on every
filing run, so drift is reported rather than discovered later; it only ever reports, and always
succeeds, so it cannot break the chain.

A mirror nobody checks is worse than no mirror — it looks like history while quietly describing
something else. That is what the check exists to prevent.

### What it compares the archive with: the committed record, not a working tree

It fetches `master` and reads each file out of `origin/master` with `git cat-file`, resolving
the ref to a commit **once** so a fetch by another session cannot move the subject of the
comparison halfway through. That costs one fetch of one branch per filing run, and it updates
nothing but `refs/remotes/origin/master` in the checkout it reads.

Until 2026-11-19 it read the checkout's **working tree** instead, whatever branch that checkout
was on — and on that day it reported `build.py`, `fetch_pmc.py`, `file_downloads.py` and
`update.bat` as drifted when all four matched `origin/master` byte for byte: `C:\dev\stanpumpR`
was simply on another session's branch, which predated the commit that changed them (PR #797).
Several checkouts and worktrees live on that machine and any of them may be on a branch
(AGENTS.md, *Several sessions run at once*), so the old check was bound to cry wolf — the very
thing the paragraph above says it exists to prevent.

Worse than the false alarm was its advice: *copy the archive's files into that checkout and
commit them*, which would have committed onto another session's branch, the collision AGENTS.md
rule 1 exists to prevent. The advice now names where such a commit belongs, and says that a
change already committed on a branch keeps showing until it is merged.

Every message names the ref or path it compared against — a check whose subject you cannot see
is one you cannot trust. Two cases weaken the answer and so get a line of their own: a **fetch
that failed** (the ref may be out of date, so a difference may be a change that has already
landed) and the **working-tree fallback**, used only when git cannot supply the record at all
(no git, not a repository, no `origin/master`, no `papers/_build` in it). A file the record
holds but that cannot be read is reported as unread, never as differing.

```
python check_in_step.py [--repo <path>] [--archive <path>]
```

**After changing a script in the archive, copy it here and commit it**, saying in the message what
changed and why. The next `update.bat` will tell you if you forgot.

## The filing cache and the queue, an invariant worth keeping

`file_downloads.py` caches a verdict per file and replays it, so the cache signature decides when
a verdict is looked at again. It hashes the download queue's **bytes** as well as the cited works,
because queueing a paper changes what a verdict should be.

That only works while a rebuild of the queue is **byte-stable**. `build.py` rewrites
`stanpumpr-queue.csv` on every run, and if those bytes moved each time, the signature would change
each time and every file in Downloads would be identified again on every run — turning a cache into
a cost.

Measured on 2026-11-18 (CodeRabbit asked, PR #784): the queue's hash was identical before a build,
after it, and after a second build. The mechanism is that `NOW` is the only time-varying value in
the writer, and the single line that uses it (`build.py`, `o["downloaded_at"] = NOW`) fires only
when a row *becomes* `done` with an empty `downloaded_at` — the first build after a paper is filed,
which is exactly when verdicts should be re-examined.

**So: do not add a timestamp, a run id, a duration or any other per-run value to a queue column.**
It would silently disable the filing cache. If you need one, keep it outside the queue, or exclude
that column from the hash deliberately and say so here.

## Clio deliveries arrive wrapped

Clio, Stanford's interlibrary loan, delivers a scan through a form POST, and a browser that saves
the response body verbatim writes the whole `multipart/form-data` envelope: a boundary line, the
part's headers, then the PDF, then the closing boundary. The file is therefore not a PDF by its
magic bytes, though poppler, pypdf and pdfium all skip the preamble, so it reads, renders and
extracts normally.

Found 2026-11-18 on nineteen files, twelve of them already filed — whose text extractions and
digest cards were all sound, which is why nothing had reported it. `file_downloads.py` now strips
the envelope before it does anything else with a file, recycling the original, so a wrapped
delivery is repaired on the next run wherever it sits.

**The lesson is about the check, not the envelope**: a strict `%PDF` magic-byte test found in
seconds what a year of readers that tolerate anything never mentioned. Nothing in this pipeline
had ever asserted that a PDF was a PDF.

## Two mistakes this pipeline invites

Both surfaced repeatedly in the review of PR #797, and both are about language rather than
logic. Every step here is an attempt to read something that may be unreadable, which is what
makes them so easy to make.

**A failure to read something is not a fact about the thing read.** An extraction that raised,
a `pdftotext` that exited non-zero, a log that would not open, two pages that came back blank
— none of these establishes that the PDF has no text, or that the log is corrupt. Four
findings in that one review were this same conflation in different disguises, and each had
written "it is not there" where the code only knew "we did not see it". The archive's whole
value is that its records say what was established, so a provenance line asserting more than
the run checked is worse than no line.

**Say what the code does, not what you hope it bounds.** Three successive wordings of one
comment in `fetch_pmc.py` claimed a delay bound the code did not deliver — because
`urlopen()`'s timeout applies per socket operation, not per request, and `ncbi._get()` has no
read deadline at all. A timeout that tightens as a budget runs out *shortens* an overrun; it
does not bound it. In this pipeline the honest form is almost always "this shortens it", and a
figure worth writing down is an illustration, not a worst case.

## What runs, in the order `update.bat` runs it

`fetch_pmc.py` runs first but **outside** the `&&` chain, as `check_in_step.py` does at the end.
Fetching first means a paper off PMC is filed, OCR-checked, counted and queued for a card in the
same run; keeping it out of the chain means a PMC outage, or one locked temporary file, cannot stop
the filing of papers already sitting in Downloads. Only the steps that must see each other's output
belong in the chain.


| Script | What it does |
|---|---|
| `fetch_pmc.py` | Downloads the free PMC copy of any paper the reference list **or the download queue** wants, verifies its **first two pages** against the title and first author, and files it. Two outcomes are neither a match nor a mismatch, and they are treated differently. **Extraction succeeded and returned no text from those two pages**, and PubMed lists a first author: filed unverified, with `check by eye` in its log line, for `ocr_scans.py` to look at later — what a blank two pages shows is that there was nothing to check against, not that the document has no text layer. **Extraction failed** (`pdftotext` missing, timing out, or exiting non-zero), or PubMed lists **no author** to check against: nothing was established about the PDF, so it is **held, not filed**, for the next run to retry. Run outside the `&&` chain, so a PMC outage cannot stop the filing below. |
| `file_supplements.py` | Files publisher supplements beside their article, by sha. |
| `file_downloads.py` | Identifies each new PDF in Downloads by printed DOI, or by title and first author, and files it as `PMID_<pmid>.pdf`. Accepts a file named by hand when the reference list **or the download queue** asks for that PMID. Strips a multipart form envelope left around a Clio delivery first. |
| `ocr_scans.py` | Adds a text layer to a scan that has none. |
| `merge.py` | Merges the per-document extractions. |
| `build.py` | Rebuilds `stanpumpR-references.xlsx` and `stanpumpr-queue.csv` from the works the stanpumpR source cites, keeping rows `arrivals.py` added. |
| `digest_prep.py` | Works out which papers still need a digest card. |
| `arrivals.py` | Closes the loop: reports which papers a model is **waiting for** that have now arrived, and adds any wanted paper the download queue does not yet carry. |
| `check_in_step.py` | Reports whether these copies and the archive's have drifted apart. |

`update.bat` then launches `digests.bat` detached, which writes up to N missing cards through
`digest_run.py` and opens a pull request.

Nothing here is run by the R package, and `papers/` is excluded from the built package
(`.Rbuildignore`), so none of this reaches CRAN or the deployed app.
