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

## What runs, in the order `update.bat` runs it

`fetch_pmc.py` runs first but **outside** the `&&` chain, as `check_in_step.py` does at the end.
Fetching first means a paper off PMC is filed, OCR-checked, counted and queued for a card in the
same run; keeping it out of the chain means a PMC outage, or one locked temporary file, cannot stop
the filing of papers already sitting in Downloads. Only the steps that must see each other's output
belong in the chain.


| Script | What it does |
|---|---|
| `fetch_pmc.py` | Downloads the free PMC copy of any paper the reference list **or the download queue** wants, verifies its **first two pages** against the title and first author, and files it. Two cases are not a match and not a mismatch, and they are treated differently: a PDF whose first two pages hold **no text at all** is filed unverified with `check by eye` in its log line, being a scan that `ocr_scans.py` will read later; but when the **extraction itself fails** — `pdftotext` missing, timing out, or exiting non-zero — nothing was learned about the PDF, so it is **held, not filed**, and the next run tries again. Run outside the `&&` chain, so a PMC outage cannot stop the filing below. |
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
