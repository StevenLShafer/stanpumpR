"""Download free PMC copies from the PMC Article Datasets bucket (s3://pmc-oa-opendata), verify, file as PMID_<pmid>.pdf."""
import json, os, re, subprocess, sys, time, urllib.request
from openpyxl import load_workbook
from ncbi import norm

# A first author called Jarčuška or Škrlín crashed the run on a cp1252 console, AFTER the PDF
# had been filed but BEFORE the fetch log was written, so the run lost its record of
# everything it had just done (2026-11-18).
sys.stdout.reconfigure(encoding="utf-8")

ARCHIVE = r"C:\dev\stanpumpRPapers"
HERE = os.path.dirname(os.path.abspath(__file__))
BUCKET = "https://pmc-oa-opendata.s3.amazonaws.com/"
TMP = os.path.join(HERE, "pmc_tmp")
os.makedirs(TMP, exist_ok=True)


# update.bat runs this before the filing chain, so a stalled PubMed or PMC request delays the
# filing of papers ALREADY in Downloads.  Four tries at a 120 s timeout is about 8 minutes for
# one URL, and a run fetches several.  So the run gets a wall-clock budget: no retry is started
# past it, the body is read in bounded pieces that stop at it, and no further paper is
# attempted.  Papers not reached stay `pending` and the next run picks them up.
#
# What it does NOT cover: the PubMed metadata lookup below, which goes through
# ncbi.esummary() -> ncbi._get(), six tries at a 60 s timeout plus 30 s of backoff -- about
# 6.5 minutes PER 150-PMID CHUNK, before the loop's first budget check.  esummary() does not
# forward `tries`, and bounding it there would change the retry policy of a module every
# script in this pipeline shares: a wider decision than this script should take alone, and
# left for Shafer.  What is bounded here is the number of CHUNKS -- one per run -- so the
# figure is a single 6.5 minutes rather than a multiple of it, and the honest worst case for
# delaying the filing below is about 6.5 min + 600 s + one 120 s read.  Two earlier wordings
# of this comment overclaimed; a bound stated in a comment is a claim the code must keep
# (CodeRabbit, PR #797).
FETCH_BUDGET_SECONDS = 600
METADATA_PMIDS_PER_RUN = 150                     # one esummary chunk; the rest wait for the next run
DEADLINE = time.time() + FETCH_BUDGET_SECONDS


def budget_spent():
    return time.time() > DEADLINE


def get(url, binary=False):
    for i in range(4):
        if budget_spent():
            raise TimeoutError("the run's %d s fetch budget was spent" % FETCH_BUDGET_SECONDS)
        try:
            req = urllib.request.Request(url, headers={"User-Agent": "stanpumpR-archive/1.0"})
            # The socket timeout TIGHTENS as the budget runs out, so the last request cannot
            # sit for a full 120 s past a deadline that has already passed.
            left = max(1.0, DEADLINE - time.time())
            with urllib.request.urlopen(req, timeout=min(120.0, left)) as r:
                # timeout= bounds each socket operation, not the whole transfer: a server
                # trickling bytes kept r.read() running past the budget, so the delay to
                # filing could exceed what the comment above promised (CodeRabbit, PR #797).
                # read1() returns after ONE underlying recv, where read(n) may loop to fill n.
                #
                # What is still NOT checked between recvs: for a CHUNKED response, read1()
                # first reads the chunk-size line with readline(), and a server trickling that
                # line stalls inside one call.  This path is live, not theoretical -- measured
                # 2026-11-18, the S3 bucket LISTING comes back chunked (the PDF itself carries
                # Content-Length) -- so the honest residual is one chunk-size line, a few
                # bytes, each costing at most the tightened timeout above.  Cancelling that
                # properly needs a watchdog closing the socket from another thread, which is
                # not worth its complexity here for a handful of bytes from S3; if this script
                # ever fetches from a host that is not trusted, reopen it.
                reader = getattr(r, "read1", None) or r.read
                parts = []
                while True:
                    if budget_spent():
                        raise TimeoutError("the run's %d s fetch budget was spent while reading %s"
                                           % (FETCH_BUDGET_SECONDS, url))
                    piece = reader(1 << 16)
                    if not piece:
                        break
                    parts.append(piece)
                b = b"".join(parts)
            return b if binary else b.decode("utf-8", "replace")
        except Exception:
            if i == 3:
                raise
            time.sleep(3 * (i + 1))


def first_text(path, pages=2):
    """The text of the first `pages` pages, and whether reading it FAILED.

    Returning "" for both an image-only scan and a pdftotext that was missing, timed out or
    crashed made the two indistinguishable, so a PDF filed after an extraction failure was
    logged as having "no text layer" -- a provenance record saying something the run never
    established.  file_downloads.py already draws this distinction (`pdf_text(strict=)`, and
    the note at its line 632); this script did not (CodeRabbit, PR #797)."""
    try:
        r = subprocess.run(["pdftotext", "-l", str(pages), "-q", path, "-"], capture_output=True, timeout=60)
        if r.returncode != 0:                    # 1 open error, 2 output, 3 permission, 99 other
            return "", True                     # pdftotext warns at exit 0, so this is a real failure
        return r.stdout.decode("utf-8", "replace"), False
    except Exception:
        return "", True


def words(s):
    return set(w for w in re.findall(r"[a-z0-9]+", (s or "").lower()) if len(w) > 3)


ws = load_workbook(os.path.join(ARCHIVE, "stanpumpR-references.xlsx"))["References"]
h = [c.value for c in ws[1]]
I = {k: i for i, k in enumerate(h)}
todo = [{k: r[I[k]] for k in ("Ref", "PMID", "PMCID", "First author", "Year", "Title", "Journal / source", "Pages")}
        for r in ws.iter_rows(min_row=2, values_only=True) if r[I["Archive status"]] == "To download (free at PMC)"]

# The spreadsheet lists the works the stanpumpR SOURCE cites.  A paper can be wanted long
# before anything cites it -- a registry's `to_obtain`, or a reading list added by
# queue_add.py -- and those live only in the download queue, so without this they would never
# be fetched even when PMC has them free (2026-11-18).  The verification below is unchanged:
# a file whose first page does not carry the title and first author is still HELD.
# The queue is an ADDITION to the workbook, never a precondition for it: a missing or
# unreadable queue, or a PubMed outage, must not stop the workbook's own downloads, which
# already carry all the metadata they need (CodeRabbit, PR #784).
try:
    import csv as _csv
    known = {str(t["PMID"]) for t in todo if t["PMID"]}
    qwant = []
    with open(os.path.join(ARCHIVE, "stanpumpr-queue.csv"), encoding="utf-8") as _f:
        for _r in _csv.DictReader(_f):
            _p = (_r.get("pmid") or "").strip()
            _n = _r.get("notes") or ""
            # Selected by the requester prefix alone.  The "free at PMC" marker is written
            # once, when the row is added, so a paper that gained a PMCID afterwards would be
            # excluded for ever if it were the filter; PubMed decides below instead.
            if (_p and _p not in known and _n.startswith("wanted by ")
                    and (_r.get("status") or "").strip().lower() == "pending"
                    and not os.path.exists(os.path.join(ARCHIVE, "PMID_%s.pdf" % _p))):
                qwant.append(_p)
                known.add(_p)
    if qwant and budget_spent():
        print("the fetch budget was spent before the queue's metadata could be looked up;"
              " its %d wanted paper(s) wait for the next run" % len(qwant))
        qwant = []
    if len(qwant) > METADATA_PMIDS_PER_RUN:      # keep the lookup to ONE esummary chunk
        print("the download queue wants %d papers; looking up the first %d this run"
              % (len(qwant), METADATA_PMIDS_PER_RUN))
        qwant = qwant[:METADATA_PMIDS_PER_RUN]
    if qwant:
        from ncbi import esummary
        _meta = esummary(qwant)
        _free = 0
        for _p in qwant:
            _m = _meta.get(_p)
            if not _m or not _m.get("pmcid"):
                continue
            todo.append({"Ref": "download queue", "PMID": _p, "PMCID": _m["pmcid"],
                         "First author": _m.get("first_author", ""), "Year": _m.get("year"),
                         "Title": _m.get("title", ""), "Journal / source": _m.get("journal", ""),
                         "Pages": _m.get("pages", "")})
            _free += 1
        print("from the download queue: %d wanted paper(s), %d of them free at PMC"
              % (len(qwant), _free))
except FileNotFoundError:
    print("no stanpumpr-queue.csv: the workbook's own rows only")
except Exception as _e:                          # noqa: BLE001 -- an outage, not a reason to stop
    print("could not read the download queue or reach PubMed (%s); the workbook's own rows only"
          " -- its requests stay eligible for the next run" % _e)

# build.py reads this log to mark the by-hand ones.  It used to be written ONCE, after the
# loop, so any raise lost the record of every paper already filed in that run -- which has
# happened (a first author called Jarcuska crashed the run on a cp1252 console, 2026-11-18;
# that fix addressed the console, not this).  And since fetch_pmc.py runs outside update.bat's
# && chain, build.py now runs anyway and marks the filed paper archived, so the next run skips
# it and its provenance -- licence, dataset, title overlap, any `check by eye` -- is lost for
# good.  So every paper's result is saved before the next is begun, and a failure on one paper
# is recorded against that paper while the batch carries on (CodeRabbit, PR #797).
log_path = os.path.join(HERE, "pmc_fetch_log.json")


def save_log(entries):
    """Merge this run's results into the log on disk.

    Called from the per-paper `finally`, which sits OUTSIDE the per-paper try, so anything
    raised here ends the run -- and the paper just filed would lose its entry while the next
    run failed on its first save, blocking the batch until somebody repaired the file by hand
    (CodeRabbit, PR #797).  An unreadable log is therefore SET ASIDE rather than parsed or
    deleted (nothing in this archive is deleted) and the run starts a fresh one; a write that
    fails is reported and the batch carries on, this run's results still in memory for the
    next save to pick up."""
    merged = {}
    if os.path.exists(log_path):
        try:
            with open(log_path, encoding="utf-8") as f:
                merged = {e["pmid"]: e for e in json.load(f)}
        except OSError as e:
            # COULD NOT READ IT, which is not "its contents are bad".  This archive lives on
            # a Google Drive-backed path where a file written moments earlier can briefly
            # refuse to open, and the first version of this code answered that by moving the
            # live log aside -- throwing away good provenance over a sync hiccup.  Observed
            # while testing this very PR.  So: leave the file alone, skip this save, and let
            # the next one pick the results up.  Same distinction as `first_text()`: a failure
            # to read says nothing about what was being read (CodeRabbit, PR #797).
            print("  the fetch log could not be opened (%s); leaving it alone and trying again"
                  " on the next save" % e, flush=True)
            return False
        except (ValueError, KeyError, TypeError) as e:
            # The CONTENTS are bad -- malformed or truncated JSON, or not the shape expected,
            # which an interrupted in-place write before this PR could leave behind.  Set it
            # aside with a timestamp (nothing in this archive is deleted) and start fresh.
            kept = log_path + ".unreadable-" + time.strftime("%Y%m%d-%H%M%S")
            try:
                os.replace(log_path, kept)
                where = os.path.basename(kept)
            except OSError:
                where = "could not be set aside"
            print("  the fetch log is not readable JSON (%s); kept as %s, starting a fresh one"
                  % (e, where), flush=True)
    merged.update({e["pmid"]: e for e in entries})
    tmp_log = log_path + ".new"
    try:
        with open(tmp_log, "w", encoding="utf-8") as f:
            json.dump(list(merged.values()), f, indent=1)
        os.replace(tmp_log, log_path)            # never a half-written log
        return True
    except Exception as e:                       # noqa: BLE001 -- do not end the run over this
        print("  could not write the fetch log (%s); this run's results stay in memory" % e,
              flush=True)
        return False


log = []
for t in todo:
    if budget_spent():
        print("the %d s fetch budget is spent; %d paper(s) left for the next run"
              % (FETCH_BUDGET_SECONDS, len(todo) - len(log)), flush=True)
        break
    pmc, pmid = t["PMCID"], t["PMID"]
    dst = os.path.join(ARCHIVE, "PMID_%s.pdf" % pmid)
    entry = {"ref": t["Ref"], "pmid": pmid, "pmcid": pmc, "first_author": t["First author"], "year": t["Year"]}
    try:
        if os.path.exists(dst):
            entry["result"] = "already in archive"
            continue
        lst = get(BUCKET + "?list-type=2&prefix=%s.&max-keys=100" % pmc)
        keys = re.findall(r"<Key>([^<]+)</Key>", lst)
        vers = sorted({int(m.group(1)) for k in keys for m in [re.match(r"%s\.(\d+)/" % pmc, k)] if m})
        if not vers:
            entry["result"] = "not in PMC Article Datasets (free to read only)"
            print(pmc, pmid, "-- not in bucket")
            continue
        v = vers[-1]
        base = "%s.%d/%s.%d" % (pmc, v, pmc, v)
        meta = {}
        if base + ".json" in keys:
            try:
                meta = json.loads(get(BUCKET + base + ".json"))
            except Exception:
                pass
        entry["license"] = meta.get("license_code") or meta.get("license") or ""
        entry["dataset"] = meta.get("is_manuscript") and "author_manuscript" or meta.get("article_type", "")
        if base + ".pdf" not in keys:
            entry["result"] = "in bucket but no PDF (XML/text only)"
            print(pmc, pmid, "-- no pdf; keys:", [k.split("/")[-1] for k in keys])
            continue
        tmp = os.path.join(TMP, "%s.pdf" % pmc)
        with open(tmp, "wb") as f:
            f.write(get(BUCKET + base + ".pdf", binary=True))
        txt, unreadable = first_text(tmp)
        tw, pw = words(t["Title"]), words(txt)
        overlap = len(tw & pw) / max(1, len(tw))
        # A PubMed record can have a PMCID and no authors at all; `.split()[0]` on "" raised
        # IndexError, which ended the batch after PDFs were filed but before the log was written
        # (CodeRabbit, PR #784).  What happens to such a file is decided below.
        _first = (t["First author"] or "").split()
        au = norm(_first[0]) if _first else ""
        au_ok = au and au in norm(txt)
        entry.update(bytes=os.path.getsize(tmp), title_overlap=round(overlap, 2), author_on_page=bool(au_ok), version=v)
        # A PDF moved into the archive before its result was recorded could end up archived
        # with no provenance at all: build.py marks it archived, the next run sees the file and
        # reports "already in archive", and the licence, overlap and any check-by-eye verdict
        # are gone.  So the result is WRITTEN first and the file moved only if that succeeded
        # (CodeRabbit, PR #797).  If the move then fails, the per-paper `except` overwrites the
        # entry with FAILED and the `finally` saves it again, so the log self-corrects.
        def file_it(result):
            entry["result"] = result
            if save_log(log + [entry]):
                os.replace(tmp, dst)
                return
            entry["result"] = ("HELD: the fetch log could not be written, so the PDF was not"
                               " filed; it stays pending for the next run")

        if overlap >= 0.6 and au_ok:
            file_it("downloaded")
        elif unreadable:
            # Nothing was learned about this PDF -- pdftotext was missing, timed out or failed.
            # That is a fact about THIS MACHINE, not about the paper, so it is no basis for
            # putting an unverified file in the archive.  Held; the next run tries again.
            entry["result"] = "HELD: could not read the PDF's text, so neither title nor author could be confirmed"
        elif not au:
            # No author to check against, in EITHER case.  The no-text fallback below files a
            # PDF unverified, on the strength of the workbook's vetted metadata; a queue row is
            # a bare PubMed record, and one without authors leaves nothing to confirm at all.
            # The comment above claimed this file was held and the code filed it anyway
            # (CodeRabbit, PR #784) -- a comment asserting what the code does not do.
            entry["result"] = "HELD: PubMed lists no author, so neither title nor author could be confirmed"
        elif not txt.strip():
            # Unverifiable by nature rather than by accident, and ocr_scans.py will look at it
            # later.  Still filed, still flagged -- but the note says only what was LOOKED AT:
            # first_text() reads two pages, so empty text there does not establish that the
            # rest of the PDF has none (CodeRabbit, PR #797; the same conflation of "we did
            # not see it" with "it is not there" as the two above).
            file_it("downloaded (no text in the first two pages - check by eye)")
        else:
            entry["result"] = "HELD: first page does not match title/author"
        print(pmc, pmid, t["First author"], t["Year"], "->", entry["result"], "overlap=%.2f author=%s %s" % (overlap, bool(au_ok), entry["license"]))

    except Exception as e:                       # this paper's failure, not the batch's
        entry["result"] = "FAILED: %s: %s" % (type(e).__name__, e)
        print(pmc, pmid, "->", entry["result"], flush=True)
    finally:
        # KeyboardInterrupt and SystemExit are not Exception, so Ctrl+C during a paper skips
        # the handler above and lands here with no `result` set -- and build.py reads
        # pl["result"] unguarded, so one interrupted paper could block every later workbook
        # rebuild until that PMID was fetched again (CodeRabbit, PR #797).  This script can
        # run for minutes, so Ctrl+C is an ordinary event, not an edge case.
        entry.setdefault("result", "INTERRUPTED: the run ended before this paper finished")
        log.append(entry)
        save_log(log)                            # before the next paper is begun

from collections import Counter
print("this run:", Counter(e["result"] for e in log))
