"""Add an OCR text layer to the archive's scanned PDFs, in place, keeping the file names.

Two runs of this script at once no longer race on the same PDF (CodeRabbit, PR #784, which
pressed the point twice and was right to).  The `.pre-ocr` absence check and the copy that
creates it are not atomic, so an interleaving existed in which one run published its
replacement, kept `.pre-ocr` because recycling failed, and the other run then copied that
replacement over the retained original -- losing the TRUE original, the one thing this script
must never lose.  `claimPdf()` now makes the whole mutating transaction exclusive -- from the
decision to OCR, through staging and publication, to disposal of the backup -- with a lock
the OPERATING SYSTEM holds on an open handle (msvcrt.locking on Windows, fcntl.flock
elsewhere).  A PDF another run holds is skipped and said.  Reading the file needs no lock,
because publication is a single os.replace: a reader sees the old file or the new one, never
half of either.

A lock named only by a PATH was tried first and is not enough: with an age-based takeover for
a crashed run, two runs can both see one stale lock and the second then removes the FRESH lock
the first has just created.  A kernel lock has no staleness to solve, since the OS drops a
dead process's lock by itself.

The download queue's read-modify-write is still unlocked (see queue_add.py).  It is a
different resource -- one file with several writers, rather than one file per transaction --
so it needs its own lock, not this one.

A scan (no usable text layer) is found the way digest_prep.py finds one: fewer than 100 letters per page from
pdftotext. Each is run through ocrmypdf (Tesseract + Ghostscript), which draws an invisible text layer under the
untouched page image, so the file looks the same but pdftotext, grep and digest_prep.py can read it. A file whose
only text is a library download stamp is forced through OCR on every page (--force-ocr); a file with no text at
all gets --skip-text, which OCRs every page that has none.

The OCR'd file replaces the original only when it has the same page count and more letters; the original goes to
the Recycle Bin (recycle.py). The archived file keeps its original modification time, so digests written from the
page images are not reported stale, and _build/text/<stem>.txt is deleted so digest_prep.py re-extracts it.
Results are appended to _build/ocr_log.json.

Usage: python ocr_scans.py [--dry-run] [--only STEM ...] [--jobs N]
Needs: C:\\Program Files\\Tesseract-OCR, C:\\Program Files\\gs\\gs*\\bin, pip install ocrmypdf (into C:\\Python314).
"""
import glob, hashlib, json, os, re, shutil, subprocess, sys, time

HERE = os.path.dirname(os.path.abspath(__file__))
ARCH = os.path.dirname(HERE)
TEXT = os.path.join(HERE, "text")
TMP = os.path.join(HERE, "ocr_tmp")
LOG = os.path.join(HERE, "ocr_log.json")
PDFTOTEXT = r"C:\Program Files\Git\mingw64\bin\pdftotext.exe"
TESS = r"C:\Program Files\Tesseract-OCR"
GS = sorted(glob.glob(r"C:\Program Files\gs\gs*\bin"))
sys.path.insert(0, HERE)
from recycle import recycle  # noqa: E402

args = sys.argv[1:]
DRY = "--dry-run" in args
JOBS = args[args.index("--jobs") + 1] if "--jobs" in args else "4"
ONLY = args[args.index("--only") + 1:] if "--only" in args else None
os.makedirs(TMP, exist_ok=True)
env = dict(os.environ)
env["PATH"] = os.pathsep.join([TESS] + GS + [env.get("PATH", "")])


def claimPdf(pdf):
    """Claim one PDF for this run, or None if another run holds it.

    The lock is held by the OPERATING SYSTEM on an open handle, not by the existence of a
    path.  A pathname lock with an age-based takeover, which this was first, has a window of
    its own: two runs can both see one stale lock, and the second then removes the FRESH lock
    the first has just created, so both proceed -- the very race the lock exists to stop
    (CodeRabbit, PR #784, which was right to keep pushing).  A second age or identity check
    before the remove only narrows that window.

    A kernel lock has no staleness problem to solve: when a process dies the OS drops its
    lock, so a crashed run blocks nobody and nothing has to be reclaimed by guesswork.  The
    lock file lives in TMP and is never deleted, so no run can delete a lock it does not hold.
    """
    os.makedirs(TMP, exist_ok=True)
    path = os.path.join(TMP, os.path.basename(pdf) + ".lock")
    f = open(path, "a+b")
    try:
        if os.name == "nt":
            import msvcrt
            f.seek(0)
            msvcrt.locking(f.fileno(), msvcrt.LK_NBLCK, 1)
        else:
            import fcntl
            fcntl.flock(f.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
    except OSError:                              # held by another run
        f.close()
        return None
    return f


def releasePdf(f):
    """Drop the lock and close the handle.  The lock file itself stays."""
    try:
        if os.name == "nt":
            import msvcrt
            f.seek(0)
            msvcrt.locking(f.fileno(), msvcrt.LK_UNLCK, 1)
        else:
            import fcntl
            fcntl.flock(f.fileno(), fcntl.LOCK_UN)
    except OSError:
        pass
    finally:
        f.close()


class LaunchError(RuntimeError):
    """pdftotext could not be started at all: fatal for the batch, not for one PDF."""


def page_letters(pdf):
    """Letters per page from pdftotext (pages split on form feeds), or None if it FAILED.

    A failure is not the same as a page with no text, and the difference decides whether this
    script replaces the file.  pdftotext timing out or crashing returned empty output, the PDF
    read as a scan with no text layer, and it was OCR'd and its original recycled although its
    text layer was perfectly good -- and the guard below could not catch it, because OCR then
    "gained" text against a count of zero (CodeRabbit on PR #780, 2026-11-18)."""
    try:
        r = subprocess.run([PDFTOTEXT, "-layout", "-enc", "UTF-8", pdf, "-"],
                           capture_output=True, timeout=600)
    except subprocess.TimeoutExpired:
        return None                              # this PDF only: skip it, keep the batch going
    except OSError as e:
        # pdftotext could not be LAUNCHED -- missing, not executable, blocked.  Swallowing
        # this skipped every PDF in the archive and reported "nothing OCR'd" with a
        # successful exit: a silent no-op indistinguishable from "no scans to do".
        # FileNotFoundError alone was not enough; PermissionError and the other OSErrors
        # reach here too (CodeRabbit, PR #784, twice).
        raise LaunchError("cannot run %s (%s)" % (PDFTOTEXT, e)) from e
    except Exception:                            # anything else about THIS file
        return None
    if r.returncode != 0:
        return None
    text = r.stdout.decode("utf-8", "replace")
    pages = text.split("\f")
    if pages and pages[-1] == "":
        pages.pop()
    return [len(re.findall(r"[A-Za-z]", p)) for p in pages] or [0]


def sha(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def page_count(pdf):
    from pypdf import PdfReader
    return len(PdfReader(pdf).pages)


log = json.load(open(LOG, encoding="utf-8")) if os.path.exists(LOG) else []
done = []
launch_error = None
try:
  for fn in sorted(os.listdir(ARCH)):
    if not fn.lower().endswith(".pdf"):
        continue
    stem = fn[:-4]
    if ONLY and stem not in ONLY:
        continue
    pdf = os.path.join(ARCH, fn)
    # Reading needs no lock: publication below is a single os.replace, so a reader sees the
    # old file or the new one, never half of either.  The lock is taken once this PDF is
    # known to need OCR, which keeps a lock file per SCAN rather than per archived paper.
    letters = page_letters(pdf)
    if letters is None:
        print(f"{stem}: pdftotext failed -- skipped (cannot tell whether it has a text layer)", flush=True)
        continue
    if sum(letters) >= 100 * len(letters):
        continue  # has a text layer
    mode = "--force-ocr" if any(0 < n for n in letters) else "--skip-text"
    print(f"{stem}: {len(letters)} pages, {sum(letters)} letters -> {mode}", flush=True)
    if DRY:
        continue
    lock = claimPdf(pdf)
    if lock is None:
        print(f"{fn}: another run is working on this file -- skipped", flush=True)
        continue
    try:
        out = os.path.join(TMP, fn)
        if os.path.exists(out):
            os.remove(out)
        t0 = time.time()
        cmd = [sys.executable, "-E", "-m", "ocrmypdf", mode, "--language", "eng", "--output-type", "pdf",
               "--optimize", "0", "--jobs", JOBS, "--tesseract-timeout", "600", pdf, out]
        r = subprocess.run(cmd, capture_output=True, env=env)
        entry = {"stem": stem, "mode": mode, "date": time.strftime("%Y-%m-%d"), "pages": len(letters),
                 "letters_before": sum(letters), "sha_before": sha(pdf), "seconds": round(time.time() - t0)}
        if r.returncode != 0 or not os.path.exists(out):
            entry["error"] = r.stderr.decode("utf-8", "replace")[-1500:]
            print(f"  FAILED (rc {r.returncode}): {entry['error'].strip().splitlines()[-1] if entry['error'].strip() else ''}")
            log.append(entry)
            continue
        try:
            same_pages = page_count(out) == page_count(pdf)
        except Exception as e:  # noqa: BLE001
            same_pages, entry["error"] = False, f"page count check failed: {e}"
        after = page_letters(out)
        if after is None:                            # cannot verify the replacement: keep the original
            entry["error"] = "pdftotext failed on the OCR output; original kept"
            print(f"  REJECTED: {entry['error']}")
            log.append(entry)
            continue
        entry["letters_after"] = sum(after)
        if not same_pages or sum(after) <= sum(letters):
            entry.setdefault("error", "page count changed or no text gained; original kept")
            print(f"  REJECTED: {entry['error']}")
            log.append(entry)
            continue
        st = os.stat(pdf)
        # The archive path must hold a readable PDF at every instant.  Recycling the original
        # first and moving the replacement in second left it with NO file if the move failed
        # (CodeRabbit, PR #780); moving the original aside first and the replacement in second
        # still left a window with nothing there, and a run that stopped in that window left a
        # `.pre-ocr` file a later run would ignore (CodeRabbit, PR #784).  So: COPY the original
        # aside, stage the replacement on the archive's own filesystem, and publish it with ONE
        # os.replace.  If anything fails before that call, the original is untouched at `pdf`;
        # the copy is recycled only once the replacement is published.
        backup = pdf + ".pre-ocr"
        if os.path.exists(backup):
            # An earlier run's original that the Recycle Bin refused.  Overwriting it would
            # destroy the true original and keep that run's OCR output in its place.
            entry["error"] = f"{os.path.basename(backup)} is already here from an earlier run; original kept"
            print(f"  {entry['error']}")
            log.append(entry)
            continue
        staged = pdf + ".ocr-new"
        partial = backup + ".tmp"
        try:
            # The backup is written under a .tmp name and renamed, so `.pre-ocr` exists only when
            # it is COMPLETE.  Copying straight to `.pre-ocr` meant a run interrupted mid-copy
            # left a half file that the guard above then read as a retained original, refusing
            # the paper for ever (CodeRabbit, PR #784).  A leftover .tmp is this run's to clear.
            if os.path.exists(partial):
                os.remove(partial)
            shutil.copy2(pdf, partial)               # original still in place at `pdf`
            os.replace(partial, backup)
            shutil.move(out, staged)                 # onto the archive filesystem, so the swap is atomic
            os.replace(staged, pdf)                  # the one moment of change
        except Exception as e:                       # noqa: BLE001
            for leftover in (partial, backup, staged):
                if os.path.exists(leftover):
                    try:
                        os.remove(leftover)
                    except OSError:
                        pass
            entry["error"] = f"could not publish the OCR output ({e}); original kept"
            print(f"  {entry['error']}")
            log.append(entry)
            continue
        os.utime(pdf, (st.st_atime, st.st_mtime))  # digests written from the page images are still current
        if not recycle(backup):                      # the replacement is already in place
            entry["backup_kept"] = backup
            print(f"  note: the Recycle Bin refused the original; it is kept as {os.path.basename(backup)}")
        entry["sha_after"] = sha(pdf)
        txt = os.path.join(TEXT, stem + ".txt")
        if os.path.exists(txt):
            os.remove(txt)  # digest_prep.py re-extracts it
        print(f"  ok: {sum(after)} letters on {len(after)} pages, {entry['seconds']} s")
        log.append(entry)
        done.append(stem)
    finally:
        releasePdf(lock)

except LaunchError as e:
  # Save what this batch already did BEFORE giving up.  Exiting here without writing the log
  # lost the sha pair of every paper already replaced, and digest_prep.py then read those as
  # content changes and could mark good digests stale (CodeRabbit, PR #784).
  launch_error = e

json.dump(log, open(LOG, "w", encoding="utf-8"), indent=1)
print(f"OCR'd {len(done)} file(s): {' '.join(done)}" if done else "nothing OCR'd")
if launch_error:
  sys.exit("ocr_scans: %s -- install poppler or fix PDFTOTEXT" % launch_error)
