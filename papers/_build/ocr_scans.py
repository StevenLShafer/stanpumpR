"""Add an OCR text layer to the archive's scanned PDFs, in place, keeping the file names.

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


def page_letters(pdf):
    """Letters per page from pdftotext (pages split on form feeds)."""
    r = subprocess.run([PDFTOTEXT, "-layout", "-enc", "UTF-8", pdf, "-"], capture_output=True)
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
for fn in sorted(os.listdir(ARCH)):
    if not fn.lower().endswith(".pdf"):
        continue
    stem = fn[:-4]
    if ONLY and stem not in ONLY:
        continue
    pdf = os.path.join(ARCH, fn)
    letters = page_letters(pdf)
    if sum(letters) >= 100 * len(letters):
        continue  # has a text layer
    mode = "--force-ocr" if any(0 < n for n in letters) else "--skip-text"
    print(f"{stem}: {len(letters)} pages, {sum(letters)} letters -> {mode}", flush=True)
    if DRY:
        continue
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
    entry["letters_after"] = sum(after)
    if not same_pages or sum(after) <= sum(letters):
        entry.setdefault("error", "page count changed or no text gained; original kept")
        print(f"  REJECTED: {entry['error']}")
        log.append(entry)
        continue
    st = os.stat(pdf)
    if not recycle(pdf):
        entry["error"] = "could not recycle the original; original kept"
        print(f"  {entry['error']}")
        log.append(entry)
        continue
    shutil.move(out, pdf)
    os.utime(pdf, (st.st_atime, st.st_mtime))  # digests written from the page images are still current
    entry["sha_after"] = sha(pdf)
    txt = os.path.join(TEXT, stem + ".txt")
    if os.path.exists(txt):
        os.remove(txt)  # digest_prep.py re-extracts it
    print(f"  ok: {sum(after)} letters on {len(after)} pages, {entry['seconds']} s")
    log.append(entry)
    done.append(stem)

json.dump(log, open(LOG, "w", encoding="utf-8"), indent=1)
print(f"OCR'd {len(done)} file(s): {' '.join(done)}" if done else "nothing OCR'd")
