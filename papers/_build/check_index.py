"""Assert that the repository's own accounting is complete, and say loudly when it is not.

Two failures on 2026-11-21 were both SILENT, and neither would have been found by reading any
report the pipeline wrote:

  * 85 PDFs had been in the archive since 17-19 October and were absent from digest_manifest.json,
    so they appeared nowhere in STATUS.md -- not as digested, not as waiting. The manifest said
    1893 while the folder held 1978. Nothing compared the two.
  * 36 PDFs had no text layer at all (0 letters). ocr_scans.py exists precisely to fix that and
    had never been run over them. Nothing compared the text files with the PDFs.

Both are the same shape: a set that should equal another set, with nobody subtracting them. This
script does the subtraction. It is read-only.

  python check_index.py            report, exit 1 if anything diverges
  python check_index.py --quiet    print only when something diverges (for the chain)
  python check_index.py --manifest <path>   judge against another manifest, to test the check

Run it LAST in update.bat, after file_downloads, ocr_scans and digest_prep, so it judges the
state those leave behind. A non-zero exit means the accounting is wrong, not that a step failed.
"""
import io
import json
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ARCH = os.path.dirname(HERE)
TEXT = os.path.join(HERE, "text")
CONTEXT = os.path.join(HERE, "context")
MANIFEST = os.path.join(HERE, "digest_manifest.json")

sys.stdout.reconfigure(encoding="utf-8")

quiet = "--quiet" in sys.argv
if "--manifest" in sys.argv:                     # so the check itself can be tested
    MANIFEST = sys.argv[sys.argv.index("--manifest") + 1]
problems = []
notes = []


def stems_on_disk():
    return {f[:-4] for f in os.listdir(ARCH) if f.lower().endswith(".pdf")}


def stems_in(folder, ext):
    if not os.path.isdir(folder):
        return set()
    return {f[: -len(ext)] for f in os.listdir(folder) if f.endswith(ext)}


def show(label, other, missing, extra, kind_missing, kind_extra):
    """One comparison. `missing` is in the archive and not in `other`; `extra` is the reverse."""
    if missing:
        problems.append("%d PDF(s) %s: %s%s"
                        % (len(missing), kind_missing, ", ".join(sorted(missing)[:6]),
                           "" if len(missing) <= 6 else ", ..."))
    if extra:
        notes.append("%d %s: %s%s"
                     % (len(extra), kind_extra, ", ".join(sorted(extra)[:6]),
                        "" if len(extra) <= 6 else ", ..."))
    if not quiet:
        print("  %-34s archive %5d   other %5d   missing %4d   orphaned %4d"
              % (label, len(disk), len(other), len(missing), len(extra)))


disk = stems_on_disk()

# 1. every PDF is in the manifest
man = set()
if os.path.exists(MANIFEST):
    man = {x["stem"] for x in json.load(io.open(MANIFEST, encoding="utf-8")) if "stem" in x}
else:
    problems.append("digest_manifest.json is missing; digest_prep.py has never run")

# 2. every PDF has an extracted text file
txt = stems_in(TEXT, ".txt")

# 3. every PDF has a context file
ctx = stems_in(CONTEXT, ".json")

if not quiet:
    print("Repository accounting")
    print("  %-34s %d" % ("PDFs in the archive", len(disk)))

show("in digest_manifest.json", man, disk - man, man - disk,
     "are not in digest_manifest.json (digest_prep.py has not seen them)",
     "manifest entry/entries name a PDF that is gone")
show("with an extracted text file", txt, disk - txt, txt - disk,
     "have no extracted text (digest_prep.py extracts it; ocr_scans.py supplies the layer)",
     "text file(s) name a PDF that is gone")
show("with a context file", ctx, disk - ctx, ctx - disk,
     "have no context file", "context file(s) name a PDF that is gone")

# 4. a PDF whose text is empty needs OCR, and ocr_scans.py should have tried
empty = []
for s in sorted(disk & txt):
    p = os.path.join(TEXT, s + ".txt")
    try:
        body = io.open(p, encoding="utf-8", errors="replace").read()
    except OSError:
        continue
    pages = body.count("\f") + (1 if body and not body.endswith("\f") else 0)
    letters = len(re.findall(r"[A-Za-z]", body))
    if letters < 100 * max(pages, 1):
        empty.append((s, pages, letters))

if empty:
    tried = set()
    log = os.path.join(HERE, "ocr_log.json")
    if os.path.exists(log):
        tried = {e.get("stem") for e in json.load(io.open(log, encoding="utf-8"))
                 if isinstance(e, dict)}
    # A file ocr_scans.py tried and could not improve is not a problem: a label page or a
    # one-page supplement can be sparse because it holds little text, and its guard keeps the
    # original when OCR gains nothing. One it has NEVER tried is the 2026-11-21 case.
    never = [e for e in empty if e[0] not in tried]
    if never:
        problems.append("%d PDF(s) have no readable text and ocr_scans.py has never tried them: %s%s"
                        % (len(never), ", ".join(s for s, _, _ in never[:6]),
                           "" if len(never) <= 6 else ", ..."))
    sparse = [e for e in empty if e[0] in tried]
    if sparse and not quiet:
        print("  %-34s %d (OCR tried; sparse, not broken)" % ("thin text layer", len(sparse)))

if not quiet:
    print()

if problems:
    print("THE ACCOUNTING IS WRONG:")
    for p in problems:
        print("  - " + p)
    if notes:
        print("Also:")
        for n in notes:
            print("  - " + n)
    print()
    print("A PDF the manifest does not list can never be digested, and nothing else reports it.")
    sys.exit(1)

if notes:
    if not quiet:
        print("Consistent, with stale leftovers:")
        for n in notes:
            print("  - " + n)
    sys.exit(0)

if not quiet:
    print("Consistent: every PDF is indexed, extracted and readable.")
sys.exit(0)
