"""Screen the archive for anything that looks like a manuscript under peer review. Read-only.

Steven L. Shafer's point 6 of the Paper Repository plan, 2026-11-21: the repository must be
screened "carefully to be certain that there are no papers 'in review' that appear in 'Paper
Repository'", because a digest written from a paper in review could become public.

This is the standing screen, not a one-off: run it after a filing run, and whenever the
quarantine rules in inreview.py change, so a rule's false-positive rate is measured against
the whole archive rather than guessed.

  python screen_inreview.py              report
  python screen_inreview.py --quiet      print only when something is flagged
  python screen_inreview.py --calibrate  also print each flag's surrounding text, to tune a rule

Exit 1 when anything is flagged: a hit needs a person to look at the paper and decide. It does
NOT move, rename or delete anything -- a false positive must never cost a filed paper.
"""
import io
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ARCH = os.path.dirname(HERE)
TEXT = os.path.join(HERE, "text")

sys.path.insert(0, HERE)
from inreview import in_review_reason          # noqa: E402

sys.stdout.reconfigure(encoding="utf-8")
quiet = "--quiet" in sys.argv
calibrate = "--calibrate" in sys.argv

PAGES = 8000          # how much of the extracted text counts as "the front of the paper":
                      # an editorial stamp is on page 1, and reading further only finds the
                      # reference list, where every one of these phrases occurs innocently.

names = sorted(f for f in os.listdir(ARCH) if f.lower().endswith(".pdf"))
flagged = []
no_text = []
for n in names:
    tp = os.path.join(TEXT, n[:-4] + ".txt")
    body = None
    if os.path.exists(tp):
        try:
            body = io.open(tp, encoding="utf-8", errors="replace").read()[:PAGES]
        except OSError:
            # NOT `continue`: that skipped the name check as well, so a manuscript-number
            # filename whose text file cannot be read produced no warning at all, and under
            # --quiet the audit exited 0 having inspected nothing (CodeRabbit, PR #846).  The
            # read failure is reported AND the name is still judged, on body = None.
            no_text.append((n, "its extracted text could not be read, so only its name was judged"))
            body = None
    else:
        no_text.append((n, "it has no extracted text, so only its name was judged"))
    r = in_review_reason(os.path.join(ARCH, n), text=body)
    if r:
        flagged.append((n, r, body))

if not quiet:
    print("Screening %d archived PDFs for manuscripts under peer review" % len(names))
    print("  rules: a journal manuscript number, a review folder, page-1 review wording")
    print()

if flagged:
    print("FLAGGED (%d of %d) -- each needs a person to look at the paper:" % (len(flagged), len(names)))
    for n, r, body in flagged:
        print("  %-34s %s" % (n, r))
        if calibrate and body:
            m = re.search(r"(?i)(for peer review|tracked changes|manuscript draft|reviewer copy"
                          r"|blinded manuscript|under review|for review only|confidential)", body)
            if m:
                i = m.start()
                print("      ...%s..." % re.sub(r"\s+", " ", body[max(0, i - 120):i + 120]))
    print()

if no_text and not quiet:
    print("%d PDF(s) could be judged by name only (no extracted text):" % len(no_text))
    for n, why in no_text[:8]:
        print("  %-34s %s" % (n, why))
    if len(no_text) > 8:
        print("  ... and %d more" % (len(no_text) - 8))
    print()

if flagged:
    print("A flagged paper is NOT removed. Look at it, and if it is under review move it out of")
    print("the archive and delete its card: a card can become public, which is the risk guarded.")
    sys.exit(1)

if not quiet:
    print("Clean: nothing in the archive looks like a manuscript under peer review.")
sys.exit(0)
