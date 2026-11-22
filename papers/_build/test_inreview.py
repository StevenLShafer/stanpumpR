"""Every case the peer-review screen must get right. Run it: python test_inreview.py

    python test_inreview.py            run; exit 1 on any failure
    python test_inreview.py --archive  also screen the whole archive (needs the Drive folder)

These are the regression cases for `inreview.py`, and each one failed at some point during
PR #846 -- most of them because a rule CLEARED more evidence than it had grounds to. They are
here, in the repository, rather than in a session's scratchpad, because CodeRabbit asked twice
where they were and was right to: cases that live in a temp folder protect nothing, and the
commit messages that cited them were claiming a guarantee the repository did not hold.

A false positive costs one paper left in Downloads with a line saying why. A false negative puts
a colleague's unpublished manuscript in the archive and possibly its digest on the internet. So
the cases below are weighted the same way: the ones that must be CAUGHT are the ones to care
about, and the ones that must be CLEARED exist to stop the rules being tightened into noise.
"""
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from inreview import in_review_reason, flatten, SUBMISSION_NUMBER   # noqa: E402

PLAIN = os.path.join("C:" + os.sep, "Users", "x", "Downloads", "paper.pdf")
IN_REVIEW_FOLDER = os.path.join("C:" + os.sep, "Users", "x", "Downloads",
                                "Single Paper Review", "paper.pdf")

# A made-up submission number. Never a real one: a number names a specific manuscript at a
# specific journal, and this repository is public (PR #846).
MS = "XXX-D-26-01234"

CASES = [
    # ---- caught by name, wherever the file sits -------------------------------------------
    ("a submission number in the name", PLAIN.replace("paper.pdf", MS + ".pdf"), None, True),
    ("a submission number before an underscore", PLAIN.replace("paper.pdf", MS + "_main.pdf"),
     None, True),
    # '_' is a word character, so a \b anchor failed here and 4 of 5 live manuscripts went
    # uncaught by name, hidden behind the folder rule.
    ("a submission number before _reviewer_supplement",
     PLAIN.replace("paper.pdf", MS + "_reviewer_supplement.pdf"), None, True),
    ("a revision number then an underscore",
     PLAIN.replace("paper.pdf", MS + "_R1_reviewer_main.pdf"), None, True),

    # ---- caught by folder, cleared by a real preprint banner -----------------------------
    ("an unremarkable PDF in a review folder", IN_REVIEW_FOLDER, "A study of propofol.", True),
    ("a genuine preprint in a review folder", IN_REVIEW_FOLDER,
     "medRxiv preprint doi: https://doi.org/10.1101/2024.05.05; this version posted May 2024.",
     False),

    # ---- caught by an editorial system's own marks ----------------------------------------
    # Editorial Manager's attachment link: the ONLY evidence on two of the five live
    # supplements, which name and folder caught but text alone had missed.
    ("Editorial Manager's attachment link", PLAIN,
     "Figure\nClick here to access/download;Figure;Figure S1.pdf", True),
    ("a tracked-changes stamp", PLAIN, "Tracked Changes Manuscript", True),
    ("a blinded manuscript", PLAIN, "Blinded Manuscript\nMethods", True),
    # 12 letters: it failed a 20-letter screenability threshold and the classifier was handed
    # None, so the strongest evidence there is went unread.
    ("a page whose only text is a Reviewer Copy stamp", PLAIN, "Reviewer Copy", True),

    # ---- whitespace: pdftotext -layout wraps headers and pads columns ---------------------
    ("a stamp broken across lines", PLAIN, "Reviewer\nCopy", True),
    ("a stamp padded by layout", PLAIN, "For  Peer  Review", True),
    ("both at once", PLAIN, "Tracked  Changes\nManuscript", True),
    ("a submission number padded by layout", PLAIN, MS + "    main document", True),

    # ---- the preprint allowlist must describe THIS document ------------------------------
    # A bare "medrxiv" substring cleared a stamped manuscript that merely cited someone
    # else's preprint in its introduction.
    ("a stamped manuscript citing someone else's preprint", PLAIN,
     "For Peer Review\nIntroduction. As Smith J. medRxiv 2024 showed, propofol ...", True),
    ("a genuine medRxiv banner clears soft evidence", PLAIN,
     "medRxiv preprint doi: https://doi.org/10.1101/2024.05.05; this version posted May 2024.\n"
     "manuscript draft", False),
    ("a genuine Research Square banner", PLAIN,
     "Research Square\nThis is a preprint, a preliminary version of the manuscript.\n"
     "manuscript draft", False),
    ("a banner broken across lines still clears", PLAIN,
     "medRxiv\npreprint  doi: https://doi.org/10.1101/2024.05.05\nmanuscript draft", False),
    # A banner never clears STRONG evidence: a reviewer's copy of a preprinted paper is still
    # a reviewer's copy.
    ("a banner does not clear an editorial stamp", PLAIN,
     "medRxiv preprint doi: https://doi.org/10.1101/2024.01.01\n"
     "Click here to access/download;Figure;Figure S1.pdf", True),

    # ---- a published paper's own wording must not be read as review evidence -------------
    # MDPI leaves its template's running header in the PUBLISHED PDF, placeholder and all:
    # 2 of 2,257 archived papers hit this, and both are published.
    ("MDPI's unreplaced running header alone", PLAIN,
     "Pharmaceuticals 2022, 14, x FOR PEER REVIEW 2 of 18\nFavipiravir appears safe", False),
    ("the same, padded by layout", PLAIN,
     "Pharmaceuticals 2022, 14,  x  FOR  PEER  REVIEW   2 of 18\nFavipiravir appears safe", False),
    # The exception must excuse its own occurrence and nothing else: suppressing the phrase
    # everywhere cleared a document carrying the header AND a stamp of its own.
    ("MDPI's header beside a separate stamp of the same phrase", PLAIN,
     "Pharmaceuticals 2022, 14, x FOR PEER REVIEW 2 of 18\nFOR PEER REVIEW - DO NOT CIRCULATE",
     True),
    ("MDPI's header beside a tracked-changes stamp", PLAIN,
     "Pharmaceuticals 2022, 14, x FOR PEER REVIEW 2 of 18\nTracked Changes Manuscript", True),
    # The journal's own received/accepted footnote, on page 1 of a published paper. The phrase
    # "submitted for publication" alone was the worst rule tried: 17 false positives in 400.
    ("a journal's accepted-on footnote", PLAIN,
     "Desvenlafaxine is in development.\nSubmitted for publication 5 April 2017; "
     "accepted 30 October 2017.", False),

    # ---- ordinary papers, which must be left alone ---------------------------------------
    ("ordinary prose", PLAIN, "Propofol pharmacokinetics in 24 volunteers.", False),
    ("no text at all", PLAIN, None, False),
    ("a page of only whitespace and form feeds", PLAIN, "\f  \n \f", False),
    ("an archive name", PLAIN.replace("paper.pdf", "PMID_9559795.pdf"), "A study.", False),
    ("a supplement's archive name", PLAIN.replace("paper.pdf", "PMID_8357297_mmc1.pdf"),
     "Supplementary tables.", False),
    ("a label's archive name",
     PLAIN.replace("paper.pdf", "NJ_label-naropin_020533_2022-06-16.pdf"), "Ropivacaine.", False),
]


def main():
    bad = []
    for label, path, text, want in CASES:
        got = in_review_reason(path, text=text)
        if bool(got) != want:
            bad.append((label, want, got))
        print("  %s %-52s %s" % ("ok  " if bool(got) == want else "FAIL",
                                 label[:52], (got or "cleared")[:44]))

    # flatten() is the one text form every rule reads; a raw extraction must never be compared
    assert flatten("a  b\n\nc\f d ") == "a b c d", flatten("a  b\n\nc\f d ")
    assert flatten(None) == "" and flatten("") == ""
    # and no real submission number is written down in this public repository
    for label, path, text, _ in CASES:
        for blob in (os.path.basename(path), text or ""):
            for m in SUBMISSION_NUMBER.finditer(blob):
                assert m.group(0).startswith("XXX-"), (
                    "a real-looking submission number is in the test file: %s" % m.group(0))

    print()
    if bad:
        print("%d case(s) FAILED:" % len(bad))
        for label, want, got in bad:
            print("  %-54s wanted %s, got %r" % (label, "a reason" if want else "None", got))
        return 1
    print("All %d cases pass." % len(CASES))

    if "--archive" in sys.argv:
        import subprocess
        r = subprocess.run([sys.executable, os.path.join(HERE, "screen_inreview.py")])
        if r.returncode != 0:
            print("the archive screen reported something; see above")
            return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
