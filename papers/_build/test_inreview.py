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
import ast
import io
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from inreview import (in_review_reason, card_refusal, extracted_text,  # noqa: E402
                      paper_card_refusal, flatten, SUBMISSION_NUMBER)

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
     "Research Square\nThis is a preprint, a preliminary version of a manuscript that has "
     "not completed peer review at a journal.\nmanuscript draft", False),
    ("a banner broken across lines and padded still clears", PLAIN,
     "medRxiv preprint doi: https://doi.org/10.1101/2024.05.05;\nthis  version\n"
     "posted  May 2024.\nmanuscript draft", False),
    # A banner never clears STRONG evidence: a reviewer's copy of a preprinted paper is still
    # a reviewer's copy.
    ("a banner does not clear an editorial stamp", PLAIN,
     "medRxiv preprint doi: https://doi.org/10.1101/2024.01.01\n"
     "Click here to access/download;Figure;Figure S1.pdf", True),

    # ---- a CITATION to a preprint is not a banner -----------------------------------------
    # Each of these is how a preprint is cited. Every one cleared a stamped manuscript before
    # the allowlist was narrowed to template wording only.
    ("stamped, citing a bare arXiv identifier", PLAIN,
     "For Peer Review\nIntroduction: Smith et al., arXiv:2401.12345, showed ...", True),
    ("stamped, citing an SSRN abstract URL", PLAIN,
     "For Peer Review\nAvailable at SSRN: https://ssrn.com/abstract=123456", True),
    ("stamped, citing a Research Square preprint", PLAIN,
     "For Peer Review\nJones et al. (Research Square preprint, 2024).", True),
    ("stamped, citing preprints.org", PLAIN,
     "For Peer Review\nSee www.preprints.org/manuscript/202401.0001", True),
    ("stamped, citing a 10.1101 DOI", PLAIN,
     "Manuscript draft\nAs shown in doi.org/10.1101/2024.01.01.573 ...", True),
    ("stamped, with prose about preprint servers", PLAIN,
     "For Peer Review\nWe searched every preprint server for comparable data.", True),

    # ---- the servers' OWN banners, which must still clear ---------------------------------
    ("arXiv's own stamp: version and bracketed class", PLAIN,
     "arXiv:2401.12345v1  [q-bio.QM]  23 Jan 2024\nmanuscript draft", False),
    ("SSRN's own footer", PLAIN,
     "Electronic copy available at: https://ssrn.com/abstract=123456\nmanuscript draft", False),
    ("medRxiv's own stamp, which always carries both halves", PLAIN,
     "medRxiv preprint doi: https://doi.org/10.1101/2024.05.05; this version posted May 2024.\n"
     "manuscript draft", False),
    ("the sentence medRxiv and Research Square both print", PLAIN,
     "This article is a preprint and has not been certified by peer review.\n"
     "manuscript draft", False),

    # A preprint statement must describe THIS document. Without that, a manuscript under
    # review discussing somebody else's preprint cleared its own stamp.
    ("stamped, saying ANOTHER study is a preprint awaiting review", PLAIN,
     "For Peer Review\nThe study by Smith (2024) is a preprint and has not completed "
     "peer review.", True),
    ("stamped, citing a medRxiv doi in reference-list form", PLAIN,
     "For Peer Review\nSmith J. medRxiv preprint doi: 10.1101/2024.01.01", True),
    ("the self-referential form medRxiv prints", PLAIN,
     "This article is a preprint and has not been certified by peer review.\n"
     "manuscript draft", False),

    ("stamped, with \"this study\" pointing at the previous sentence", PLAIN,
     "For Peer Review\nSmith et al. (2024) report an intervention. This study is a "
     "preprint and has not completed peer review.", True),

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


# What digest_run.py asks before it writes a card. Stricter than in_review_reason(), because a
# card is public: text that is missing, unreadable or whitespace-only is a refusal in itself.
# read_text() returns "" for a file that is absent AND for one it cannot read, and a manifest's
# `scan` flag cannot be relied on to have noticed, since digest_prep.py's return code is not
# checked and a failed preparation can leave an older non-scan entry in use.
#
# These exist because CodeRabbit declined, correctly, to accept "verified by reading" for this
# branch -- and it could not be tested where it was, inline in a queue loop that cannot be
# imported without starting a digest run.
CARD_CASES = [
    ("a clean paper is carded", "Propofol pharmacokinetics in 24 volunteers.", False),
    ("missing text file (read_text returns the empty string)", "", True),
    ("a text file it could not read (same empty string)", "", True),
    ("whitespace only", " \n \t ", True),
    ("form feeds only, as pdftotext gives for a page with no text layer", "\f\f", True),
    ("None, defensively", None, True),
    ("a manuscript under review is never carded", "Tracked Changes Manuscript\nMethods", True),
    ("a 12-letter reviewer stamp is never carded", "Reviewer Copy", True),
    ("a genuine preprint IS carded",
     "medRxiv preprint doi: https://doi.org/10.1101/2024.05.05; this version posted "
     "May 2024. Propofol in volunteers.", False),
]


def composition_cases(tmp):
    """paper_card_refusal() over a real archive layout: what the digest queue decides.

    This is the queue's whole text-based decision, executed -- not its call site inspected.
    Each paper below is one the MANIFEST would call a normal, non-scan paper: digest_prep.py's
    return code is unchecked, so a failed preparation can leave a stale non-scan entry on a
    paper whose text is missing, unreadable or blank, and the refusal must not depend on that
    flag (CodeRabbit, PR #846).
    """
    arch = os.path.join(tmp, "archive")
    text = os.path.join(arch, "_build", "text")
    os.makedirs(text)

    rows = []

    io.open(os.path.join(arch, "PMID_1.pdf"), "wb").write(b"%PDF-")
    rows.append(("text file absent entirely", "PMID_1", True))

    os.makedirs(os.path.join(text, "PMID_2.txt"))          # a directory: cannot be read
    rows.append(("text file cannot be read", "PMID_2", True))

    for stem, body, refuse in (("PMID_3", " \n \t ", True),
                               ("PMID_4", "\f\f", True),
                               ("PMID_5", "", True),
                               ("PMID_6", "Propofol pharmacokinetics in 24 volunteers.", False),
                               ("PMID_7", "Tracked Changes Manuscript", True),
                               ("PMID_8", "Reviewer Copy", True),
                               ("PMID_9", "medRxiv preprint doi: https://doi.org/10.1101/"
                                          "2024.05.05; this version posted May 2024.", False)):
        io.open(os.path.join(text, stem + ".txt"), "w", encoding="utf-8").write(body)
        rows.append(("text %r" % (body[:26] or ""), stem, refuse))

    bad = []
    for label, stem, refuse in rows:
        got = paper_card_refusal(stem, arch, text)
        if bool(got) != refuse:
            bad.append(("queue: " + label, refuse, got))
        print("  %s queue: %-44s %s" % ("ok  " if bool(got) == refuse else "FAIL",
                                        label[:44], (got or "carded")[:36]))
    return bad


def queue_wiring():
    """digest_run.py must ask paper_card_refusal() and skip the candidate when it answers.

    The loop itself cannot be executed: importing digest_run.py starts an unattended digest
    run. So this checks the CALL SITE, and only the things a call site can show -- that the
    decision is asked at the top level of the loop that appends to `cand`, that a truthy answer
    skips the candidate WITHOUT negation, and that the append comes after.

    What this cannot show, and what composition_cases() above covers by executing the decision
    instead: that the extracted text reaches the refusal, and what the refusal then decides.
    The earlier version of this function checked only that the pieces were called and in what
    order, which would have passed an inverted condition and a text argument that never
    arrived (CodeRabbit, PR #846) -- the reason the decision now lives in one callable.
    """
    src_dr = io.open(os.path.join(HERE, "digest_run.py"), encoding="utf-8").read()
    tree = ast.parse(src_dr)

    loops = [n for n in ast.walk(tree) if isinstance(n, (ast.For, ast.While))
             and any(isinstance(c, ast.Attribute) and c.attr == "append"
                     and isinstance(c.value, ast.Name) and c.value.id == "cand"
                     for c in ast.walk(n))]
    if len(loops) != 1:
        return ["expected exactly one loop that appends to `cand`, found %d" % len(loops)]
    body = loops[0].body

    asked = appended = None
    name = None
    for i, stmt in enumerate(body):
        for c in ast.walk(stmt):
            if isinstance(c, ast.Call) and getattr(c.func, "id", "") == "paper_card_refusal":
                if asked is None:
                    asked = i
                    if isinstance(stmt, ast.Assign) and isinstance(stmt.targets[0], ast.Name):
                        name = stmt.targets[0].id
            if isinstance(c, ast.Attribute) and c.attr == "append" and \
                    isinstance(c.value, ast.Name) and c.value.id == "cand" and appended is None:
                appended = i

    problems = []
    if asked is None:
        return ["paper_card_refusal() is not called at the top level of the candidate loop"]
    if appended is None:
        return ["the candidate loop does not append to `cand`"]
    if asked >= appended:
        problems.append("paper_card_refusal() is asked after the candidate is queued")
    if name is None:
        problems.append("the refusal is not bound to a name, so it cannot be tested")
        return problems

    # the guard: `if <name>:` followed by a continue. A negated test would skip exactly the
    # papers that must be refused, which the previous check could not tell apart.
    guard = None
    for stmt in body[asked:appended]:
        if isinstance(stmt, ast.If) and isinstance(stmt.test, ast.Name) and stmt.test.id == name:
            if any(isinstance(c, ast.Continue) for c in ast.walk(stmt)):
                guard = stmt
    if guard is None:
        negated = any(isinstance(s, ast.If) and isinstance(s.test, ast.UnaryOp)
                      and isinstance(s.test.op, ast.Not)
                      for s in body[asked:appended])
        problems.append("a truthy refusal does not skip the candidate"
                        + (": the condition is negated" if negated else ""))
    for stmt in body[asked:appended]:
        for c in ast.walk(stmt):
            if isinstance(c, ast.Constant) and c.value == "scan":
                problems.append("the refusal is entangled with the manifest's `scan` flag")
    return problems


def main():
    import tempfile
    wiring = queue_wiring()
    print("  %s queue wiring: paper_card_refusal() is asked, and a refusal skips the candidate"
          % ("ok  " if not wiring else "FAIL"))
    for w in wiring:
        print("       %s" % w)
    bad = [("queue wiring", wiring)] if wiring else []
    with tempfile.TemporaryDirectory() as tmp:
        bad += composition_cases(tmp)
    print()
    for label, text, want_refused in CARD_CASES:
        got = card_refusal(os.path.join("D:", os.sep, "x", "PMID_1.pdf"), text)
        if bool(got) != want_refused:
            bad.append(("card: " + label, want_refused, got))
        print("  %s card: %-46s %s" % ("ok  " if bool(got) == want_refused else "FAIL",
                                       label[:46], (got or "carded")[:40]))
    print()
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
    print("All cases pass: %d classifier, %d card, and the queue composition on real files."
          % (len(CASES), len(CARD_CASES)))

    if "--archive" in sys.argv:
        import subprocess
        r = subprocess.run([sys.executable, os.path.join(HERE, "screen_inreview.py")])
        if r.returncode != 0:
            print("the archive screen reported something; see above")
            return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
