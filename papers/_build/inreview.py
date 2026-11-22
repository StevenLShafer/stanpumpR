"""Recognise a manuscript that is under peer review, so it is never filed and never digested.

Steven L. Shafer, 2026-11-21: "papers that are in peer review shouldn't appear anywhere in
their pre-approved form, unless the author uploads the paper to a preprint server. While the
papers themselves in the Paper Repository will not be public, there is a risk that a digest
prepared from a paper in peer review might be public. That is a risk that must be guarded
against."

The exposure was real and live when this was written: five manuscripts sat in Downloads, one of
them a tracked-changes manuscript, and file_downloads.py spared them only BY ACCIDENT -- it
could not match them to a stanpumpR reference, so it left them alone. One was in fact reported
as "rename it PMID_<pmid>.pdf by hand". Their submission numbers are deliberately not recorded
anywhere here: a number names a specific manuscript at a specific journal, and the stanpumpR
repository is public, so writing one down would leak the very thing this file protects. Test
against the files themselves, never against a number pasted into a comment.

WHAT THIS IS NOT.  It is not a classifier for "unpublished". A preprint the author posted is
public by that choice. An accepted manuscript (the _aam files unpaywall_fetch.py files) is the
author's version of a PUBLISHED paper. Both are fine. What is guarded is the version in
review, which only its reviewers and editors hold.

STRONG AND SOFT EVIDENCE, and why the distinction exists (CodeRabbit, PR #846).  The first
version had one list and two blanket escapes, and both escapes were too powerful:

  * the preprint allowlist searched the WHOLE text, so a manuscript under review whose
    reference list cites a medRxiv preprint cleared itself.  That is a false negative in the
    only direction that matters.
  * one published-form match returned None for the file, so an MDPI running header would have
    cleared a document that ALSO said "tracked changes manuscript".

So evidence is now weighed, not short-circuited.  STRONG evidence is a mark only an editorial
system or a copy made for a reviewer carries; nothing clears it.  SOFT evidence is suggestive
but has innocent explanations, and a preprint banner at the TOP of the document clears it.  A
published form explains away ONE phrase, never the file.

A false positive costs one paper left in Downloads with a line saying why, which a person
undoes in seconds.  A false negative puts a colleague's unpublished manuscript in a repository
and possibly its digest on the internet.  The weighting is set accordingly.

  in_review_reason(path, name=None, text=None) -> str or None

`text` is page 1 if the caller has it.  With no text only the name and folder are judged, and
a caller that cannot read the text must FAIL CLOSED rather than treat that as a clean bill:
see file_downloads.py and digest_run.py, which both refuse instead.
"""
import io
import os
import re

# A journal's manuscript number: these exist only inside an editorial system.  The shape is
# <JOURNAL>-D-<yy>-<number>, optionally _R<n>, as in a made-up XXX-D-26-01234_R1.
#
# A word-boundary anchor will not do at either end: '_' is a word character, so \b after the
# digits fails on a name of the form XXX-D-26-01234_reviewer_supplement.pdf -- 4 of the 5 live
# manuscripts went uncaught by name that way (measured 2026-11-21; the folder rule was hiding
# it).  Lookarounds on alphanumerics instead, which treat '_' as the separator it is here.
SUBMISSION_NUMBER = re.compile(r"(?<![A-Za-z0-9])[A-Z]{2,8}-[A-Z]-\d{2}-\d{3,6}(?:_R\d+)?(?!\d)")

# STRONG: a mark only an editorial system, or a copy made for a reviewer, carries.  No published
# paper has one, so nothing clears these -- not a preprint banner, not a publisher's header.
STRONG_TEXT = (
    # Editorial Manager (Aries) writes this into the PDF it builds from a submission, one per
    # attached item: "Click here to access/download;Figure;Figure S1.pdf".  It is the clickable
    # attachment link of the submission itself.  It was the ONLY evidence on two of the five
    # live supplements of 2026-11-21, which name and folder caught but text alone had missed.
    "click here to access/download",
    "tracked changes manuscript",
    "blinded manuscript",
    "anonymised manuscript",
    "anonymized manuscript",
    "confidential - for review",
    "confidential: for review",
    "reviewer copy",
)

# SOFT: suggestive, with innocent explanations.  A preprint banner clears these.
SOFT_TEXT = (
    "for peer review",
    "for review only",
    "manuscript draft",
    "manuscript under review",
)

# Folders that exist to hold work under review.  SOFT: a preprint can be saved in one.
REVIEW_FOLDERS = ("single paper review", "peer review", "under review", "to review",
                  "manuscripts in review", "reviews")

# A published paper's own wording that looks like review wording.  Each entry explains away ONE
# phrase and nothing else, so other evidence still stands.
#
#   MDPI's manuscript template leaves "Pharmaceuticals 2022, 14, x FOR PEER REVIEW" in the
#   PUBLISHED PDF, placeholder "x" and all.  ScholarOne's genuine stamp has no such placeholder,
#   so the placeholder is what separates them.  2 of 2,257 archived papers hit this.
PUBLISHED_FORMS = {
    "for peer review": re.compile(r"\bx\s+for peer review", re.I),
}

# Measured out, and deliberately absent: "submitted for publication".  It is the journal's own
# received/accepted footnote ("Submitted for publication 5 April 2017; accepted 30 October
# 2017"), printed on page 1 of a PUBLISHED paper -- 17 false positives in the first 400
# archived papers sampled, the worst rule tried.  "do not distribute" and "not for distribution"
# went the same way: publishers print them on reprints of published papers.

# A preprint server's own banner, which means the author chose to make the paper public.  It
# must appear near the TOP of the document to count: a citation to someone else's medRxiv
# preprint, in a reference list, says nothing about this document (CodeRabbit, PR #846).
# A preprint server's OWN BANNER, which means the author chose to make the paper public.  It
# must be the server's wording, not any mention of a server: a bare "medrxiv" substring cleared
# a document stamped "For Peer Review" that merely cited someone else's medRxiv preprint in its
# introduction (CodeRabbit, PR #846).  A citation reads "Smith J. medRxiv 2024"; a banner reads
# "medRxiv preprint doi: https://doi.org/10.1101/...; this version posted May 2024."
PREPRINT = (
    # medRxiv and bioRxiv stamp both of these across the top of every page.
    re.compile(r"(?:med|bio)rxiv\s+preprint\s+doi", re.I),
    re.compile(r"this version posted", re.I),
    # The sentence medRxiv, bioRxiv and Research Square print on page 1. Their wordings
    # differ -- "is a preprint and has not been certified by peer review" against "is a
    # preprint, a preliminary version of a manuscript that has not completed peer review
    # at a journal" -- so the rule is "is a preprint" and then "peer review" within the
    # same sentence. A citation carries neither half.
    re.compile(r"is a preprint[^.]{0,200}peer\s+review", re.I | re.S),
    # SSRN's footer, on every page of a paper downloaded from SSRN. A CITATION says
    # "Available at SSRN: https://ssrn.com/abstract=..." and does not say this.
    re.compile(r"electronic copy available at", re.I),
    # arXiv's own sideways stamp, which carries a version and a bracketed primary
    # class: "arXiv:2401.12345v1  [q-bio.QM]  23 Jan 2024". A citation is the bare
    # identifier, with neither.
    re.compile(r"arxiv\s*:\s*\d{4}\.\d{4,5}v\d+\s*\[", re.I),
)
PREPRINT_WINDOW = 2500        # characters; a server's banner is on the title page or its head


def flatten(text):
    """Runs of whitespace to single spaces, so a phrase broken across lines still matches.

    pdftotext -layout pads columns with runs of spaces and wraps a header, so a stamp arrives as
    "Reviewer\nCopy" or "For  Peer Review".  Every text test below works on this form; nothing
    compares against the raw extraction (CodeRabbit, PR #846).
    """
    return re.sub(r"\s+", " ", text or "").strip()


def preprint_banner(flat):
    """Does this document announce ITSELF as a preprint, near the top? `flat` is flatten()'s."""
    return any(p.search(flat[:PREPRINT_WINDOW]) for p in PREPRINT)


def extracted_text(path, limit=8000):
    """The pipeline's extracted text for a paper, or "" if there is none to be had.

    "" covers a file that is ABSENT and one that cannot be READ -- indistinguishable here, and
    both mean the document was never inspected.  It lives beside card_refusal() so that the
    composition the digest queue actually performs can be tested on real files, which was
    CodeRabbit's point on PR #846: testing the pure function alone does not show that what feeds
    it behaves.
    """
    try:
        with io.open(path, encoding="utf-8", errors="replace") as f:
            return f.read(limit)
    except OSError:
        return ""


def card_refusal(pdf_path, text):
    """Why this paper must not be given a digest card, or None.

    The card is the PUBLIC artefact, so this is stricter than in_review_reason(): text that is
    missing, unreadable or whitespace-only is a refusal in itself, not an invitation to judge
    the file name instead.  read_text() returns "" both for a file that is absent and for one it
    cannot read, and a manifest's `scan` flag cannot be relied on to have noticed -- digest_prep.py's
    return code is not checked, so a failed preparation can leave an older non-scan entry in use
    (CodeRabbit, PR #846).  The absence of evidence is not evidence of absence.

    It lives HERE, beside the classifier, rather than inline in digest_run.py's queue loop,
    because that loop cannot be imported without running a whole unattended digest run -- so the
    branch could not be tested where it was, and "verified by reading" is not verification.
    """
    if not (text or "").strip():
        return ("no readable extracted text, so it could not be screened for a manuscript under "
                "peer review; digest it by hand")
    why = in_review_reason(pdf_path, text=text)
    if why:
        return "looks like a manuscript under peer review: %s; NOT carded" % why
    return None


def in_review_reason(path, name=None, text=None):
    """Why this looks like a manuscript under peer review, or None.

    The returned string is shown to a person and recorded, so it names the evidence.  A caller
    with no text gets a name-and-folder judgement only, and must say so rather than imply the
    document itself was inspected.
    """
    name = name or os.path.basename(path)
    folder = os.path.dirname(os.path.abspath(path)).lower()
    flat = flatten(text)                 # the ONE form every text test below uses
    low = flat.lower()

    strong, soft = [], []

    m = SUBMISSION_NUMBER.search(name)
    if m:
        strong.append("its name carries a journal manuscript number, %s" % m.group(0))

    for f in REVIEW_FOLDERS:
        if f in folder:
            soft.append("it sits in a folder named for work under review (%s)" % f)
            break

    if text:
        for phrase in STRONG_TEXT:
            if phrase in low:
                strong.append('page 1 says "%s"' % phrase)
        for phrase in SOFT_TEXT:
            if phrase in low:
                explain = PUBLISHED_FORMS.get(phrase)
                if explain:
                    # Strike out the occurrences the published form explains, then look again:
                    # suppressing the phrase wherever it appears cleared a document carrying
                    # BOTH MDPI's running header and a separate stamp of its own.
                    if phrase not in explain.sub(" ", flat).lower():
                        continue
                soft.append('page 1 says "%s"' % phrase)
        m2 = SUBMISSION_NUMBER.search(flat)
        if m2:
            strong.append("page 1 carries a journal manuscript number, %s" % m2.group(0))

    if strong:
        return "; and ".join(strong)          # nothing clears strong evidence

    if soft and not preprint_banner(flat):
        return "; and ".join(soft)

    return None
