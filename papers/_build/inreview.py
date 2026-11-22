"""Recognise a manuscript that is under peer review, so it is never filed and never digested.

Steven L. Shafer, 2026-11-21: "papers that are in peer review shouldn't appear anywhere in
their pre-approved form, unless the author uploads the paper to a preprint server. While the
papers themselves in the Paper Repository will not be public, there is a risk that a digest
prepared from a paper in peer review might be public. That is a risk that must be guarded
against."

The exposure is real and was live when this was written: five manuscripts sat in Downloads,
one of them a tracked-changes manuscript, and file_downloads.py spared them
only BY ACCIDENT -- it could not match them to a stanpumpR reference, so it left them alone.
Hand-name one PMID_<n>.pdf, or let it through under a rule that files everything, and it goes
straight into the archive; under "everything needs a digest" a card follows it into a public
repository. The protection has to be deliberate.

WHAT THIS IS NOT.  It is not a classifier for "unpublished". A preprint the author posted is
public by the author's own choice and is fine. An accepted manuscript (the _aam files
unpaywall_fetch.py files) is the author's accepted version of a PUBLISHED paper, also fine.
What is guarded is the version in review, which only its reviewers and editors hold.

HOW IT DECIDES, and why it is deliberately crude: a false positive costs one paper left in
Downloads with a line saying why, which a person undoes in a second. A false negative puts a
colleague's unpublished manuscript in a repository and possibly its digest on the internet.
So anything that looks like a submission is refused, and the reasons are reported rather than
silently applied.

  in_review_reason(path, name=None, text=None) -> str or None

`text` is the first page or two if the caller already has it (file_downloads.py does); when it
is None only the name and folder are judged, which is enough for a submission number.
"""
import os
import re

# A journal's manuscript number, the strongest signal there is: these exist only inside an
# editorial system. The shape is <JOURNAL>-D-<yy>-<number>, optionally _R<n>, as in a made-up
# XXX-D-26-01234_R1. Five real ones were in Downloads on 2026-11-21 and are deliberately NOT
# written down here: a submission number names a specific manuscript at a specific journal,
# and this repository is public, so recording one would leak the very fact this file exists to
# protect. Test against the files themselves, never against a number pasted into a comment.
SUBMISSION_NUMBER = re.compile(r"(?<![A-Za-z0-9])[A-Z]{2,8}-[A-Z]-\d{2}-\d{3,6}(?:_R\d+)?(?!\d)")

# Folders that exist to hold work under review. A path is judged as well as a file name: the
# five live examples were all in "Single Paper Review\upload parts (size-limited copies)".
REVIEW_FOLDERS = ("single paper review", "peer review", "under review", "to review",
                  "manuscripts in review", "reviews")

# Page-1 wording. Each of these is put on a manuscript BY an editorial system or by its author
# while it is in review. "Author Manuscript" is deliberately absent: PubMed Central stamps that
# on accepted manuscripts of published papers, which are fine, and the archive holds many.
REVIEW_TEXT = (
    # Editorial Manager (Aries) writes this into the PDF it builds from a submission, one
    # per attached item: "Click here to access/download;Figure;Figure S1.pdf". It is the
    # clickable attachment link of the submission itself, so no published paper carries it,
    # and it is the ONLY wording on page 1 of the two live supplements of 2026-11-21 --
    # both of which name and folder had caught, but which text alone had missed.
    "click here to access/download",
    "tracked changes manuscript",
    "for peer review",
    "for review only",
    "confidential - for review",
    "confidential: for review",
    "manuscript draft",
    "manuscript under review",
    "reviewer copy",
    "anonymised manuscript",
    "anonymized manuscript",
    "blinded manuscript",
)

# Phrases that LOOK like review wording on a published paper, measured against 2,257 archived
# papers rather than guessed. Each is checked before REVIEW_TEXT and clears the file:
#
#   "submitted for publication 5 April 2017; accepted 30 October 2017" -- the journal's own
#   received/accepted footnote, on page 1 of a published paper. 17 of the first 400 sampled.
#   The phrase alone was the single worst rule tried: it is evidence of publication.
#
#   "Pharmaceuticals 2022, 14, x FOR PEER REVIEW" -- MDPI's manuscript template leaves this
#   running header in the PUBLISHED PDF, placeholder "x" and all. ScholarOne's genuine stamp
#   says "For Peer Review" without that placeholder, so the placeholder is what separates them.
#
# "do not distribute" and "not for distribution" were dropped outright: publishers print them
# on reprints of published papers, so they say nothing about peer review.
PUBLISHED_FORMS = (
    re.compile(r"submitted for publication[^.;]{0,40}(;|\.)?\s*accept", re.I),
    re.compile(r"\bx\s+for peer review", re.I),
)

# Said by a preprint server, which means the author chose to make it public: that is NOT in
# review for this purpose, and must beat the wording above (a preprint can say "manuscript
# draft" on its face).
PREPRINT = ("medrxiv", "biorxiv", "arxiv", "research square", "ssrn", "preprints.org",
            "is a preprint", "preprint server", "doi.org/10.1101/")


def in_review_reason(path, name=None, text=None):
    """Why this looks like a manuscript under peer review, or None.

    The returned string is shown to a person and recorded, so it names the evidence.
    """
    name = name or os.path.basename(path)
    folder = os.path.dirname(os.path.abspath(path)).lower()
    low = (text or "").lower()

    # The author put it on a preprint server: public by their own choice.
    if any(p in low for p in PREPRINT):
        return None

    m = SUBMISSION_NUMBER.search(name)
    if m:
        return "its name carries a journal manuscript number, %s" % m.group(0)

    for f in REVIEW_FOLDERS:
        if f in folder:
            return "it sits in a folder named for work under review (%s)" % f

    if text:
        if any(r.search(text) for r in PUBLISHED_FORMS):
            return None
        for phrase in REVIEW_TEXT:
            if phrase in low:
                m2 = SUBMISSION_NUMBER.search(text)
                extra = (", and the text carries the manuscript number %s" % m2.group(0)) if m2 else ""
                return "page 1 says \"%s\"%s" % (phrase, extra)
        m2 = SUBMISSION_NUMBER.search(text)
        if m2:
            return "page 1 carries a journal manuscript number, %s" % m2.group(0)

    return None
