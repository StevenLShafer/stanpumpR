# Paper digests

One Markdown **evidence card** per paper, product label or regulatory document that stanpumpR's
models are built from — over 1,100 of them, and growing. Each card records what its source actually says
(design, population, parameters with the table and page they came from, caveats) and then
verifies the repository against it, location by location.

They live here so that they travel with the code they check. A card's
`checked_against: stanpumpR master <sha>` names the tree it was verified against, and now that
tree is in the same history.

## The PDFs are not here, and will not be

The sources themselves are copyrighted and stay in a private archive
(`stanpumpRPapers`, about 1,120 PDFs and 1.3 GB). **The digests are not copies of them.** They
are facts and paraphrase, written under a rule that allows no quoted passage longer than
15 words and at most one per card — so the card is distributable where the paper is not.
That separation is deliberate, and is the reason these can be public at all.

A card names its source in `file:` and cites pages within it. Without the PDF you cannot
re-check a page reference, which is exactly why the card states the facts rather than
pointing at them.

**The FDA labels are the exception.** A product label is a public document, so the US prescribing
information for the library's drugs is kept in `labels/`, indexed by `inst/extdata/fdaLabels.csv` and
linked from each drug's help page (`labels/README.md`). A label also in the archive keeps its archive
file name there, so its card's `file:` names it in both places.

Papers that work is waiting on but that could not be downloaded are listed in
[`to-obtain.md`](to-obtain.md).

## A paper under peer review never gets here

A manuscript in peer review is held only by its reviewers and editors, and a card written from
one could be published — these cards are public, which is the whole point of the paragraph above.
So such a paper is kept out of the archive rather than digested carefully, and four separate
places refuse it (`_build/inreview.py` decides; Steven L. Shafer, 2026-11-21):

| Where | What it refuses |
|---|---|
| `_build/file_downloads.py`, before the file is touched | filing it, unwrapping it, hashing it |
| the same, before the "needs a look" note | inviting a session to hand-rename it to `PMID_<n>.pdf` |
| the same, at the moment of filing | a manuscript hand-named `PMID_<n>.pdf` |
| `_build/digest_run.py`, before the queue | writing the card, which is the public artefact |

`_build/screen_inreview.py` is the standing audit, run by `update.bat`: it reports anything in the
archive that looks like a manuscript under review and **never deletes** — a hit needs a person to
look at the paper. On the day the rules were written it screened 2,257 archived PDFs clean, while
catching all five of the manuscripts then sitting in Downloads.

The evidence it reads is a journal manuscript number (shaped like a made-up `XXX-D-26-01234`), a folder named for work
under review, or page-1 wording an editorial system stamped on the file — including Editorial
Manager's `Click here to access/download;Figure;…`, which no published paper carries. A **preprint**
is not under review for this purpose: the author chose to make it public. Two rules were tried and
measured out, both being evidence of *publication*: "submitted for publication" (the journal's own
accepted-on footnote, 17 false positives in 400 papers) and MDPI's unreplaced `x FOR PEER REVIEW`
running header, which it leaves in published PDFs.

A false positive costs one paper left in Downloads with a line saying why. A false negative puts a
colleague's unpublished work in a repository and its digest on the internet. The rules are tuned
accordingly, and anything flagged is reported rather than quietly acted on.

## nlmixr2lib never gets here

The model library **nlmixr2lib** (`docs/nlmixr2lib/README.md`) is off limits to the archive, to the
Paper Management session and to the whole pipeline (Steven L. Shafer, 2026-11-28). Its package, model files,
vignettes and any copy or extract of them are never uploaded into `stanpumpRPapers`, filed, queued or
digested, and they never get a card. It is read only by the repository's cross-check,
`data-raw/nlmixr2libCrossCheck.R`, outside the archive. A drug or a reference of interest it leads to
is handled as a new drug or a new manuscript by the usual route: the paper is obtained from its
publisher under its own PMID, filed, and digested like any other.

## Reading a card

`digests/README.md` is the authoring guide: the frontmatter contract, the verification
verdicts (`matches`, `discrepancy`, `partly-verified`, `not-applicable`), the `open_questions`
queue and what `revised:` and `reviewed:` mean.

Three things worth knowing before trusting one:

- A card's **Study section and parameter blocks are transcription**; the prose around them is
  interpretation. Three defects found in October 2026 were in the prose, downstream of a
  correct transcription.
- A **comparative word is a claim with a hidden second half.** When a source says a parameter
  is higher or shorter, the card must name what it was measured against. One unexamined
  "higher" carried a 64% error into a plotted curve.
- **A two-stage paper's summary parameters may describe no subject at all.** Where a source
  fitted each subject and then averaged the parameters, say so in the card and do not let the
  means pass as a model. Averaging is worst for whichever parameter is most skewed across
  subjects, usually a rate constant or half-life, and the others can look healthy while that
  one is badly wrong. de Groot 1994's printed means give a central volume of 32.60 L where a
  refit of the same six men gives 18.32 -- and the card that first described it said the model
  could not be closed at all, which was wrong twice over. The procedure for recovering such a
  paper is *A two-stage source* in `docs/adding-a-drug.md`.

## How a card gets here

Cards are written by `_build/digest_run.py` in the private archive, which runs Claude headless
against one PDF at a time. It does not write into this directory in a checkout somebody happens
to have open: it keeps a git worktree of this repository pinned to `master`, writes the run's
cards into that worktree's `papers/digests/`, and pushes them on a branch of their own
(`claude/digests-<timestamp>`), so a run arrives as a pull request. `_build/cards.py` is the one
place that says where that worktree is.

That indirection was bought with three hand reconciliations in a single day. For a few hours
after the move the archive kept its own copy of the cards and the scripts still wrote to it, so
every revision landed in one place and not the other — including one that diverged *while* the
pull request fixing the previous divergence was open. A generated copy and a tracked copy of the
same file will drift; only one of them can be the original.

**Two generated indexes are deliberately not here**, because they describe the archive's PDFs
rather than this repository: `STATUS.md`, which lists the PDFs still needing a card, and
`REVIEW.md`, the queue of open questions and cards awaiting human review. They are rebuilt from
the cards and the archive by `_build/digest_prep.py` and live in the archive beside the PDFs.
Nothing is lost by their absence: every open question is in its own card's `open_questions:`
frontmatter, which is here and is the authority.
