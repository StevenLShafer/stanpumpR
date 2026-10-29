# Paper digests

One Markdown **evidence card** per paper, product label or regulatory document that stanpumpR's
models are built from — 1,125 of them. Each card records what its source actually says
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

## Reading a card

`digests/README.md` is the authoring guide: the frontmatter contract, the verification
verdicts (`matches`, `discrepancy`, `partly-verified`, `not-applicable`), the `open_questions`
queue and what `revised:` and `reviewed:` mean.

Two things worth knowing before trusting one:

- A card's **Study section and parameter blocks are transcription**; the prose around them is
  interpretation. Three defects found in October 2026 were in the prose, downstream of a
  correct transcription.
- A **comparative word is a claim with a hidden second half.** When a source says a parameter
  is higher or shorter, the card must name what it was measured against. One unexamined
  "higher" carried a 64% error into a plotted curve.

`digests/STATUS.md` lists what still needs a digest; `digests/REVIEW.md` is the queue of open
questions and cards awaiting human review.
