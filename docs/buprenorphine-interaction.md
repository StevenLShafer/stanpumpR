# Buprenorphine μ-receptor interaction — the mathematics (typeset)

The full, typeset mathematics of the buprenorphine opioid-receptor interaction
model — for a reader who wants the details — is the LaTeX source
[`buprenorphine-interaction.tex`](buprenorphine-interaction.tex) (compile with
`pdflatex buprenorphine-interaction.tex`, run twice for references), possibly for
future publication.

This is a single, authoritative copy of the heavy math, so it cannot drift. The
same content, in Markdown and kept in step with the engine, is
[`buprenorphine-interactions-math.md`](buprenorphine-interactions-math.md); the
design specification is
[`buprenorphine-interactions.md`](buprenorphine-interactions.md); and the engine
it describes is `R/opioidReceptorInteraction.R`.

It works the five problems P1–P5 (the exact single-slow-ligand advance; several
slow ligands; the operational analgesia/ventilation transduction; backward
compatibility; tolerance), with four `[Correction]` callouts strengthening the
original brief and Steven L. Shafer's three design decisions of 2026-11-19 as
`[Decision]` callouts.
