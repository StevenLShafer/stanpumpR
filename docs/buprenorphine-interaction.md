# The mathematics of a buprenorphine μ-receptor interaction model

Status: **proposal, not implemented.** Drafted 2026-11-19 at the request of
Steven L. Shafer. Nothing here changes the app.

Because the mathematics is heavy, the full design note is typeset:

- **[buprenorphine-interaction.pdf](buprenorphine-interaction.pdf)** — the document to read.
- **[buprenorphine-interaction.tex](buprenorphine-interaction.tex)** — the source of truth.
  Rebuild with `pdflatex buprenorphine-interaction.tex` (run twice for
  cross-references). It needs only a minimal TeX Live
  (`texlive-latex-base`, `-recommended`, `-extra`, `-fonts-recommended`).

The heavy math lives only in the `.tex`/PDF so the two cannot drift; this file
is a pointer, not a second copy.

## What it covers

A kinetic (non-equilibrium) μ-receptor occupancy model for buprenorphine
interacting with full agonists (fentanyl, sufentanil) and antagonists
(naloxone, naltrexone, nalmefene), as a **pharmacodynamic post-process** on the
effect-site curves `Ce_i(t)` the simulator already produces — it does not touch
the PK, and reduces to the existing additive opioid scale
`U = Σ Ce_j/MEAC_j` when no partial agonist or antagonist is present. The
defining premise: buprenorphine's blockade, precipitated withdrawal and
ventilatory ceiling are set by its **slow dissociation** (`k_off`), not its
affinity, which an equilibrium model cancels out.

It works through the five problems of the external brief it assesses (P1 exact
single-slow-ligand advance; P2 several slow ligands; P3 operational transduction
and the analgesia/ventilation asymmetry; P4 backward compatibility; P5 tolerance
interpolation), and flags four places where it **departs from or strengthens**
that brief:

1. **P2 is linear, indeed compartmental (Metzler), not nonlinear** — so a
   matrix-exponential / Magnus step is exact-to-rounding and simplex-preserving
   by construction, the same device as `advanceTransitChain()` and
   `advanceClosedFormGas()`.
2. **P4 has an exact invariant, not merely a bounded deviation** — the legacy
   scale `U` equals the fast-ligand occupancy odds `Σρ_fast / ρ_0` exactly,
   whether or not buprenorphine is present.
3. **P1's closed form is exact but not automatically best-conditioned** — the
   incomplete-gamma evaluation competes with a short bounded-kernel quadrature;
   the kernel is written overflow-proof either way.
4. **P3's reduction to Olofsen's linear transduction holds only when the
   transduction Hill coefficient `n = 1`.**

It also records that the Nasser 2014 occupancy curve is an order-of-magnitude
sanity check on `K_D,B`, not a tight one, and ends with where an engine would
sit in the pipeline plus three **[decision]** points left for Shafer.

> The note is an assessment of an external problem brief; it is not built from an
> archived paper, and cites published sources (Olofsen 2022, Yassen 2006/2007,
> Black & Leff 1983, Nasser 2014) only by name, as the brief does.
