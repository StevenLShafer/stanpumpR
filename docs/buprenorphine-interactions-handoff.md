# Handoff: the mathematics of buprenorphine's opioid-receptor interaction

**For:** a fresh modelling session (Opus 5.5 or Fable 5.2) to take the mathematics deeper.
**From:** the buprenorphine-interaction design session, 2026-11-19.
**Read first:** `docs/buprenorphine-interactions.md` — the full design spec. This note is the
short brief and the list of open mathematical problems; the spec is the reference.

> This is a self-contained brief. It names only buprenorphine, the full agonists fentanyl and
> sufentanil, and the antagonists naloxone, naltrexone and nalmefene. You need no other opioid to
> do this work; please do not widen the ligand set beyond what a problem below requires.

## The clinical problem (why this matters, and why it is hard)

Buprenorphine is a high-affinity μ-opioid **partial agonist** with **very slow receptor
dissociation** (k_off half-life ~37 min). The slow dissociation — not the affinity — is what makes
it clinically difficult, in three settings:

1. **Pain management** with buprenorphine: a ceiling on respiratory depression (safety), but also a
   ceiling and competition that complicate adding other analgesia.
2. **Opioid use disorder** maintenance: high, durable receptor occupancy that *blocks* subsequently
   taken full agonists — the therapeutic point.
3. **Surgery on a maintenance patient**: the hard case. A patient on maintenance buprenorphine needs
   a full-agonist analgesic that must *compete with buprenorphine at the receptor*. How much is
   needed, whether it works at all, and the risk of precipitated withdrawal if buprenorphine is
   introduced on top of a full agonist, are exactly what the model must predict.

The mathematical difficulty mirrors the clinical difficulty, and it is concentrated in the **binding
kinetics**, because standard competitive-occupancy equations (which use only the equilibrium
dissociation constant K_D = k_off/k_on) cancel k_off and so cannot represent slow dissociation at
all. Sufentanil makes the point: it binds *more tightly* than buprenorphine (lower K_D) yet is
readily displaced, because its k_off is fast. Displaceability is a kinetic property (residence time
1/k_off), not an affinity one.

## What is already settled (do not re-derive)

The model structure is fixed and literature-grounded. It is the receptor association/dissociation
kinetics fitted to human data by the Leiden group:

- **Olofsen, Algera, Dahan & Laffont, JCI Insight 2022;7(9):e156973** — buprenorphine + fentanyl on
  ventilation, two populations (opioid-naive and chronic opioid use). CC-BY 4.0. This is the engine.
- **Yassen 2007 (Clin Pharmacokinet 46:965)** — buprenorphine + naloxone on ventilation (the
  antagonist case). Same engine, α = 0 partner.
- **Yassen 2006 (Anesthesiology 104:1232)** — buprenorphine antinociception (the analgesia endpoint;
  faster k_off ~9 min there).
- **Nasser 2014 (Clin Pharmacokinet 53:813)** — buprenorphine receptor occupancy vs plasma
  concentration, an independent check on K_D.

The engine, normalising the receptor pool to 1 (ρ_i = fraction bound by ligand i, ρ_0 free):

```
Each ligand:         dρ_i/dt = k_on,i · Ce_i(t) · ρ_0  −  k_off,i · ρ_i,     ρ_0 = 1 − Σ_j ρ_j
Fast ligands (QSSA): a fast-k_off agonist collapses to its conditional equilibrium —
                     ρ_j = (1 − Σ_slow ρ) · φ_j / (1 + Σ_k φ_k),   φ_j = Ce_j / C50_j
Effect (ventilation): E = E0 · (1 − Σ_i α_i ρ_i)       [Olofsen linear transduction]
```

`α_i` combines intrinsic activity and receptor reserve: ≈1 for a full agonist, <1 for buprenorphine
(the ceiling), 0 for a pure antagonist. Only buprenorphine (and the long-residence antagonists) are
"slow" and carry the ODE; the full agonists and naloxone are "fast" and are algebraic. Fitted values
for buprenorphine and fentanyl, including the opioid-naive vs chronic-use split, are in the spec, §3.

The implementation target in stanpumpR: a cheap post-process along the already-simulated effect-site
curves `Ce_i(t)` (the same pattern as the `R/pdAdaptation.R` tolerance panel), *not* a new PK engine.
The model must reduce to the app's existing additive scale `U = Σ Ce_i/MEAC_i` when no partial
agonist/antagonist is present, so nothing in the current panels moves until buprenorphine or an
antagonist is in play.

## Open mathematical problems (the actual work)

### P1 — Exact closed-form advance for the reduced binding ODE

With the fast ligands eliminated by QSSA, the whole system collapses to a **single scalar linear ODE**
for buprenorphine occupancy x(t) = ρ_bup:

```
dx/dt = a(t)·(1 − x) − k_off·x,        a(t) = k_on,B · B(t) / (1 + Σ_j C_j(t)/C50_j)
```

where B(t) is buprenorphine's effect-site concentration and C_j(t) the full agonists'. It is linear
in x with a time-varying coefficient a(t). stanpumpR prizes **exact closed-form advances between grid
points** (see the closed-form 3-compartment engines and the exact transit-chain engine). Over an
interval where B and C_j are piecewise-linear in t, a(t) is a ratio of linear functions of t, so the
integrating factor exp(∫(a+k_off)dt) involves logarithms and the particular solution an
exponential-integral-type quadrature.

- Derive the exact (or semi-analytic to machine precision) update
  `x(t_{n+1}) = f(x(t_n), B_n, B_{n+1}, {C_{j,n}}, {C_{j,n+1}})` for the piecewise-linear interval.
- Handle the degenerate limits safely: constant Ce (a const → simple exponential relaxation),
  a(t) → 0 (no buprenorphine present), and C50 denominators near their reference.
- If the closed form is ill-conditioned for some regime, characterise the error of an embedded
  Dormand-Prince / RK step instead and set a tolerance, as the Michaelis-Menten engine does.
  Either way, prove the result against an independent high-accuracy integration.

### P2 — Two or more slow ligands

Buprenorphine plus a long-residence **antagonist** (naltrexone, nalmefene) gives two slow states,
coupled through `ρ_0 = 1 − Σ_slow ρ − Σ_fast ρ`. The scalar reduction of P1 no longer applies; it
becomes a small coupled nonlinear ODE system. Give the accurate advance for the 2-slow-ligand case
(closed form if one survives — it probably does not — else the conditioned numeric step), and state
how it generalises.

### P3 — Endpoint-specific transduction and receptor reserve

Olofsen's `α` is endpoint-specific and absorbs receptor reserve (their chronic-use buprenorphine α on
ventilation is <1; an analgesia endpoint has larger reserve, so an effective α can exceed 1). The
**operational model of agonism** (Black & Leff 1983) is the principled carrier: effect
`E/E_max = (Σ_i τ_i ρ_i)^n / (1 + (Σ_i τ_i ρ_i)^n)` with per-endpoint efficacy τ_i.

- Define the minimal operational parameterisation that (a) reproduces Olofsen's *linear* ventilation
  transduction in its fitted regime, and (b) reproduces buprenorphine's clinical asymmetry — a
  near-full agonist for analgesia, a ceiling-limited partial agonist for ventilation — as a
  difference in reserve between the two endpoints.
- Map Olofsen's fitted (α, α) onto (τ, reserve) so the ventilation fit is preserved exactly.

### P4 — Backward-compatibility bound

With the full agonists calibrated so `C50_j ∝ MEAC_j`, prove and measure the bound on
`|Σ_j ρ_j − U| / U` at clinical occupancies when no slow ligand is present (U = Σ Ce_j/MEAC_j). This
is the acceptance criterion that keeps every existing opioid panel unchanged until buprenorphine or an
antagonist appears.

### P5 — Tolerance as a parameter manifold

Olofsen gives two points on the tolerance axis (opioid-naive, chronic high-dose use), differing in
(k_on,B, α_B, C50_full) but **not** k_off,B. The app's Patient Profile has four tolerance levels.
Define a defensible interpolation of the triple across levels (e.g. log-linear in a single tolerance
coordinate anchored at Olofsen's two points), and confirm k_off,B is held fixed (residence time is a
property of the bond, not the patient).

## Scope boundary (Lutfy & Cowan 2004, Curr Neuropharmacol 2:395)

μ receptor only. The ceiling is in (α < 1). Deliberately **out of scope**: the bell-shaped
(biphasic) dose–response at very high dose (an ORL-1/NOP effect, not μ; and absent in human
ventilation per Olofsen), κ/δ antagonism, and the active metabolite norbuprenorphine (not yet a PK
species in the library). Keep the model μ-only; record the rest as future work.

## Deliverable

A revision of `docs/buprenorphine-interactions.md` (or a companion note) with P1–P5 worked, plus a
concrete implementation plan against stanpumpR's architecture (the `pdAdaptation`-style post-process,
the per-drug receptor-constant registry, the panels). Then the drug-by-drug wiring follows
`docs/adding-a-drug.md`. Validate against Olofsen 2022, Yassen 2007 and Nasser 2014 as in the spec's
§6.3.
