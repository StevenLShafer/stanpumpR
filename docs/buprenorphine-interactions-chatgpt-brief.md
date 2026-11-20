# Self-contained brief: the mathematics of a buprenorphine opioid-receptor interaction model

*This is a standalone problem statement for an external model (e.g. ChatGPT) with no access to the
source repository. Everything needed is below. Please derive the results independently; the work is
being cross-checked against another solution.*

## Background

A pharmacokinetic/pharmacodynamic simulator for anaesthesia and pain medicine needs a model of how
buprenorphine interacts with other opioids at the μ-opioid receptor. Buprenorphine is a high-affinity
μ **partial agonist** whose defining property is **very slow dissociation** from the receptor
(dissociation half-life ≈ 37 min). The clinical difficulty — durable blockade of other opioids,
precipitated withdrawal, a respiratory-depression ceiling, and the problem of providing analgesia to a
maintenance patient who needs a full agonist — is driven by that slow dissociation, **not** by
affinity. (Affinity alone cannot explain it: a full agonist such as sufentanil binds with comparable
or higher affinity yet is readily displaced, because its dissociation is fast. Displaceability is set
by the dissociation rate constant / residence time, which an equilibrium model cancels out.)

The simulator already produces, for every opioid, a time course of **effect-site (biophase)
concentration** `Ce_i(t)`. The interaction model is a **pharmacodynamic post-process** on those
curves: it must not change the pharmacokinetics, and it must reduce to the simulator's existing
additive opioid scale when no partial agonist or antagonist is present.

Scope: **μ receptor only.** Ligand set: buprenorphine; the full agonists fentanyl and sufentanil; the
antagonists naloxone, naltrexone, nalmefene. κ/δ effects, the ORL-1/NOP receptor (which causes a
bell-shaped high-dose antinociception curve, absent in human ventilation data), and the metabolite
norbuprenorphine are out of scope.

## The model (fixed; this is the published structure)

Index the μ ligands by *i*. Each has a simulated effect-site concentration `Ce_i(t)` and receptor
association/dissociation rate constants `k_on,i`, `k_off,i` (so `K_D,i = k_off,i / k_on,i`). Normalise
the receptor pool to 1; let `ρ_i` be the fraction of receptors bound by ligand *i* and
`ρ_0 = 1 − Σ_j ρ_j` the free fraction. The binding kinetics (Olofsen et al. 2022; Yassen et al.
2006/2007):

```
dρ_i/dt = k_on,i · Ce_i(t) · ρ_0(t) − k_off,i · ρ_i(t),      ρ_0 = 1 − Σ_j ρ_j.        (1)
```

Ligands split by residence time `τ = 1/k_off`:

- **Slow** ligands — buprenorphine (`k_off ≈ 0.019 min⁻¹`) and the long-residence antagonists
  naltrexone, nalmefene — each keep a differential state (1).
- **Fast** ligands — fentanyl, sufentanil, naloxone — equilibrate quickly and are reduced to their
  **conditional equilibrium** within whatever pool the slow ligands leave free (singular
  perturbation; this is exactly what Olofsen do for fentanyl):

```
ρ_j = (1 − Σ_{slow} ρ) · φ_j / (1 + Σ_k φ_k),     φ_j = Ce_j / C50_j,     C50_j = k_off,j / k_on,j.   (2)
```

Define the **crowding factor** `Φ(t) = Σ_{fast k} φ_k(t) = Σ_k Ce_k/C50_k`, so the total fast-bound
fraction is `(1 − Σ_slow ρ)·Φ/(1+Φ)` and the free fraction is `ρ_0 = (1 − Σ_slow ρ)/(1+Φ)`.

**Effect (ventilation), linear transduction (Olofsen):**

```
E = E0 · (1 − Σ_i α_i ρ_i),                                                              (3)
```

where `α_i` combines intrinsic activity and receptor reserve: `α = 1` for a full agonist (no ceiling),
`α < 1` for buprenorphine (the ceiling), `α = 0` for a pure antagonist.

**Between simulation grid points, each `Ce_i(t)` is piecewise-linear in t** (the simulator samples on a
dense but non-uniform grid). The model should advance occupancy exactly across one such interval given
the endpoint concentrations.

## Fitted parameters (human, from Olofsen et al. 2022, ventilation; two populations)

| Parameter | Opioid-naive | Chronic high-dose opioid use |
|---|---|---|
| buprenorphine k_on,B | 0.26 | 0.085  (mL·ng⁻¹·min⁻¹) |
| buprenorphine k_off,B | 0.019 | 0.019  (min⁻¹; t½ ≈ 37 min, population-independent) |
| buprenorphine α_B (activity + reserve) | 0.81 | 0.48 |
| fentanyl C50,F (= k_off,F/k_on,F) | 0.68 | 2.23  (ng/mL) |
| fentanyl α_F | 1 (fixed) | 1 (fixed) |
| baseline ventilation E0 (= VB) | 20.2 L/min | 20.2 L/min |

Each ligand also has an effect-site equilibration rate `ke0` that produces its `Ce_i(t)` from plasma;
for this problem the `Ce_i(t)` are given inputs, so `ke0` does not enter the occupancy mathematics.
An independent occupancy dataset (Nasser et al. 2014) gives buprenorphine whole-brain μ occupancy
`= 91.4·C/(0.67 + C)` percent vs plasma concentration, a check on `K_D,B`.

## The problems to solve

**P1 — Exact advance for the single-slow-ligand case.** With buprenorphine the only slow ligand and
all agonists/antagonists fast, substitute (2) into (1) to show the whole system collapses to a single
scalar ODE for `x(t) = ρ_bup`:

```
dx/dt = a(t)·(1 − x) − k_off,B · x,      a(t) = k_on,B · B(t) / (1 + Φ(t)),
```

where `B(t)` is buprenorphine's effect-site concentration and `Φ(t)` the crowding factor from the fast
ligands. This is linear in x with a time-varying coefficient. Over one grid interval `[0, h]` where
`B(t)` and every `Ce_j(t)` are linear in t (so `Φ(t)` and hence `a(t)` are ratios of linear functions
of t), derive the **exact closed-form advance** `x(h) = f(x(0), …)` via the integrating factor
`exp(∫₀ᵗ (a(s)+k_off) ds)`. Give:
- the closed form (the integrals of `a(t)` and of the integrating-factor-weighted source are
  elementary — logarithms and an incomplete-integral / special-function term; state them explicitly);
- a numerically stable ("overflow-proof") evaluation suitable for implementation;
- all degenerate limits (constant `Ce`; `a ≡ 0`; vanishing crowding; coincident denominators);
- any conditioning hazard (e.g. large `(a+k_off)·h`) and how to bound or sub-step it;
- a validation against an independent high-accuracy integration.

**P2 — Two or more slow ligands.** With buprenorphine plus a long-residence antagonist, there is no
scalar reduction. Give the accurate advance for the coupled small nonlinear system (state whether any
closed form survives; otherwise a well-conditioned exponential/numeric step), keeping all occupancies
in the physical simplex `ρ_i ≥ 0, Σρ_i ≤ 1`.

**P3 — Operational transduction and the analgesia-vs-ventilation asymmetry.** Replace (3) with an
operational model of agonism (Black & Leff 1983): give each ligand an efficacy `τ_i` and each endpoint
a receptor reserve, with effect `E/E_max = (Σ_i τ_i ρ_i)^n / (1 + (Σ_i τ_i ρ_i)^n)`. Show (a) that
Olofsen's linear form (3) is the low-reserve limit, and (b) that a single receptor model with
different reserve per endpoint reproduces buprenorphine's clinical asymmetry — a near-full agonist for
**analgesia**, a ceiling-limited partial agonist for **ventilation**. Map Olofsen's fitted `α` onto
`(τ, reserve)` so the ventilation fit is preserved.

**P4 — Backward compatibility.** The simulator's existing additive opioid scale is
`U = Σ_j Ce_j/MEAC_j` (MEAC = minimum effective analgesic concentration). Calibrating each fast
agonist so `C50_j = MEAC_j`, prove the exact identity (no slow ligand present) relating `Σ_j ρ_j` and
the fast-bound effect to `U`, and bound the deviation when a slow ligand is present, so that the model
leaves the existing additive panels unchanged until buprenorphine or an antagonist appears.

**P5 — Tolerance as a one-parameter manifold.** Olofsen gives two tolerance states (naive, chronic)
differing in `(k_on,B, α_B, C50,F)` but not `k_off,B`. The application exposes four tolerance levels.
Define a defensible interpolation of the triple across a single tolerance coordinate `θ ∈ [0,1]`,
anchored to reproduce Olofsen's two fits at `θ = 0, 1`, with `k_off,B` held fixed.

## Deliverable

For each of P1–P5: the derivation, the explicit formulas, the numerical safeguards, and a validation
statement (what it was checked against and the achieved accuracy). Treat the closed-form exactness of
P1 and the backward-compatibility identity of P4 as the two load-bearing results.
