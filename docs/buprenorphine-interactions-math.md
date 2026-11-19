# The mathematics of buprenorphine's opioid-receptor interaction model

**Status:** design mathematics / companion to `docs/buprenorphine-interactions.md`. Nothing here is
implemented yet.
**Author:** Claude (Opus 4.8), 2026-11-19, at the request of Steven L. Shafer.
**Read first:** `docs/buprenorphine-interactions.md` (the design spec) and
`docs/buprenorphine-interactions-handoff.md` (the brief and the five open problems P1–P5). This note
works P1–P5 and gives the implementation plan. Every quantitative claim below was checked against an
independent high-accuracy integration; the validation scripts and their output are summarised in §6.

> **Scope, unchanged from the spec and handoff.** μ receptor only. The ligand set is buprenorphine,
> the full agonists fentanyl and sufentanil, and the antagonists naloxone, naltrexone and nalmefene.
> ORL-1/NOP (the bell-shaped high-dose effect), κ/δ antagonism and norbuprenorphine are out of scope.

---

## 0. The system, in one place

Index the μ ligands by *i*. Each carries a simulated effect-site concentration `Ce_i(t)` and receptor
rate constants `k_on,i`, `k_off,i`. The receptor pool is normalised to 1; `ρ_i` is the fraction bound
by ligand *i*, `ρ_0 = 1 − Σρ_j` the free fraction. The engine (Olofsen 2022 eq. 1–2; Yassen 2006/2007):

```
dρ_i/dt = k_on,i · Ce_i(t) · ρ_0(t) − k_off,i · ρ_i(t),      ρ_0 = 1 − Σ_j ρ_j        (2)
```

Ligands split by residence time `τ = 1/k_off`:

- **slow** ligands — buprenorphine (`k_off ≈ 0.019 min⁻¹`, t½ ≈ 37 min on ventilation), and the
  long-residence antagonists naltrexone and nalmefene — each keep a differential state (2);
- **fast** ligands — fentanyl, sufentanil, naloxone — collapse to their **conditional equilibrium**
  inside whatever pool the slow ligands leave free (Olofsen's singular-perturbation reduction):

```
ρ_j = (1 − Σ_slow ρ) · φ_j / (1 + Σ_k φ_k),        φ_j = Ce_j / C50_j,   C50_j = k_off,j/k_on,j   (4)
```

Write the shared **crowding denominator**

```
D(t) = 1 + Σ_j Ce_j(t)/C50_j      (sum over FAST ligands),   D ≥ 1 always.
```

Then the free pool is `ρ_0 = (1 − Σ_slow ρ)/D`, and the slow-ligand equations become

```
dρ_i/dt = a_i(t)·(1 − Σ_slow ρ) − k_off,i·ρ_i,      a_i(t) = k_on,i · Ce_i(t) / D(t)     (slow)
```

This is the object of P1 (one slow ligand) and P2 (two or more). The transduction from `{ρ_i}` to a
clinical endpoint is P3; backward compatibility with `U = Σ Ce_j/MEAC_j` is P4; the tolerance
dependence of the constants is P5.

---

## P1 — Exact advance for the single-slow-ligand ODE

With buprenorphine the only slow ligand, `x(t) = ρ_bup` obeys a **scalar linear ODE with a
time-varying coefficient**:

```
dx/dt = a(t)·(1 − x) − k_off·x = a(t) − [a(t) + k_off]·x,        a(t) = k_on,B·B(t)/D(t)     (P1.1)
```

Over a grid interval `[t_n, t_{n+1}]` of length `h`, stanpumpR's `Ce_i` are **piecewise-linear** in
`τ = t − t_n ∈ [0,h]`, so with `b = (B_{n+1}−B_n)/h`, `D_n = 1 + Σ C_{j,n}/C50_j`,
`d = (D_{n+1}−D_n)/h`:

```
B(τ) = B_n + b·τ,     D(τ) = D_n + d·τ,     a(τ) = k_on,B·(B_n + b·τ)/(D_n + d·τ)
```

`a(τ)` is a **ratio of two linear functions of τ** — exactly the structure the handoff anticipated.

### P1.1 The exact reduction

Define the full integrating-factor exponent `Φ(τ) = ∫_0^τ [a(s) + k_off] ds = k_off·τ + A(τ)`, with
`A(τ) = ∫_0^τ a(s) ds`. The ODE is linear, so `x(τ) = e^{−Φ(τ)}·(x_n + ∫_0^τ a(s)e^{Φ(s)} ds)`. The
forcing integral collapses because `a = Φ′ − k_off`:

```
∫_0^τ a(s)e^{Φ(s)}ds = ∫_0^τ (Φ′ − k_off)e^{Φ}ds = (e^{Φ(τ)} − 1) − k_off·I(τ),   I(τ)=∫_0^τ e^{Φ(s)}ds
```

Substituting gives the **exact update**:

```
x(τ) = 1 − (1 − x_n)·e^{−Φ(τ)} − k_off·e^{−Φ(τ)}·I(τ)                                      (★)
```

Everything in (★) except `I(τ)` is elementary. `A(τ)` is a closed form: with `β = b/d` and
`G = (B_n·d − b·D_n)/d²`,

```
A(τ) = k_on,B·[ β·τ + G·ln(D(τ)/D_n) ]           (d ≠ 0)                                   (P1.2)
```

so `Φ(τ)` is an affine term plus a logarithm of the (always positive) linear denominator. The whole
difficulty of the interval advance is isolated into **one scalar quadrature** `I(τ)`.

### P1.2 Overflow-proof form (the version to implement)

Because `a ≥ 0` and `k_off > 0`, `Φ` is **monotone increasing**, so fold the decay into the integral:

```
J(τ) := e^{−Φ(τ)}·I(τ) = ∫_0^τ e^{−(Φ(τ)−Φ(s))} ds,       integrand ∈ (0,1]  (exponent ≤ 0)
```

```
x(τ) = 1 − (1 − x_n)·e^{−Φ(τ)} − k_off·J(τ)                                                (★★)
```

Both `e^{−Φ(τ)} ∈ (0,1]` and `J(τ) ∈ (0,h]` are bounded — no overflow, and `x` stays in the physical
range whenever `x_n ∈ [0,1]`. `J` is computed by **Gauss–Legendre on [0,τ]** of the smooth, strictly
positive integrand `e^{−(Φ(τ)−Φ(s))}`; map `s = τ·u`, `u ∈ [0,1]`.

### P1.3 Degenerate limits (all handled, all exact)

- **No buprenorphine, `B ≡ 0` ⟹ a ≡ 0.** `Φ = k_off·τ`, and (★★) gives `x(τ) = x_n·e^{−k_off·τ}` —
  pure dissociation decay at `k_off`. ✔ (verified analytically and numerically to 5e-15).
- **Constant `Ce` (a const).** `Φ = (a+k_off)τ`; (★★) reduces to `x(τ) = x_∞ + (x_n − x_∞)e^{−(a+k_off)τ}`
  with `x_∞ = a/(a+k_off)` — the standard exponential relaxation. ✔ (3.8e-15). Use the closed
  `J = (1 − e^{−(a+k_off)τ})/(a+k_off)` here rather than quadrature.
- **`d → 0` (near-constant crowding).** `β` and `G` carry `1/d` and `1/d²`, but the 1/d terms in
  (P1.2) **cancel exactly** (they must — the limit is finite): `βτ + G·(dτ/D_n) → (B_n/D_n)τ`, and
  `A(τ) → (k_on,B/D_n)(B_nτ + bτ²/2)`, the constant-D quadratic. Implement `A` with a `log1p`/series
  branch for `|d·τ/D_n| < 10⁻⁶`, so the log form is never evaluated in its cancellation regime.
- **C50 denominators near reference.** `D ≥ 1` by construction (`D = 1 + Σ Ce/C50`, `Ce ≥ 0`), so the
  denominator is never near zero; no special handling needed.

### P1.4 The one conditioning hazard, characterised and bounded

The exact reduction (★★) is algebraically exact; the **only** numerical component is `J`. A fixed
Gauss–Legendre rule on `J` is machine-exact *unless the integrand develops a boundary layer*, which
happens when the effective relaxation rate over the interval, `λ·h = (a(τ)+k_off)·h`, is large: then
`e^{−(Φ(τ)−Φ(s))}` is a thin spike of width `~1/λ` at `s = τ`, and a low-order rule over the whole
interval under-resolves it. This is the regime the handoff asked to be characterised. Guard rule:

```
n_sub = ceil(λ_max·h / ρ),   λ_max = k_on,B·max(B_n,B_{n+1})/min(D_n,D_{n+1}) + k_off,   ρ ≈ 2
```

and advance `n_sub` equal sub-intervals with Gauss–Legendre-8. **On stanpumpR's actual grid this is a
non-issue**: `a ≲ k_on,B·B/D ~ O(1) min⁻¹` and grid spacing is `h ~ 1–2 min`, so `λ·h = O(1)` and
`n_sub = 1`. The guard only engages on pathologically long single steps, and it restores machine
precision when it does (§6). For a coarse grid one may instead use the analytic form of `I` in
incomplete-Gamma functions, `I(τ) = D_n^{−m}∫_0^τ(D_n+ds)^m e^{λs}ds` with `m = k_on,B·G`,
`λ = k_off + k_on,B·β` — but that carries its own sign/magnitude conditioning across `sign(d)`,
`sign(λ)`, and non-integer `m`, and is **not recommended** over the sub-stepped quadrature.

### P1.5 Result

(★★) + Gauss–Legendre-8 on `J`, with the `log1p` branch for small `d` and the sub-step guard for
large `λh`, is the exact-to-machine-precision advance in the spirit of stanpumpR's closed-form and
transit-chain engines. It is **not** an adaptive ODE step: the ODE is solved in closed form; only one
smooth bounded scalar integral is quadratured, and its error is controllable to `ε_machine`
independent of step size. This is a stronger guarantee than the Michaelis–Menten engine's adaptive
Dormand–Prince and is the right standard to hold the engine to.

---

## P2 — Two or more slow ligands

Buprenorphine (`x_1`) plus a slow antagonist (`x_2`; naltrexone or nalmefene) gives two differential
states coupled through the shared free pool. **A key simplification the handoff did not assume: after
the fast-ligand QSSA the coupled system is LINEAR in `(x_1, x_2)`**, not nonlinear — the only
nonlinearity (the `ρ_0`-weighted association) became the shared factor `(1 − x_1 − x_2)`:

```
dx_i/dt = a_i(t)·(1 − x_1 − x_2) − k_off,i·x_i,   a_i(t) = k_on,i·Ce_i(t)/D(t),   i = 1,2
```

In vector form, with **x** = (x_1,x_2)ᵀ, **a** = (a_1,a_2)ᵀ, K = diag(k_off,1,k_off,2), 1 = (1,1)ᵀ:

```
d**x**/dt = **a**(t) − M(t)**x**,    M(t) = **a**(t)·1ᵀ + K = [[a_1+k_off,1, a_1],[a_2, a_2+k_off,2]]
```

### P2.1 No scalar closure in general

`M(t)` does **not** commute with itself at different times, because `**a**(t)`'s direction
`a_1:a_2 = k_on,1 Ce_1 : k_on,2 Ce_2` rotates over the interval. So there is no single matrix-
exponential closed form, and (confirming the handoff's guess) **no scalar reduction survives** —
summing the two equations gives `d(x_1+x_2)/dt = (a_1+a_2)(1−Σx) − (k_off,1 x_1 + k_off,2 x_2)`, whose
last term is a function of `σ = x_1+x_2` alone **only if `k_off,1 = k_off,2`**. Generically the two
differ, so the system must be advanced as a coupled pair.

### P2.2 The advance

Because the stiffness (the fast agonists) was already removed by the QSSA, the remaining slow states
are **non-stiff relative to each other** (both `k_off` are `O(0.002–0.02) min⁻¹`), so a modest
exponential integrator is exact to machine precision on one grid interval. Recommended, in order of
preference:

1. **Piecewise-constant-M exponential propagator (variation of constants).** On each (sub-)interval
   freeze `M` and `**a**` at the midpoint and advance `**x** ← e^{−M h}**x** + M⁻¹(I − e^{−M h})**a**`
   (the 2×2 `e^{−Mh}` by its closed-form eigendecomposition). Second order in `h`; sub-step a grid
   interval a few times for machine precision. This is the natural generalisation of P1 and keeps the
   "exact exponential" flavour.
2. **4th-order Magnus**, whose leading term `Ω_1 = −∫_0^h M = −[(∫**a**)1ᵀ + Kh]` is the *closed form*
   `∫**a** = (A_1(h), A_2(h))` from P1.2, with one commutator correction. Highest accuracy per step.
3. **The existing adaptive Dormand–Prince 5(4)** (`mmDormandPrince()`), simply integrating the
   n-state linear system. The path of least new code, fully consistent with the MM engine's standard.

All three generalise to `n` slow ligands as an `n`-state linear system with `M = **a**1ᵀ + K`.

### P2.3 The physical simplex and a safeguard

The structure preserves `Σρ ≤ 1` (at `Σρ = 1`, `d(Σρ)/dt = −Σ k_off,i ρ_i ≤ 0` **while every
`ρ_i ≥ 0`**), and the `ρ_i = 0` faces are inflowing while `Σρ < 1`. So with clinical rate constants
the trajectory stays in the simplex `{ρ_i ≥ 0, Σρ ≤ 1}` — verified over a 240-min bup-maintenance +
antagonist-onset + fentanyl-washout run: occupancies in [0,1], `max Σρ = 0.975` (§6). Only
pathological inputs (e.g. an antagonist with `k_on·Ce/k_off ≫ 1` driving its pseudo-equilibrium
occupancy toward 1 while another ligand is bound) can push a component negative, after which the sum
bound also fails. **Safeguard:** after each advance, clamp each `ρ_i` to `[0,1]` and, if `Σρ > 1`,
renormalise `ρ ← ρ/Σρ` (project onto the simplex). With clinical constants the clamp never binds; it
is cheap insurance for extreme user input, in the same spirit as the MM engine emptying a sub-tolerance
state.

---

## P3 — Operational transduction and the analgesia-vs-ventilation asymmetry

Olofsen 2022 use a **linear** ventilation transduction, `VE = E0·(1 − S)`, `S = Σ_i α_i ρ_i`, with
`α_full = 1` (apnea at infinite agonist), `α_bup < 1` (the ceiling), `α_antagonist = 0`. The
operational model of agonism (Black & Leff 1983) is the principled carrier that (a) contains Olofsen's
linear form, (b) fixes an unphysicality in it, and (c) produces buprenorphine's endpoint asymmetry
from **one receptor model** as a difference in **receptor reserve**.

### P3.1 The operational layer

Give each ligand an endpoint-specific **efficacy** (transducer ratio) `τ_i^(e)` and define a total
**stimulus** `S_e = Σ_i τ_i^(e)·ρ_i`. A hyperbolic (n = 1) transducer maps stimulus to fractional
effect:

```
R_e = S_e / (1 + S_e),        S_e = Σ_i τ_i^(e)·ρ_i                                        (P3.1)
```

(`R_e` = fractional ventilatory depression for ventilation; fractional analgesia for the pain
endpoint; `E = E0·(1 − R_vent)` for ventilation, `E = E0·R_analg` for analgesia.) Higher Hill slope
`n_e` sharpens the transducer; the fit below uses `n = 1` because Olofsen's ventilation data are
described by a transduction with no supra-linear steepness.

### P3.2 Olofsen's linear form is the low-reserve limit — exact to first order

Expand (P3.1) for small stimulus: `R_e = S_e − S_e² + … = Σ τ_i^(e)ρ_i − (Σ τ_i^(e)ρ_i)² + …`. To
first order in occupancy,

```
R_e ≈ Σ_i τ_i^(e)·ρ_i,     i.e.    α_i = τ_i^(e)   ⟺   τ_i^(vent) = α_i                   (P3.2)
```

So **Olofsen's `α_i` are exactly the operational efficacies at the ventilation endpoint**, and the
operational model reduces to Olofsen's linear sum in the zero-reserve (low-stimulus) limit. Setting
`τ_i^(vent) = α_i` reproduces the ventilation fit to first order in occupancy; the second-order
curvature `−S²` is below Olofsen's residual over their fitted occupancy range (their ventilation data
do not reach `ρ_full = 1`, which would need infinite fentanyl — `α_full = 1` is their deliberate linear
*extrapolation*, not a fitted saturation).

**Bonus — a physicality the linear form lacks.** Olofsen's `S = Σα_iρ_i` can exceed 1 (predicting
`VE < 0`) when several agonists are present; the operational `R = S/(1+S) < 1` always, so ventilation
is bounded below by 0. The operational model is therefore a strict, better-behaved generalisation.

Buprenorphine's **ventilation ceiling** falls out: alone at full occupancy (`ρ_bup = 1`), the maximal
depression is `R_vent = τ_B/(1+τ_B)` with `τ_B = τ_B^(vent) = α_B` in the low-reserve calibration, i.e.
the plateau sits near `α_B` (0.81 naive, 0.48 chronic) — breathing continues at `≈ (1 − plateau)·E0`,
for any dose.

### P3.3 The asymmetry as reserve

The **same** receptor occupancies `ρ_i(t)` drive both endpoints; only the transducer differs.
Analgesia has **larger receptor reserve** than ventilation — a given occupancy produces proportionally
more effect — modelled as a reserve gain `r > 1` on the efficacies:

```
τ_i^(analg) = r · τ_i^(vent) = r · α_i,       R_analg = S_analg/(1 + S_analg),   S_analg = r·Σα_iρ_i
```

With `r` large enough, buprenorphine's analgesic stimulus `τ_B^(analg)·ρ_B` saturates `R_analg` even
though `τ_B^(vent)·ρ_B` does not saturate `R_vent` — so **one molecule is a ceiling-limited partial
agonist for ventilation and a near-full agonist for analgesia**, which is buprenorphine's whole
clinical character, from a single binding model. Calibrate `r` to Yassen 2006: buprenorphine showed
no analgesic ceiling up to 0.6 mg/70 kg (max receptor occupancy ≈ 70%, Fig. 5), so `r` is chosen so
that `R_analg` is still rising (not yet plateaued) at `ρ_bup ≈ 0.7` and buprenorphine's analgesic
`R_analg` at clinical occupancies approaches that of a full agonist. (A concrete first value: `r` such
that `r·α_B ≳ 3–5`, giving `R_analg(ρ_bup=0.7) ≳ 0.7–0.8` of maximum with the curve still climbing —
to be fixed against Yassen's dose–effect points when the analgesia endpoint is wired.)

### P3.4 Mapping and what is preserved

- `τ_i^(vent) = α_i` (P3.2): ventilation fit preserved to first order; the deviation `|R_vent − Σαρ|`
  is `O(S²)` and below Olofsen's residual over the fitted range.
- `k_off,B` and `α_B` are endpoint-specific *inputs* (ventilation: `k_off 0.019`, `α 0.81/0.48`;
  analgesia: `k_off 0.0785`, reserve `r`); the receptor core (2)–(4) is shared.
- Reserve `r` is the single extra parameter carrying the asymmetry; it is an analgesia-endpoint
  constant, not a per-patient one.

---

## P4 — Backward-compatibility: an exact identity plus a continuity bound

### P4.1 The exact identity (no slow ligand)

When no slow ligand is present, every ligand is fast and (4) fills the whole pool:
`ρ_j = φ_j/(1+Φ)`, `Φ = Σ_j φ_j = Σ_j Ce_j/C50_j`. **Calibrating `C50_j = MEAC_j` makes the model's
opioid drive identically the additive scale:**

```
Φ ≡ Σ_j Ce_j/MEAC_j = U         (exact — the same arithmetic, not an approximation)        (P4.1)
```

This is the clean statement the spec's §6.1 was reaching for, and it is an **identity, not a bound**.
The occupancy sum is a monotone transform of it, `Σρ_j = Φ/(1+Φ) = U/(1+U)` (verified: `Φ = U` to the
bit, §6). **Note a correction to the spec's acceptance test:** the quantity that equals `U` is the
*drive* `Φ`, not the *occupancy* `Σρ`. `Σρ = U/(1+U)` differs from `U` by the occupancy fraction
(e.g. `U = 1 → Σρ = 0.5`), so a regression test must compare `Φ` (exact) to `U`, **not** `Σρ` to `U`.
The spec's "`|S − U|/U` below tolerance" is satisfied with `S := Φ` at tolerance 0.

Architecturally this means the simplest and safest design — the one recommended — is a **hard switch**:
the existing `% MEAC`, propofol–opioid and opioid–MAC panels keep consuming `U` **unchanged** whenever
no partial agonist/antagonist is in the dose table. Backward compatibility is then exact by
construction: turning the model on changes nothing until buprenorphine or an antagonist appears.

### P4.2 The continuity bound (slow ligand present)

When a slow partial agonist occupies `x = ρ_bup`, the fast agonists reach only the free pool `(1 − x)`:
`Σρ_fast = (1 − x)·Φ/(1+Φ) = (1 − x)·U/(1+U)`. Inverting the occupancy→drive map on the agonist part,
their **MEAC-equivalent drive** is scaled by availability, `Ũ = (1 − x)·U`, so

```
|Ũ − U|/U = x = ρ_bup           (exact; linear in the slow-ligand occupancy)               (P4.2)
```

(verified exactly, §6). So the model is **continuous** with the additive world: as `ρ_bup → 0` the
corrected drive `Ũ → U` with error exactly equal to the buprenorphine occupancy — a clean, bounded,
monotone departure that is zero when buprenorphine is absent and grows only as fast as buprenorphine
actually occupies receptors. This is the quantity to display when the panels switch from `U` to the
receptor model, and it is the precise sense in which "nothing moves until buprenorphine appears".

---

## P5 — Tolerance as a one-parameter manifold

Olofsen fitted two points differing in `(k_on,B, α_B, C50_F)` but **not** `k_off,B`:

| Parameter | Opioid-naive | Chronic opioid use |
|---|---|---|
| k_on,B (mL·ng⁻¹·min⁻¹) | 0.26 | 0.085 |
| α_B (ventilation) | 0.81 | 0.48 |
| C50,F (ng/mL) | 0.68 | 2.23 |
| k_off,B (min⁻¹) | 0.019 | 0.019 (fixed) |

The Patient Profile has four tolerance levels (`OPIOID_TOLERANCE_FACTORS`: none 1, mild 1.5,
somewhat 2, high 3 — today a flat MEAC multiplier). Anchor **naive ↔ none (M = 1)** and
**chronic ↔ high (M = 3)**, and use the MEAC multiplier itself as the tolerance coordinate:

```
θ = log(M)/log(3)         (θ = 0 at none, 1 at high)
k_on,B(θ) = 0.26·(0.085/0.26)^θ         (log-linear — a rate constant)
C50,F(θ)  = 0.68·(2.23/0.68)^θ          (log-linear — a concentration)
α_B(θ)    = 0.81 + (0.48 − 0.81)·θ      (linear in [0,1]; use a logit interpolation if either
                                         anchor is near 0 or 1)
k_off,B   = 0.019                       (held fixed at every level — residence time is intrinsic
                                         to the molecule–receptor bond, not the patient)
```

Evaluated (verified, §6):

| level | M | θ | k_on,B | α_B | C50,F |
|---|---|---|---|---|---|
| none | 1.0 | 0.000 | 0.2600 | 0.810 | 0.680 |
| mild | 1.5 | 0.369 | 0.1721 | 0.688 | 1.054 |
| somewhat | 2.0 | 0.631 | 0.1284 | 0.602 | 1.439 |
| high | 3.0 | 1.000 | 0.0850 | 0.480 | 2.230 |

**A result worth flagging:** `C50,F(θ) ≈ M · MEAC_naive` to within 9% across the range (ratios 1.00,
1.03, 1.06, 1.09 — §6). The chronic/naive C50,F ratio is `2.23/0.68 = 3.28 ≈ 3 = M_high`. So the app's
*existing* MEAC multiplier is, to a few percent, already Olofsen's fentanyl-desensitisation shift —
which both validates the anchor choice (chronic ↔ high) and means this model does not fight the
current tolerance field but *is* a mechanistic restatement of it, extended to buprenorphine. The
`k_on,B` and `α_B` shifts are the genuinely new content the MEAC multiplier cannot carry.

`k_off,B` fixed is the model's key tolerance statement: **blockade is just as durable in the tolerant
patient**, even as sensitivity falls. Analgesia-endpoint `α`/reserve shifts with tolerance are not in
Olofsen's data (ventilation only); hold the analgesia reserve `r` of P3 constant across tolerance and
record as future work.

**Open decision for Shafer.** The chronic anchor could instead be "somewhat" (M = 2) rather than
"high" (M = 3), giving a steeper extrapolation to the most tolerant level. The C50,F≈3.28≈3 match
argues for chronic ↔ high, which is the recommendation; this is a one-line change if Shafer prefers
otherwise.

---

## 6. Validation summary

Scripts in the session scratchpad; all run under R 4.6.1. Numbers below are reproduced from their
output.

**P1 — exact advance vs independent high-accuracy integration.** The update (★★) with Gauss–Legendre
on `J` was compared, over single grid intervals, against (i) an adaptive `integrate()` evaluation of
the integrating-factor solution and (ii) a 2×10⁵-step RK4 of the raw ODE, across nine regimes
(buprenorphine onset; fentanyl rising/falling crowding; `d ≈ 0`; `a = 0`; constant Ce; two agonists;
deep occupancy; slow-antagonist k_off):

- Max |exact − `integrate()`| across the battery: **< 1e-13** in every well-conditioned case
  (typically 1e-16 to 1e-14).
- Gauss–Legendre convergence (hardest two-agonist interval): nGL 4 → 1.1e-7, nGL 8 → 8.7e-12,
  nGL 12 → 4.9e-15. **nGL-8 is the production default.**
- Boundary-layer regime (`(a+k_off)·h = 63.5`, a 60-min single step): plain GL-12 errs 1.4e-5; the
  sub-step guard restores **1.8e-12**.
- On a realistic 1-min grid over 0–240 min (buprenorphine onset against a decaying fentanyl), the full
  interval-by-interval trajectory matches a global fine RK4 to **6.4e-5**, and that residual is
  dominated by the piecewise-linear `Ce` approximation (the same approximation the inhaled-gas engine
  makes), not by the ODE solve — the solve itself is machine-exact.

**P2 — coupled two-slow-ligand advance.** The exponential propagator matched a 2×10⁵-step RK4 to
**7.8e-10** and **4.9e-8** on two intervals. A full 240-min clinical trajectory (buprenorphine
maintenance, antagonist onset at 30 min, fentanyl washout) kept both occupancies in [0,1] with
`max Σρ = 0.975`; buprenorphine fell from 0.90 to 0.71 as the antagonist rose to 0.27 — displacement
gated by buprenorphine's slow `k_off`, exactly the intended physics.

**P4 — identity and continuity.** With `C50_j = MEAC_j` and no slow ligand, `Φ = U` to the bit
(`identical(U, Φ) = TRUE`); `Σρ = U/(1+U)` exactly; `|Ũ − U|/U = ρ_bup` exactly at `ρ_bup = 0,
0.01, 0.1, 0.5`.

**P5 — interpolation.** Table above reproduces Olofsen's two anchors at θ = 0 and 1 exactly and gives
the two intermediate levels; `C50,F(θ)/(M·MEAC_naive) ∈ [1.00, 1.09]`.

---

## 7. Implementation plan against stanpumpR's architecture

The model is a **pharmacodynamic post-process along already-simulated `Ce_i(t)` curves**, not a new PK
engine — the pattern of `applyAntiseizureInteractions()` (which the spec calls the
`pdAdaptation`-style post-process; note there is no `R/pdAdaptation.R` in the tree today, so
`R/antiseizureInteractions.R` is the live template to copy). Nothing is re-simulated.

### 7.1 New PD layer — `R/opioidReceptorInteraction.R`

A function, called after `processdoseTable()` has simulated every drug alone, that:

1. Collects the μ ligands present (via the registry, 7.2) and their effect-site series `Ce_i(t)` on the
   shared dense time base (as `totalOpioidMEAC()` and `meacOverlay.R` already read `$MEAC`/effect-site
   series — reuse `R/derivedSeries.R` so a peak between equispaced points is not stepped over).
2. Builds `D(t) = 1 + Σ_fast Ce_j(t)/C50_j` on the grid, piecewise-linear between points.
3. Advances each **slow** ligand's occupancy by P1 (one slow ligand) or P2 (two or more) interval by
   interval: `advanceBindingExact()` implementing (★★) with Gauss–Legendre-8, the `log1p` small-`d`
   branch, and the sub-step guard; `advanceBindingCoupled()` for the n-slow case.
4. Computes the **fast** ligand occupancies algebraically from (4) in the pool the slow ligands leave.
5. Returns `ρ_i(t)`, the free pool `ρ_0(t)`, the stimulus/effect `S(t)` and `R_vent(t)` / `R_analg(t)`
   (P3), and the corrected drive `Ũ(t)` (P4.2) for the existing panels.

Keep it a pure function of the simulated series (testable without Shiny, via
`simulateDrugsWithCovariates()`), cached by a key over the μ-ligand `Ce` series and the receptor
constants — the `simulationKey()` discipline.

### 7.2 Per-drug receptor-constant registry

Receptor constants live on the drug functions as PK parameters do (§7 of the spec). Add, to each μ
ligand's `drugs_<name>.R` return list, a block e.g.:

```r
receptor = list(
  class   = "slow",            # "slow" (ODE) or "fast" (algebraic)
  kon     = 0.26,              # mL/ng/min  (buprenorphine, ventilation, naive)
  koff    = 0.019,             # 1/min      (fixed across tolerance)
  alpha   = 0.81,              # ventilation efficacy τ^(vent)
  C50     = NA,                # fast ligands only: k_off/k_on, calibrated to MEAC
  endpoint_koff = c(ventilation = 0.019, analgesia = 0.0785),
  reserve = c(ventilation = 1, analgesia = r_analg)   # P3
)
```

with a central table (`inst/extdata/opioidReceptorConstants.csv`, mirrored like
`antiseizureParameters.csv`) carrying value, units, source, population and verification per constant,
rebuilt by a `data-raw/` script and checked by a test. First citizens: buprenorphine, fentanyl,
naloxone (Olofsen 2022 / Yassen 2006–2007); sufentanil, naltrexone, nalmefene follow. Full agonists
get `C50 = MEAC` by the P4.1 calibration and `alpha ≈ 1`; `C50` and `alpha` that are stanpumpR's own
(not a source's) are flagged `pending_verification` and not drawn until signed.

### 7.3 Tolerance wiring (P5)

Add `opioidReceptorConstantsAtTolerance(level)` returning `(k_on,B, α_B, C50_F)` by the P5 manifold,
driven by the existing **Opioid tolerance** field. This sits beside the current MEAC multiplier;
given the `C50,F ≈ M·MEAC` result (P5), the two are consistent, and Shafer's open decision (spec §8.3)
of whether the field *selects the population set* or *keeps the multiplier and adds this for
buprenorphine* is a clean switch — recommend the field drives both, since they agree.

### 7.4 Panels

Under one **Buprenorphine / opioid-receptor interaction** graph option (a `relevantField()` shown only
while a partial agonist or antagonist is in the dose table, per `R/patient-fields.R`):

1. **Receptor occupancy** (stacked area): `ρ_bup`, each agonist's `ρ`, antagonist `ρ`, free `ρ_0`.
2. **Net μ activation / effect** `S(t)` → `R_vent`, `R_analg` (P3): the honest replacement for the
   additive line when a partial agonist/antagonist is present.
3. **Precipitated-withdrawal flag**: a marked downward step in `S(t)` within ~an hour of introducing
   buprenorphine onto a full-agonist background (the signature only the kinetic form produces).
4. **Breakthrough headroom**: the full-agonist `Ce` (hence dose) needed to lift `R` to a target over a
   buprenorphine background.

The existing `% MEAC`, propofol–opioid and opioid–MAC panels keep reading `U` unchanged when no
partial agonist/antagonist is present (P4.1 hard switch); they switch to `Ũ`/`R` only when one is.

### 7.5 Drug-by-drug wiring follows the seven-item procedure

Each μ ligand that gains a receptor block is a change under `docs/adding-a-drug.md` (the drug function,
the CSV/registry row, the test pinning constants with `expect_equal_rounded()`, the help narrative,
and — for a doseable drug — the pregnancy, therapeutic-range and FDA-label rows). Reuse, don't
duplicate: naloxone's competition (`R/drugs_naloxone.R`) and buprenorphine's occupancy band
(`R/drugs_buprenorphine.R`) become instances of this one model.

### 7.6 Tests and validation targets

- Reduction to `U` (P4.1): pure full agonists → `Φ = U` to tolerance 0; `|Ũ − U|/U = ρ_bup`.
- P1/P2 engine tests: `advanceBindingExact()`/`Coupled()` against an independent fine integration
  (the §6 battery), pinned.
- Olofsen 2022 (keystone): reproduce the fitted ventilation curves for buprenorphine + escalating
  fentanyl in both populations, the ≥ 2 ng/mL buprenorphine protection against high-dose fentanyl in
  chronic users, and the probability-of-apnea behaviour.
- Yassen 2007: buprenorphine + naloxone reversal, including the bell-shaped loss of reversal above
  ~4 mg/70 kg/h (the slow-k_off signature).
- Nasser 2014: `ρ_bup(C)` reproduces ~70% occupancy at 2 ng/mL (an independent check on `k_off/k_on`;
  the Emax/EC50 occupancy model already lives in `drugs_buprenorphine.R`).
- Ceiling (Olofsen α < 1) and precipitated-withdrawal timing (buprenorphine near a fentanyl peak vs
  after fentanyl has declined).

---

## 8. What is settled, and what is left for Shafer

**Settled (this note):** the exact single-slow-ligand advance (P1, machine-precision, degenerate
limits and the one conditioning hazard characterised); the coupled slow-ligand advance and its
linearity (P2); the operational-model carrier and the reserve-asymmetry mapping (P3); the exact
backward-compatibility identity and continuity bound, with a correction to the spec's acceptance test
(compare `Φ`, not `Σρ`, to `U`) (P4); the tolerance manifold and its near-coincidence with the
existing MEAC multiplier (P5).

**Left for Shafer (unchanged from spec §8, plus two from here):**
1. Lead endpoint (ventilation first, the safety endpoint with the interaction data — recommended).
2. Full-agonist `C50` scale: calibrate to MEAC (P4.1, keeps every panel still — recommended).
3. Tolerance field drives the population set vs keeps the multiplier (P5 shows they agree; recommend
   the field drives both).
4. Whether buprenorphine re-enters the `U`-based panels or only the new occupancy panels (recommend:
   new panels own buprenorphine; `U` panels switch to `Ũ`/`R` only when a partial agonist/antagonist
   is present).
5. **New:** the analgesia reserve `r` (P3.3) — fix against Yassen 2006's dose–effect points when the
   analgesia endpoint is wired.
6. **New:** the chronic tolerance anchor — chronic ↔ high (M = 3, recommended) vs chronic ↔ somewhat.

### Sources
Olofsen E, Algera MH, Moss L, et al. *JCI Insight* 2022;7(9):e156973 (the engine; buprenorphine +
fentanyl ventilation, two populations). · Yassen A, et al. *Clin Pharmacokinet* 2007;46:965–980
(PMID 17922561; buprenorphine + naloxone reversal). · Yassen A, et al. *Anesthesiology*
2006;104:1232–1242 (PMID 16732095; antinociception receptor kinetics). · Nasser AF, et al. *Clin
Pharmacokinet* 2014;53:813–824 (PMID 25015174; occupancy vs plasma). · Black JW, Leff P. *Proc R Soc
Lond B* 1983;220:141–162 (the operational model). · Lutfy K, Cowan A. *Curr Neuropharmacol*
2004;2:395–402 (the μ-only scope boundary).
