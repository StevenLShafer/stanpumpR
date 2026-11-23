# The mathematics of a buprenorphine μ-receptor interaction model

**Status:** design mathematics / companion to `docs/buprenorphine-interactions.md`. Proposal, not
implemented.
**Author:** Claude (Opus 4.8), 2026-11-19, at the request of Steven L. Shafer.
**Reconciliation:** the derivations below were cross-checked by four independent passes — the original
derivation, ChatGPT, Gemini, and a second Opus 4.8 pass (the typeset note in PR #804). They converge on
P1, P2 and P4; P3 is resolved below. Where a later pass corrected or sharpened the first, it is marked
**[correction]**. Steven L. Shafer's three design decisions of 2026-11-19 are recorded in §9.

> **Scope.** μ receptor only. Ligands: buprenorphine; the full agonists fentanyl and sufentanil; the
> antagonists naloxone, naltrexone, nalmefene. κ/δ, ORL-1/NOP (the bell-shaped high-dose effect) and
> norbuprenorphine are out of scope.

A typeset PDF version of this note, for any mathematician who wants the details, is
[`docs/buprenorphine-interaction.pdf`](buprenorphine-interaction.pdf).

---

## 0. The system

Index the μ ligands by *i*. Each has a simulated effect-site concentration `Ce_i(t)` and receptor
rate constants `k_on,i`, `k_off,i` (`K_D,i = k_off,i/k_on,i`). The receptor pool is normalised to 1;
`ρ_i` is the fraction bound by ligand *i*, `ρ_0 = 1 − Σ_j ρ_j` the free fraction. Binding kinetics
(Olofsen 2022; Yassen 2006/2007):

```
dρ_i/dt = k_on,i · Ce_i(t) · ρ_0(t) − k_off,i · ρ_i(t),      ρ_0 = 1 − Σ_j ρ_j.        (1)
```

Ligands split by residence time `τ = 1/k_off`:

- **Slow** — buprenorphine (`k_off ≈ 0.019 min⁻¹`, t½ ≈ 37 min) and the long-residence antagonists
  naltrexone, nalmefene — keep a differential state (1).
- **Fast** — fentanyl, sufentanil, naloxone — reach their conditional equilibrium within the pool the
  slow ligands leave free. Setting `dρ_j/dt = 0` and summing:

```
ρ_j = (1 − Σ_slow ρ) · φ_j / (1 + Σ_k φ_k),   φ_j = Ce_j/C50_j,   C50_j = k_off,j/k_on,j.   (2)
```

Define the **crowding factor** `Φ(t) = Σ_fast φ_k = Σ Ce_k/C50_k`. Then
`ρ_0 = (1 − Σ_slow ρ)/(1 + Φ)` and the total fast-bound fraction is `(1 − Σ_slow ρ)·Φ/(1+Φ)`.
Effect (ventilation), linear transduction (Olofsen): `E = E0·(1 − Σ_i α_i ρ_i)`, with `α = 1` for a
full agonist, `α < 1` for buprenorphine (the ceiling), `α = 0` for a pure antagonist.

Between grid points each `Ce_i(t)` is **piecewise-linear in t**; the engine advances occupancy exactly
across one such interval given the endpoint concentrations.

**Fitted parameters** (Olofsen 2022, ventilation; two populations):

| Parameter | Opioid-naive | Chronic high-dose |
|---|---|---|
| k_on,B (mL·ng⁻¹·min⁻¹) | 0.26 | 0.085 |
| k_off,B (min⁻¹; t½ ≈ 37 min) | 0.019 | 0.019 |
| α_B | 0.81 | 0.48 |
| C50,F = k_off,F/k_on,F (ng/mL) | 0.68 | 2.23 |
| α_F (fixed) | 1 | 1 |
| E0 = VB (L/min) | 20.2 | 20.2 |

## 1. One grid interval

On `[0,h]` write every input linear in *t*: `k_on,B·B(t) = r + s·t` and `1 + Φ(t) = p + q·t`, with
`p ≥ 1 > 0` (a crowding factor is non-negative). Then `a(t) = k_on,B·B(t)/(1+Φ(t)) = (r+st)/(p+qt)` is
a Möbius function; polynomial division gives `a(t) = A + w/(p+qt)`, `A = s/q`, `w = (rq − sp)/q`.

---

## P1 — exact advance for the single-slow-ligand ODE

With buprenorphine the only slow ligand, `Σ_slow ρ = x ≡ ρ_bup`, so `ρ_0 = (1−x)/(1+Φ)` and (1)
collapses to a **scalar linear ODE** with a time-varying coefficient:

```
dx/dt = a(t)·(1 − x) − k_off,B·x,     i.e.    dx/dt + [a(t) + k_off,B]·x = a(t).       (P1)
```

### Exact closed form
With `κ = k_off,B`, the integrating-factor exponent `Ψ(t) = ∫₀ᵗ[a+κ] = (A+κ)t + m·ln((p+qt)/p)`,
`m = w/q` and **`w = r − sp/q`**, and `x(h) = e^{−Ψ(h)}[x(0) + ∫₀ʰ e^{Ψ}a dt]`. The source integral
reduces to **lower incomplete-gamma** terms. *(Both audits sharpened the naming: the order
`m = (rq − sp)/q²` is **generically non-integer**, so it is an exponential integral only for the special
case `m ∈ −ℕ₀`; and the incomplete-gamma argument is **negative** whenever `(s/q + κ)/q > 0`, putting the
standard function on a branch cut — a real-valued form needs Tricomi's `γ*` or Kummer's `M`.)* So the
advance is closed-form — a logarithm in `Ψ` plus an incomplete-gamma term in the source — which is exactly
why the production path quadratures the bounded kernel instead (below).

### Overflow-proof form (the production path)
Never form `μ = e^{Ψ}` or the `((p+qt)/p)^m` factor alone — either can overflow while the product is
finite. Write the advance as a weighted average with a **bounded** kernel:

```
x(h) = G(0,h)·x(0) + ∫₀ʰ G(t,h)·a(t) dt,
G(t,h) = exp(−(A+κ)(h−t) − m·ln((p+qh)/(p+qt))) ∈ (0,1].                                  (P1*)
```

Evaluate each kernel exponent as **one number** (not `e^{·}·pow(·)`).

### [correction] Exact is not the same as best-conditioned
The incomplete-gamma closed form is exact, but its evaluation at non-integer / negative order has its
own cancellation and **pole traps** (negative-integer order), and stanpumpR imports only base R/`stats`
(DESCRIPTION) — no special-function package. Because `a(t)` is smooth and bounded on one step, a
**3–4 node Gauss–Legendre quadrature of the bounded kernel (P1\*)** reaches machine precision with none
of that fragility. **Recommendation: production = bounded-kernel quadrature; closed form = reference /
validator.** "Load-bearing" is the *exactness requirement*, which (P1\*) also meets — the
special-function evaluation need not ship.

### Degenerate limits (all exact, all handled)
- `a ≡ 0` (`B = 0`): pure decay, `x(h) = x(0)·e^{−κh}`.
- Constant `a` (`q = 0, s = 0`, or `w = 0`): `x(h) = x_ss + (x(0) − x_ss)·e^{−(a+κ)h}`,
  `x_ss = a/(a+κ)`. The `w = 0` branch (`rq = sp`, coincident Möbius roots) collapses here — test `|w|`.
- `q → 0` (Φ constant, `a` linear in *t*): `m = w/q` diverges but `m·ln(1+qt/p) → wt/p` is finite —
  implement via the series `ln(1+qt/p)/q → t/p`, **not** the raw formula, or it cancels catastrophically.
  Here `Ψ` is quadratic in *t* and the source integral is **an error function only for `s < 0` (falling
  B); for `s > 0` (rising B — every buprenorphine onset) it is Dawson's integral `erfi`, and for `s = 0`
  it is elementary** (both audits).
- A **cancellation-free `∫_t^h a`** for all `q`, removing the `q → 0` branch entirely (both audits), with
  `ε = q(h−t)/(p+qt)`: `∫_t^h a du = [(r+st)(h−t)/(p+qt)]·ln(1+ε)/ε + [s(h−t)²/(p+qt)]·(ε−ln(1+ε))/ε²`,
  taking the `ε → 0` limits (1 and ½) by series.
- Guard on small `|q|` and `|w|` before evaluating.

### Conditioning, sub-stepping, and three edge cases to test
- Large `(A+κ)h` (many time constants/step): `G(0,h)` underflows, `x →` quasi-steady — correct, but put
  the quadrature nodes near `t = h` where `G = O(1)`, and sub-step so `∫_t^h(a+κ) = O(1)` per sub-step.
- **[correction] Crowding surge — the sub-step trigger must watch the pole, not only time constants.**
  `a(t) = (r+st)/(p+qt)` has a pole at `t = −p/q`; when `1+Φ` surges across one step (a fast bolus, Φ
  0 → 100) the pole sits beside `[0,h]` and collapses Gauss–Legendre to **3.8% error**, even though the
  step spans only `ΔΨ ≈ 0.02` time constants, so a "many time constants" trigger never fires (both audits,
  identical counterexample). The engine now also sub-steps on the **linear crowding change**:
  `nsub ≥ |Φ₁−Φ₀| / ((R−1)·min(1+Φ₀, 1+Φ₁))`, keeping the pole ≥ one sub-interval away; the counterexample
  returns 2e-14, and 7e-14 at a 500× surge.
- **Small-ΔΦ confluence** (`|Φ_h − Φ_0| ~ 1e-6` with `B` changing): the `m·ln` difference blows up as
  `|m| ~ (ΔΦ)^{−2}` toward the Dawson/`erfcx` limit — use the cancellation-free `∫a` above.
- **Dual-endpoint cancellation**: the kernel form (P1\*) `x₀·G(0,h) + ∫G·a` stays exact down to
  `x = 1e-300`, where the by-parts form `1 − (1−x₀)G − κ∫G` loses digits near `x ≪ 1` (but the latter
  removes the pole — hence the crowding sub-step trigger is kept with the (P1\*) form).

### Validation
`x(h)` against adaptive Dormand–Prince 5(4) / Gauss–Kronrod on (P1); expect ≈ 1e-12 relative, in the
style of `test-transit-chain.R`. *(Cross-checked: the original pass ≤ 1e-13; ChatGPT 2.3e-13; Gemini
70-digit mpmath.)*

---

## P2 — two or more slow ligands

**[correction] The multi-slow-ligand advance is linear — indeed compartmental — not nonlinear.** With
buprenorphine plus a long-residence antagonist, both keep differential states (no scalar reduction), but
`k_on,i·Ce_i(t)·ρ_0` is linear in the occupancies (`Ce_i` is a known input, not a state), and the only
"nonlinear-looking" factor is itself linear. **[correction] The explicit auxiliary state is the AVAILABLE
pool, not the free fraction** (both audits): `x_avail = 1 − Σ_slow ρ = (1+Φ)·ρ_0`. Because
`a_i = k_on,i·Ce_i/(1+Φ)` already carries the `1/(1+Φ)`, the association term `a_i·x_avail = k_on,i·Ce_i·ρ_0`
is correct, `(ρ_B, …, x_avail)` sums to 1, and there is no `dΦ/dt` term (which carrying the *free* fraction
`ρ_0` would wrongly introduce). For two slow ligands `B`, `A`:

```
d/dt (ρ_B, ρ_A, x_avail)ᵀ = K(t)·(ρ_B, ρ_A, x_avail)ᵀ,
K = [[−k_off,B, 0, a_B], [0, −k_off,A, a_A], [k_off,B, k_off,A, −(a_B+a_A)]].             (P2)
a_i(t) = k_on,i·Ce_i(t)/(1+Φ(t)).
```

`K(t)` is **Metzler** (off-diagonals ≥ 0) with **zero column sums** (conservative) — a continuous-time
Markov generator. So `exp(KΔ)` is nonnegative and **column-stochastic**: a matrix-exponential step keeps
the state ≥ 0 and `Σ = 1` **exactly**, the physical simplex preserved by construction, **no clamp**. (The
negative off-diagonals that appear in a 2-state `(ρ_B, ρ_A)` reduction are an artifact of eliminating
`x_avail`; keep it and they vanish.) After the step, recover `ρ_0 = x_avail/(1+Φ)` and the fast occupancies
`ρ_j = x_avail·φ_j/(1+Φ)` by (2).

**The step.** Over `[0,h]`, `K(t)` varies through the Möbius `a_i(t)`. The engine advances with the
**4th-order commutator-free Magnus** (Blanes & Moan), two Gauss nodes `c₁,₂ = ½ ∓ √3/6` and weights
`α₁,₂ = ¼ ± √3/6`:
```
state ← exp(h(α₂K₁ + α₁K₂))·exp(h(α₁K₁ + α₂K₂))·state,     Kᵢ = K(cᵢh).
```
The **ordering** matters (both audits confirmed it empirically: single-step errors `1.5e-4 → 9.0e-6 →
4.2e-7 → 1.6e-8` on halving, ratios climbing toward 32, a 5th-order *local* error; the reversed ordering
is only 2nd order), and each factor's zero column sums preserve `Σ = 1` exactly. Sub-stepped on the same
time-constant and crowding criteria as P1.

**[correction] Positivity is unconditional, not a hedge.** Although `α₂ = ¼ − √3/6 < 0`, each factor's
exponent is Metzler for every `h > 0` with piecewise-linear inputs. Writing `a_i = N/D` with `N, D`
affine on the step (`N₀,N₁ ≥ 0`, `D₀,D₁ > 0`), the exact identity `α₁c₁² + α₂c₂² = 0` gives
`[α₂a(c₁h) + α₁a(c₂h)]·D(c₁h)D(c₂h) = (N₀D₀ + N₁D₁ + 4N₁D₀)/12 ≥ 0` (and the mirror image with `4N₀D₁`
for the other factor). *(This corrects the first audit's `(2+√3)/12`; the coefficient is `4/12`, per the
second audit. A `[6/6]` Padé can still leave ~`−1e-17` on a zero entry, so flooring tiny negative roundoff
is cheap insurance.)* It requires that no step straddle an input breakpoint — which 4th order needs anyway.

**Validation:** dense RK of the `(N+1)`-state system; `Σ = 1` and `ρ ≥ 0` to machine precision; 4th-order
convergence confirmed. *(Cross-checked: ≈ 9e-7 on a coarse step; both audits confirm the order, ordering
and column-stochasticity.)*

---

## P3 — operational transduction and the analgesia/ventilation asymmetry

Replace the linear transduction with the operational model of agonism (Black & Leff 1983): give each
ligand an efficacy `τ_i` and each **endpoint** a reserve/gain `γ`, with transduction Hill `n`:

```
E/E_max = Θⁿ/(1 + Θⁿ),     Θ = γ·Σ_i τ_i ρ_i.                                            (P3)
```

### [correction] The reduction to Olofsen's linear form requires n = 1
For a low-reserve endpoint `Θ ≪ 1`, `Θⁿ/(1+Θⁿ) ≈ Θⁿ`, which is **linear in occupancy only when
`n = 1`**: then `E/E_max ≈ γ·Σ τ_i ρ_i`, and writing ventilation as depression `E = E0(1 − Σα_iρ_i)`
identifies `α_i = γ_vent·τ_i`. For `n ≠ 1` the low-reserve limit is a power law. **State n = 1 whenever
the reduction is invoked.**

### Ventilation stays linear; the operational form is for analgesia
With `n = 1`, ventilation is Olofsen's own linear transducer, whose ceiling is **exactly `α_B`**
(0.81 naive, 0.48 chronic) at full buprenorphine occupancy — correct, and the respiratory-sparing
plateau. *(The first pass erred here: it tried to express ventilation through the sigmoid and mis-stated
the plateau as `α_B`; with a single operational reserve the sigmoid ceiling is `α_B/(1+α_B) ≈ 0.45`,
not `α_B`. Keeping ventilation linear removes the error.)* The operational form carries the **analgesia**
endpoint:

- **Analgesia** (high `γ`): `Θ` saturates even at the modest `Σ τ_i ρ_i` a partial agonist makes ⇒
  buprenorphine looks **near-full**.
- **Ventilation** (low `γ`): `E/E_max ≈ γ·Σ τ_iρ_i` stays sub-saturating ⇒ a ceiling `≈ γ_vent·τ_bup < 1`
  ⇒ the **partial agonist**.

One receptor, different reserve per endpoint — the standard receptor-reserve account of the μ
analgesia/ventilation split, needing neither NOP nor biased signalling (consistent with the μ-only scope).

### [correction] Identifiability: ventilation data fix only the product γ·τ
In a single endpoint `τ` and `γ` enter only as `γτ`, so ventilation data fix `γ_vent·τ_i = α_i` but not
the split. The **analgesia reserve `γ_analgesia`** is a *second parameter* that cannot be read off the
ventilation fit; it must come from analgesia data or a clinical anchor. **That anchor is Shafer's
decision #3 (§9): the analgesic ceiling at 2–4× MEAC.** The efficacy *ratio*
`τ_bup/τ_F = α_bup/α_F` is endpoint-independent; the absolute ceiling differs by endpoint through `γ`.

### The deconvolution is interpretive support, not a parameter substitution
Gemini observed that `(1 − α_B)·k_on,B` is nearly population-independent (0.0494 naive vs 0.0442
chronic), giving a single deconvolved `K_D,B ≈ 0.38–0.43 ng/mL` — which reconciles the ventilation fit
with Nasser 2014's PET occupancy (half-max ≈ 0.67 ng/mL plasma) far better than the **raw**
`K_D = k_off/k_on` (0.073 naive, 0.22 chronic, which is 3–9× off Nasser). This is encouraging evidence
that the operational structure is right and that buprenorphine's *true* affinity is one
population-independent number. **But**: the ventilation fit does not force this split (above), and
Nasser is plasma whole-brain PET vs effect-site ventilation, so it is an **order-of-magnitude sanity
check, not a tight validation — do not present them as agreeing closely.** The **drawn** curves use
Olofsen's fitted `k_on,B`/`k_off,B` and `α_B` directly (respiratory) plus the clinical analgesia reserve
(analgesia); the deconvolution informs interpretation, it is not substituted into the model.

---

## P4 — backward compatibility

Calibrate each fast agonist so `C50_j = MEAC_j`. Then `φ_j = Ce_j/MEAC_j`, so the crowding factor is
**exactly the legacy opioid scale**, `Φ = Σ_fast Ce_j/MEAC_j = U`. With no slow ligand,
`Σ_fast ρ = U/(1+U)` and `ρ_0 = 1/(1+U)`.

### [correction] P4 has an exact invariant, not merely a bounded deviation
The quantity equal to `U` is the crowding/drive `Φ`, **not** the occupancy `Σρ` (`Σρ = U/(1+U)` is a
saturating transform of `U`; `U = 1 ⟹ Σρ = ½`, i.e. MEAC = the 50%-occupancy point). Read the legacy
panel as the fast-ligand **odds** and the correspondence is exact *and survives a slow ligand*: with
`Σ_slow ρ` arbitrary,

```
(Σ_fast ρ) / ρ_0 = Φ = U,     exactly, independent of Σ_slow ρ.                          (P4)
```

So **report `U = Σ_fast ρ / ρ_0` and the legacy scale is reproduced identically, at all times** —
buprenorphine enters only through its own occupancy term, never by corrupting `U`.

**[correction] Exact but near-tautological, and three caveats (both audits).** The odds equal `U` follows
directly from the QSS relation (2) — `ρ_j/ρ_0 = φ_j` per fast species — not from the full kinetic system,
so it is a property of the reduction. Three precise points: (i) the raw fast occupancy is
`Σ_fast ρ = (1−σ)·U/(1+U)` with `σ = Σ_slow ρ`; the `U/(1+U)` form holds **only when `σ = 0`**. (ii) The
slow modes scale fast occupancy **multiplicatively** by `(1−σ)`, not "additively" — and the *odds* are
blind to `σ` by construction (they return the input), so reading `U` from the odds shows no slow-mode
effect at all; the physical blockade lives in the `(1−σ)` scaling of the occupancies, which is what the
occupancy panel draws. (iii) `Φ = U` requires **every** fast species to carry a weight in `U`; a fast
antagonist (naloxone), which is in `Φ` but not in `U`, makes `Φ > U` — so the odds must be taken over the
fast **agonists** (`Σ_fast-ag ρ / ρ_0`), which is exactly the available-drive odds the engine computes.
**Acceptance test: compare `Φ` (exact) to `U`, not `Σρ` to `U`.**

*Under the unified framing (spec §8) the engine runs on each ligand's true `K_D`, not `C50 = MEAC`, so
`Φ ≠ U` by construction; backward compatibility is then by the hard switch (the `U`-panels are untouched
until a partial agonist/antagonist is dosed), and the MEAC-equivalent readout `(Σ eᵢρᵢ)/ρ*` reduces to `U`
at low occupancy. The identity above is the clean statement of the `C50 = MEAC` limit it reduces to.*

---

## P5 — tolerance as a one-parameter manifold

Olofsen gives two states (naive, chronic). With two anchors only a one-parameter monotone path can be
supported, so interpolate linearly in the natural transform of each parameter across `θ ∈ [0,1]`. **Under
the unified framing (spec §8) the binding `K_D` is fixed and tolerance moves the TRANSDUCTION and the
full-agonist potency:**

- `logit(α_B)` linear in θ (0.81 → 0.48), i.e. the ventilation **reserve** — and this has a mechanistic
  reading (both audits): under Black–Leff with `n = 1`, `logit(α_B) = ln(γ_vent·τ_B)`, so logit-interpolating
  `α_B` is exactly **log-linear decay of the ventilation reserve** `γ_vent`, matching the full-agonist potency.
- `log(full-agonist potency)` linear in θ (the Grid-B factor `3^θ`), equivalently the apparent `C50`
  shift Olofsen fitted (0.68 → 2.23 for fentanyl's ventilation).
- buprenorphine analgesic efficacy `e_bup` interpolated with it.
- buprenorphine true `K_D` (≈ 0.40) and `k_off` **fixed** — binding is intrinsic to the bond, not the patient.

**Spacing — decision (§9): Grid B.** The four levels sit at `θ_k = ln M_k / ln 3 = (0, 0.369, 0.631, 1)`
for `M_k = (1, 1.5, 2, 3)` (`OPIOID_TOLERANCE_FACTORS`), so the model and the existing knob move together.

**[correction] This is a parsimonious monotone path, not the maximal one** (both audits): the log and logit
scales are themselves (untestable) curvature choices from two anchors, as is `θ = ln M/ln 3`. Stated as a
defensible choice, not a theorem; "anything with curvature over-fits" is dropped. (Note `k` and `K = k_off/k_on`
for one species are not independent, so the interpolation is specified per species on its free parameters.)

---

## 6. Validation summary

| Result | Checked against | Achieved |
|---|---|---|
| P1 advance (P1\*) | DP5(4) / Gauss–Kronrod on (P1) | ≤ 1e-13 (orig), 2.3e-13 (ChatGPT), 70-digit (Gemini) |
| P1 degenerate limits | decay / constant-coeff / erf forms | exact |
| P2 matrix step | dense RK of (N+1)-state system; Σρ=1, ρ≥0 | ≈ 1e-9/1e-10, simplex exact |
| P3 reduction | linear form at n=1, small Θ | matches slope (exact to 1st order) |
| P4 invariant (P4) | algebra; sim with/without slow ligand | `Φ = U` to the bit |
| P5 interpolation | Olofsen anchors at θ=0,1 | reproduced exactly |
| P1 crowding surge (Φ 0→100 in one step) | dense RK4 | 2e-14 (was 3.8e-2 before the fix) |

**[correction] What the error figures mean (both audits).** The `~1e-11` (P1) and `O(h⁴)` (P2 CF4) are
exactness **with respect to the piecewise-linear `Ce(t)` the simulator plots** — they give unconditional
stability and exact simplex conservation on arbitrary grids. Convergence to the underlying *smooth*
biophase ODE is `O(h²)`, from linearly interpolating `Ce` between grid points, whenever `h` is comparable
to `1/k_e0` (e.g. a long washout step) — exactly the approximation the inhaled-gas engine makes. Initial
condition: `ρ_slow = 0` at `t₀ = 0` before the earliest dose (not at a shifted plot-window start).

---

## 7. Where an implementation sits

A **pharmacodynamic post-process** after `simCpCe()` has produced every opioid's `Ce_i(t)`, not inside
the PK engines — the pattern of `applyAntiseizureInteractions()` running after each drug is simulated
alone. (Note: there is no `R/pdAdaptation.R` in the live tree; `R/antiseizureInteractions.R` is the
template to copy.) It: (1) partitions μ ligands into slow (differential state) and fast (QSSA (2)) by
`k_off`; (2) advances slow occupancies interval-by-interval on the shared non-uniform grid — P1 (one) or
P2 (several); (3) forms `Φ(t)` and the fast occupancies by (2) at each output time; (4) maps occupancy to
effect — ventilation linear now, analgesia operational (P3) when the analgesia endpoint is wired. It
leaves the additive panels identical until buprenorphine or an antagonist is dosed — guaranteed exactly
by (P4).

## 8. Presentation (per decision #1; full design in the spec §4)

Two validated potency scales, bridged only by `ρ_bup`:
- **Occupancy competition + respiratory panel** run on the **binding** scale (Olofsen's ventilatory
  `C50,F`; buprenorphine's binding constants) — the only physically consistent choice, since an
  analgesic MEAC cannot enter a binding denominator.
- **`% MEAC` and opioid–MAC panels** keep the **analgesic MEAC** scale `U`, unchanged, gaining the
  **available-drive line `Ũ = (1 − ρ_bup)·U`** (blockade, from the occupancy-derived `ρ_bup`) when
  buprenorphine is present.
- Panels: a stacked **receptor-occupancy** graph (ρ_bup / full agonists / antagonist / free), the two
  **effect curves** (analgesia; respiratory depression with its ceiling), the **`Ũ`** line, and a
  **precipitated-withdrawal marker** (a sharp fall in net activation when buprenorphine is introduced on
  a full-agonist background). Offered only while a partial agonist or antagonist is drawn.

## 9. Shafer's design decisions (2026-11-19)

1. **Dual C50 scale.** The respiratory panel uses Olofsen's ventilatory `C50`; `% MEAC` and the
   opioid–MAC interaction keep the analgesic MEAC.
2. **Tolerance Grid B.** `θ = ln M / ln 3 = (0, 0.369, 0.631, 1)`, anchored to `OPIOID_TOLERANCE_FACTORS`
   `(1, 1.5, 2, 3)`.
3. **Analgesic ceiling 2–4× MEAC** (naive), rendered as a band — Typical 3×, Lower 2×, Upper 4× — which
   also sets the precipitated-withdrawal / breakthrough crossover `U*`.

## 10. Open items

- The analgesia endpoint is drawn only once `γ_analgesia` is fixed from decision #3; until then the
  engine is ventilation-only and the linear transducer stands.
- Slow antagonists (naltrexone, nalmefene) need `k_on`/`k_off` from binding-kinetics sources before they
  join the slow set; buprenorphine, fentanyl and naloxone are the first citizens.
- norbuprenorphine (μ/ORL-1 active) is out of scope until it is a PK species.

### Sources
Olofsen, Algera, Moss, Dobbins, Groeneveld, van Velzen, Niesters, Dahan, Laffont. *JCI Insight*
2022;7(9):e156973 (buprenorphine–fentanyl competitive ventilation). Yassen 2006 (*Anesthesiology*
104:1232), Yassen 2007 (*Clin Pharmacokinet* 46:965), Nasser 2014 (*Clin Pharmacokinet* 53:813), Lutfy &
Cowan 2004 (*Curr Neuropharmacol* 2:395). Black & Leff 1983 (operational model). Reviews reconciled:
ChatGPT, Gemini, and the second Opus 4.8 pass (PR #804).
