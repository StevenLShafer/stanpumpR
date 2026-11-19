# A kinetic receptor-binding model of buprenorphine's interaction with other opioids

**Status:** design specification / research proposal. Nothing here is implemented yet.
**Author:** Claude (Opus 4.8), 2026-11-19, at the request of Steven L. Shafer.
**Scope:** a μ-opioid *pharmacodynamic* interaction layer for stanpumpR that lets
buprenorphine, the full agonists, and the antagonists share one receptor and
modulate each other's effect — analgesia and ventilation — without disturbing
the pharmacokinetics or the existing full-agonist behaviour.

> **Revision, 2026-11-19.** An earlier draft of this spec built the interaction on
> *equilibrium* competitive occupancy (the Gaddum `K_D`-only form) and offered the
> binding kinetics as an optional refinement. That is backwards, and the correction
> is the whole point of the model. **What makes buprenorphine difficult is not its
> affinity but its slow dissociation from the receptor** — a kinetic property that
> equilibrium equations cancel out. Steven L. Shafer made this point directly, and
> it is settled in the literature: Olofsen, Algera, Dahan & Laffont (*JCI Insight*
> 2022;7(9):e156973) published a fitted human model of the buprenorphine–fentanyl
> interaction on ventilation built on exactly the receptor association/dissociation
> kinetics below, and Yassen 2007 did the same for buprenorphine–naloxone. This spec
> is now built on *their* equations; the equilibrium form survives only as the
> quasi-steady-state reduction that those authors themselves apply to the fast ligands.

---

## 1. The problem, stated precisely

**Everything in the app that combines opioids is additive on one scale.** The
% MEAC total-opioid line, the propofol–opioid response surface (`models/interaction.md`,
Bouillon 2004), and the opioid–MAC reduction (`models/opioid-mac.md`) all reduce
the opioids present to

```
U = Σ_i  Ce_i / MEAC_i                                                    (1)
```

a sum of effect-site concentrations each divided by that opioid's minimum effective
analgesic concentration. Equation (1) is the correct behaviour of full agonists that
do not compete for a limiting number of receptors.

**Buprenorphine breaks every assumption behind (1)**, and the reason is *kinetic*.

| Property | Buprenorphine | Source |
|---|---|---|
| μ dissociation rate k_off | 0.019 min⁻¹, **t½ ≈ 37 min** | Olofsen 2022 Table 3 (ventilation) |
| μ association rate k_on | 0.26 (opioid-naive) / 0.085 (chronic use) mL·ng⁻¹·min⁻¹ | Olofsen 2022 Table 3 |
| intrinsic activity + reserve α | 0.81 (naive) / 0.48 (chronic use) — both < 1 | Olofsen 2022 Table 3 |
| apparent potency K = k_off/k_on | 0.073 (naive) / 0.22 (chronic) ng/mL | Olofsen 2022 |
| occupancy vs plasma C | μORO = 91.4·C/(0.67 + C) % | Nasser 2014 Table 3 |

So buprenorphine is given `MEAC = 0` and dropped from `U` (`R/drugs_buprenorphine.R`).
That is honest, but it means the app is silent on the four questions a prescriber asks:
the **ceiling**, **blockade** of an added full agonist, **precipitated withdrawal**,
and the **breakthrough-analgesia** headroom on maintenance. All four are shared-receptor,
and three of the four are *kinetically* controlled — they depend on buprenorphine's
slow k_off, not on its affinity.

### 1.1 Affinity is not displaceability — the point the equilibrium model misses

Occupancy at equilibrium is set by `K_D = k_off/k_on` alone; `k_off` and `k_on`
appear only through their ratio. But *displaceability* — how readily a bound ligand
yields its receptor to a competitor — is governed by `k_off` by itself (the residence
time `τ = 1/k_off`), which equilibrium has cancelled.

**Sufentanil is the proof.** Its `K_D` is as low as or lower than buprenorphine's — it
binds *more tightly* at equilibrium — yet clinically it is readily displaced by another
opioid, because its `k_off` is fast: the receptors it holds turn over continuously and a
competitor captures them as they free. Buprenorphine, with an ordinary-to-high affinity
but a ~37-minute dissociation half-life, is **not** displaced: once bound it stays, and a
competitor can take a receptor only as fast as buprenorphine releases it. Equal `K_D`,
opposite behaviour. No equilibrium model can tell them apart. This is the drug–target
*residence time* idea (Copeland 2006; Tummino & Copeland 2008), and for buprenorphine it
is not a subtlety — it is the mechanism.

The asymmetry this produces is the whole clinical picture:

- **buprenorphine displacing fentanyl** — slow, gated by buprenorphine's slow `k_on`
  onto transiently-free receptors (it must wait for fentanyl to let go, then is slow to grab);
- **fentanyl displacing buprenorphine** — very slow, gated by buprenorphine's slow `k_off`
  (fentanyl can seize a receptor only in the rare moments buprenorphine releases it);
- **fentanyl displacing sufentanil** — fast, both fast `k_off`.

An equilibrium model gives all three the same answer. The kinetic model gives all three
correctly, and it is why buprenorphine blockade is durable, why reversal needs *sustained*
high naloxone (Yassen 2007's bell-shaped reversal), and why induction protocols lower the
full agonist before introducing buprenorphine.

---

## 2. The core: kinetic competitive receptor binding (Olofsen 2022 / Yassen 2007)

Index the μ ligands present by *i* (every opioid in the dose table, plus the antagonists).
Each carries an **effect-site concentration** `Ce_i(t)` (already simulated, driven to the
biophase by the drug's own `ke0`) and receptor rate constants `k_on,i`, `k_off,i`
(`K_D,i = k_off,i/k_on,i`). The receptor pool is normalised to 1; `ρ_i` is the fraction
bound by ligand *i*, `ρ_0 = 1 − Σρ_j` the free fraction.

Each ligand's occupancy evolves by association/dissociation (Olofsen 2022 eq. 1–2;
Yassen 2006/2007):

```
dρ_i/dt = k_on,i · Ce_i(t) · ρ_0(t)  −  k_off,i · ρ_i(t)                  (2)
          ρ_0(t) = 1 − Σ_j ρ_j(t)
```

This is the engine. It is driven by the already-computed `Ce_i(t)` curves, so it is a
cheap post-process along the simulated time grid — the same architectural pattern as the
tolerance panel (`R/pdAdaptation.R`), with the one addition that the states couple through
`ρ_0`. Nothing is re-simulated.

### 2.1 The slow/fast reduction the Leiden group use (and why it is exact enough)

Equation (2) is stiff: buprenorphine's `k_off` is ~0.02 min⁻¹ while a full agonist's is
orders of magnitude faster. Olofsen 2022 resolve this by **singular perturbation** — they
keep the ODE for the slow ligand and collapse the fast ligand to its conditional
equilibrium. For fentanyl, "because k_off,F is large, assume `k_on,F·[F]·[R] − k_off,F·[FR] = 0`",
which gives (their eq., normalised):

```
[FR] = (1 − [BR]) · ([F]/C50,F) / (1 + [F]/C50,F),     C50,F = k_off,F/k_on,F     (3)
```

Read (3) exactly as the mechanism: **the fast agonist occupies only the receptors the slow
ligand is not holding** `(1 − [BR])`, and within that free pool it sits at its own
equilibrium saturation. Buprenorphine `[BR]` still evolves by its slow ODE (2), with
`ρ_0 = 1 − [BR] − [FR]`. This is the clean, non-stiff reduced system:

- **slow ligands** (buprenorphine; also the long-residence antagonists naltrexone,
  nalmefene) each carry one ODE state (2);
- **fast ligands** (fentanyl and the other full agonists, naloxone) are algebraic (3),
  competing among themselves and with the free receptors for the non-slow pool
  `(1 − Σ_slow ρ)`. For several fast agonists *j*:

```
ρ_j = (1 − Σ_slow ρ) · φ_j / (1 + Σ_k φ_k),   φ_j = Ce_j / C50_j          (4)
```

The `φ_j = Ce_j/C50_j` are exactly the additive-`U` terms, so when **no slow ligand is
present** the fast pool reduces to the equilibrium occupancy and, at low occupancy,
`Σρ_j ≈ Σ Ce_j/C50_j = U`. **Backward-compatible** (§6.1).

### 2.2 Net μ activation and effect (Olofsen's linear transduction)

Effect is the efficacy-weighted sum of bound-receptor fractions — for ventilation,
Olofsen 2022's linear transduction:

```
VE = VB · (1 − S),     S = Σ_i α_i ρ_i                                     (5)
```

where `α_i` combines intrinsic activity and receptor reserve: `α = 1` for a full agonist
(apnea at infinite concentration), `α < 1` for buprenorphine (the ceiling), `α = 0` for a
pure antagonist (naloxone occupies receptors but contributes nothing to `S`). `S` is the
single scalar that answers §1's four questions:

- **ceiling** — buprenorphine alone cannot push `S` past `α_bup < 1`, for any dose;
- **blockade** — an added agonist reaches only the free pool `(1 − [BR])` in (3);
- **precipitated withdrawal** — introduce buprenorphine onto a full-agonist background:
  `[BR]` ratchets up (slow `k_on`, then held by slow `k_off`), evicting the agonist from
  (3), and because `α_bup < α_full`, `S` **falls** — a downward step in `S(t)`;
- **breakthrough analgesia** — to raise `S` over a buprenorphine background the agonist must
  supply enough `φ` to fill `(1 − [BR])`; the model quantifies how much.

### 2.3 Naloxone and fentanyl are the same engine with different partners

Yassen 2007 fitted (2) for buprenorphine + **naloxone** (antagonist, `α = 0`, fast `k_on`);
Olofsen 2022 fitted it for buprenorphine + **fentanyl** (full agonist, `α = 1`, fast via (3)).
Same equations, same slow buprenorphine ODE, different partner. So the model unifies the
naloxone competition already coded in `R/drugs_naloxone.R`, the occupancy band in
`R/drugs_buprenorphine.R`, and the full-agonist interaction, into one receptor layer.

---

## 3. Tolerance is measured here, not assumed

Olofsen 2022 fitted two populations — **opioid-naive** and **chronic high-dose opioid use**
— simultaneously, and tolerance appears as a shift in three receptor parameters at once, not
as a single potency multiplier:

| Parameter | Opioid-naive | Chronic opioid use | Shift |
|---|---|---|---|
| k_on,B (buprenorphine association) | 0.26 | 0.085 mL·ng⁻¹·min⁻¹ | ↓ ~3× |
| α_B (buprenorphine activity + reserve) | 0.81 | 0.48 | ↓ (deeper ceiling) |
| C50,F (fentanyl potency) | 0.68 | 2.23 ng/mL | ↑ 3.3× (desensitised) |
| k_off,B | 0.019 | 0.019 min⁻¹ | unchanged (residence time is intrinsic) |

Two things worth noting. First, `k_off,B` does **not** move: buprenorphine's residence time
is a property of the molecule–receptor bond, not of the patient's tolerance — so blockade is
just as durable in the tolerant patient, even as sensitivity falls. Second, this is a far
richer, measured basis for the Patient Profile's **Opioid tolerance** field
(`OPIOID_TOLERANCE_FACTORS`, today a flat MEAC multiplier that cannot even touch buprenorphine)
than a single number: in this model "tolerance" selects a parameter set, and it moves
buprenorphine and the full agonist in the directions the data show. Algera 2021 (fentanyl
C50 0.42→1.82 ng/mL naive→chronic) and the developing-tolerance `pdAdaptation` panel are the
time-course companions to this static two-population split.

A deliberate caution: Olofsen fixed `α_F = 1` with no variability, i.e. apnea occurs at
infinite fentanyl concentration in both populations. Tolerance in this model raises the
*concentration* needed (C50,F) and blunts buprenorphine; it does not give the full agonist
a ceiling. That is the respiratory-safety asymmetry — buprenorphine has a ceiling, fentanyl
does not — and it is the model's central safety message.

---

## 4. Outputs the app should draw

1. **Receptor occupancy panel** (stacked area): `ρ_bup`, each full agonist's `ρ`, antagonist
   `ρ`, and free `ρ_0`. Shows blockade and displacement directly — buprenorphine holding 80%
   of the receptor while an added agonist reaches only the free remainder, and the slow
   crossover when one displaces the other.
2. **Net μ activation `S` / effect** (ventilation via (5); analgesia via §5): the honest
   replacement for the additive total-opioid line whenever a partial agonist or antagonist
   is present.
3. **Precipitated-withdrawal flag**: a marked downward excursion of `S` within ~an hour of
   introducing buprenorphine onto a full-agonist background — the one dynamic signature
   clinicians most need warned about, and one only the kinetic model produces.
4. **Breakthrough headroom**: on a buprenorphine maintenance background, the full-agonist
   `Ce` (hence dose) needed to lift effect to a target.

The existing % MEAC, propofol–opioid and opioid–MAC panels keep using `U` when no partial
agonist/antagonist is present (unchanged), and switch to `S` when one is.

---

## 5. Parameters — what is in hand, what must be found

| Quantity | Status | Source / route |
|---|---|---|
| Buprenorphine k_on, k_off, α, ke0 — **ventilation**, two populations | **published, fitted** | Olofsen 2022 Table 3 |
| Buprenorphine k_on, k_off, K_D, α, ke0 — **antinociception** | **published, fitted** | Yassen 2006 Table 2 |
| Buprenorphine + naloxone competitive kinetics | **published, fitted** | Yassen 2007 Table IV |
| Fentanyl C50 (ventilation), ke0, two populations | **published, fitted** | Olofsen 2022 Table 3 |
| Buprenorphine occupancy vs plasma C (independent check on K_D) | **published** | Nasser 2014 |
| Other full agonists' C50 (ventilation) / analgesia | **partly** | fit where published; else calibrate C50 ∝ MEAC so (4)→(1) at low occupancy (§6.1) |
| Other full agonists' α (≈ 1) | **assume 1** | Olofsen fixed α_F = 1; refine only where a ceiling is published |
| ke0 per ligand | **mostly in library** | the drugs already carry ke0/tPeak |
| Slow antagonists (naltrexone, nalmefene) k_off | **literature** | binding-kinetics papers; default to the naloxone/bup template |

Two anchors keep it disciplined: calibrate each fast agonist's `C50` to its MEAC so the model
reduces to the current `U` when buprenorphine/naloxone are absent (§6.1); and check
buprenorphine's `k_off/k_on` against Nasser 2014's occupancy curve (an independent measurement
of the same equilibrium). Fits that are stanpumpR's rather than a source's are flagged
`pending_verification` and not drawn until signed.

**Endpoint choice.** The two fitted human datasets are *ventilation* (Olofsen 2022, Yassen 2007)
and *experimental-pain antinociception* (Yassen 2006). Ventilation is the safety endpoint and
the one with the interaction data, so it is the natural first transduction; analgesia is the
pain-medicine endpoint and has its own buprenorphine kinetics (faster k_off, ~9 min, Yassen 2006)
and reserve. The receptor core (2)–(4) is shared; only `α` and the transduction differ by
endpoint. Draw both where the drugs present allow.

---

## 6. Compatibility, scope, validation

### 6.1 It must not move the full-agonist world

With `C50` calibrated to MEAC, for pure full agonists at the occupancies clinical anaesthesia
reaches, `Σρ_j` from (4) and `U` from (1) agree to the order of the occupancy. A regression test
pins `|S − U|/U` below a stated tolerance across the MEAC-panel scenarios; the propofol–opioid and
opioid–MAC panels read `U` unchanged unless a partial agonist/antagonist is in the table.
**Turning the model on changes nothing until buprenorphine or naloxone is present** — the
non-negotiable acceptance criterion.

### 6.2 Scope — what the μ model deliberately excludes (Lutfy & Cowan 2004)

Lutfy & Cowan's review maps buprenorphine's full pharmacology and marks the edges of a μ-only model:

- **μ partial-agonist ceiling** — *in*, as `α < 1` (5).
- **Bell-shaped (biphasic) dose–response** — the antinociceptive effect *declining* at very high
  dose — is **not** a μ phenomenon; it is ORL-1/NOP receptor activation. It is seen chiefly in
  rodent antinociception; Olofsen 2022 found human ventilation **monotone** (no bell shape), so a
  monotone competitive-μ model is adequate for stanpumpR's human endpoints. ORL-1 is **out of scope**
  and documented as such.
- **κ/δ antagonism, and norbuprenorphine** (μ and ORL-1 active, not even a PK species in the library
  yet) — **out of scope**, flagged as future work.

One receptor pool, μ only, is the stated boundary.

### 6.3 Validation plan

1. **Reduction to U.** Pure full agonists → `Σρ` matches `U` within tolerance (§6.1). Test.
2. **Olofsen 2022 (keystone).** Reproduce the fitted ventilation curves for buprenorphine + escalating
   fentanyl in both populations, including the protection at ≥ 2 ng/mL buprenorphine against high-dose
   fentanyl in chronic users, and the probability-of-apnea behaviour. This is a *kinetic competition*
   dataset an equilibrium model cannot reproduce — so passing it is the proof the kinetics are necessary.
3. **Yassen 2007.** Reproduce buprenorphine + naloxone reversal of ventilation, including the bell-shaped
   loss of reversal above ~4 mg/70 kg/h — the slow-k_off signature.
4. **Ceiling.** Buprenorphine dose–ventilation plateau from `α_bup < 1`.
5. **Occupancy.** `ρ_bup(C)` reproduces Nasser 2014's ~70%-at-2-ng/mL.
6. **Precipitated withdrawal, timing.** Buprenorphine given near a fentanyl peak drops `S` sharply;
   given after fentanyl has declined, it does not — the induction-protocol timing dependence, which only
   the kinetic form produces.

---

## 7. How it sits in the codebase

- **New PD layer**, parallel to `R/antiseizureInteractions.R` (a PK interaction engine) but acting on
  effect. Reads the simulated `Ce_i(t)` of the μ ligands and produces `ρ_i(t)`, `S(t)`, and the endpoint
  transductions.
- **Integrator.** The slow buprenorphine ODE (2) with the fast ligands algebraic (3)–(4): a 1–2 state
  non-stiff system solved along the existing grid, `Ce` piecewise-linear between points — the
  `pdAdaptation` post-process pattern. (If more than one slow ligand is present, one state each.)
- **Per-drug descriptors** (`k_on`, `k_off`, `α`, `ke0`, per endpoint) live on the drug functions as PK
  parameters do; a registry marks μ ligands and carries their constants. Buprenorphine, fentanyl and
  naloxone are the first citizens, from Olofsen 2022 / Yassen 2006–2007; other full agonists get `C50`
  by the MEAC calibration and `α ≈ 1`.
- **Panels** under one *Buprenorphine / opioid-receptor interaction* graph option, offered only while a
  partial agonist or antagonist is drawn.
- **Reuse, don't duplicate.** Naloxone's competition and buprenorphine's occupancy band become instances
  of this one model.

---

## 8. Open questions for Steven

1. **Lead endpoint.** Ventilation (the safety endpoint, and where the interaction data are — Olofsen 2022,
   Yassen 2007) as the first transduction, analgesia (Yassen 2006) second? Or analgesia first for the pain
   audience? The receptor core is shared either way.
2. **Full-agonist C50 scale.** Calibrate to MEAC (keeps every existing panel still — my recommendation),
   or to published ventilatory C50 where they exist (more mechanistic, but then `S ≠ U` and the full-agonist
   panels move and must be re-defended)?
3. **Tolerance.** Make the Patient Profile **Opioid tolerance** field select the Olofsen population set
   (naive vs chronic), i.e. move `k_on,B`, `α_B`, `C50,F` together, replacing the flat MEAC multiplier — or
   keep the multiplier and add this only for buprenorphine?
4. **Does buprenorphine re-enter the `U`-based panels**, or only the new occupancy panels? Proposed: the new
   panels own buprenorphine; the `U` panels switch to `S` only when a partial agonist/antagonist is present.
5. **norbuprenorphine** — model its formation as a PK species (it has μ and ORL-1 activity and bears on the
   respiratory story), or leave it out of scope for now?
6. **Ligand scope.** μ only to start (opioids + naloxone). Naltrexone/nalmefene as slow antagonists are a
   clean next addition on the same engine; κ/δ/ORL-1 are excluded.

---

### Sources (to be digested into `papers/digests`)

**Olofsen E, Algera MH, Moss L, Dobbins RL, Groeneveld GJ, van Velzen M, Niesters M, Dahan A, Laffont CM.
Modeling buprenorphine reduction of fentanyl-induced respiratory depression. JCI Insight 2022;7(9):e156973.**
(CC-BY 4.0 — the fitted buprenorphine–fentanyl competitive-kinetic model; §2–3, §5, §6.3 rest on it.)
· **Lutfy K, Cowan A. Buprenorphine: A Unique Drug with Complex Pharmacology. Curr Neuropharmacol 2004;2(4):395–402.**
(the mechanistic map and the §6.2 scope boundary: ceiling, bell-shape/ORL-1, κ/δ.)
· Yassen 2007 (buprenorphine–naloxone competitive ventilation, PMID 17922561, already digested) · Yassen 2006
(antinociception receptor kinetics, PMID 16732095, digested) · Nasser 2014 (occupancy vs plasma, PMID 25015174,
digested) · Björnsson 2023 (absolute-scale PK, PMID 37584841, in the library) · Algera 2021 (naive vs chronic
ventilatory C50, PMID 32865832). Copeland 2006 / Tummino & Copeland 2008 for the drug–target residence-time
framework in §1.1.
