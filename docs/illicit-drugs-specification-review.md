# Review of the illicit-drug specification, and what was implemented

Claude Code, 2026-10-17, at the request of Steven L. Shafer.

The specification *Build evidence-gated illicit drug PK/PD models* (16 October 2026,
written with ChatGPT) proposed source-versioned plasma models for cocaine,
d-methamphetamine, MDMA, Δ9-THC, LSD and psilocybin, plus route layers on the existing
diamorphine, fentanyl and ketamine entries. This file records what was checked, what
survived the check, and what was deliberately not built.

Every parameter below was re-checked against its primary source through PubMed and
PubMed Central before it was coded. Where a published **table** could not be read
directly — several of these papers are paywalled and this container reaches only
abstracts and the open-access full texts — the file says so, and the test suite pins the
paper's own published summaries (half-life, peak time, peak concentration) rather than
resting on the table cells alone.

---

## 1. What the specification got right

The following were confirmed verbatim against the primary sources and are used as the
specification gave them:

| Source | Confirmed |
|---|---|
| Chow 1985, cocaine | Two-compartment fit in five adults; steady-state volume 131.8 L (1.96 L/kg); clearance 2.10 L/min; terminal half-life 48 min |
| Javaid 1983, cocaine | One-compartment intravenous fit, half-life 41.4 ± 8.2 min (SEM, n = 4) |
| Jeffcoat 1989, cocaine | Intranasal absorption half-time 11.7 min, smoked 1.1 min, intravenous distribution half-life 11 min, terminal 78 min, intranasal bioavailability about 80% |
| Coe 2018, cocaine | Oral bioavailability 0.32 ± 0.04 at 100 mg and 0.45 ± 0.06 at 200 mg, and they are SEM |
| Fattinger 2000, cocaine | Mucosal fraction 19% (95% CI 11-26), contributing 31% of exposure (23-37); oral median bioavailability 33%; input peaks near 10 and 45 min |
| Harris 2003, methamphetamine | Clearance 272 mL/h/kg, steady-state volume 4.2 L/kg, mean residence time 16 h, intravenous half-life 11.4 h, intranasal bioavailability 79% with the peak at 2.7 h, smoked 67% of delivered or 37.4% of pipe load |
| Huestis 2025, MDMA | CL/F 41.5 L/h (37.7-45.8), V/F 462 L (429-496), ka 1.17 /h, ALAG1 0.322 h, D1 0.304 h, MDA formation fraction **0.1 FIXED**, and the fed/fasted comparison |
| The smoked cannabis model (*Br J Clin Pharmacol* 2020;86(7):1334-1345) | Steady-state volume 3401 L and clearance 0.72 L/min for THC, 50% of THC clearance going to 11-OH-THC, 11-OH-THC volume 415.2 L, THC-COOH volume 29.1 L and clearance 0.12 L/min, six participants, plasma, 168 h |

The specification's central design argument — that a parameter set belongs to an analyte,
a dose basis, a route, a formulation, a matrix and a source rather than to a drug name —
is sound, and it is what the implementation is built around.

## 2. Errors found in the specification

**Its description of the repository is from an older commit.** It quotes ketamine's
micro-constants as `k10 0.438, k12 0.592, k13 0.590, k21 0.247, k31 0.0146`. Those were
replaced on master on 2026-10-14; the current values are `k10 0.2243386, k12 0.8702122,
k13 0.3291103, k21 0.4417174, k31 0.01373109`. It also quotes ketamine's `tPeak` comment
as "Just a guess", which now reads "Assumed, not from Domino (who measured no effect)".
The fentanyl parameters it quotes are current and correct.

**There is no rectal route in this engine.** The specification refers to "limited second
oral/rectal depots". `RA` in stanpumpR is **regional anesthesia** — a local anesthetic
injected into tissue — not rectal administration. The second oral depot is `ka_PO2`; the
slow regional depot is `ka_RA_slow`. It also names the constant `tPeakRoute`, which is
`TPEAK_ROUTES`.

**A misattributed citation.** The specification credits methamphetamine's oral
bioavailability (0.672 ± 0.031) and the fast/slow absorption model to Cook's 1992 oral
paper (PMID 1362938). Both are in Cook's **1993 smoking** paper (PMID 8104133), and the
oral figure there is explicitly a calculation across that study and the 1992 one, not a
result of either alone. The 1992 paper's own contribution is the structural one: a
one-compartment model with a lag time fits oral methamphetamine best.

**A question it left open is actually settled.** It asks whether MDMA's apparent CL/F and
V/F were fitted to hydrochloride salt mass or base mass, and makes that a release gate.
The paper doses MDMA hydrochloride throughout ("120 mg MDMA HCl", "a single oral dose of
100 mg MDMA HCl"), so the apparent parameters are on the **hydrochloride salt mass**
basis. No rescaling is needed if the dose is entered as salt.

**Its MDMA absorption question resolves two-to-one.** The specification is right that the
paper contradicts itself. The Methods list "sequential zero-order, followed by
first-order" among the models evaluated, the Results say "MDMA had a parallel first- and
zero-order absorption model", and the Discussion says "sequential zero- and first-order
absorption". Two of three say sequential, and so does the reported parameter set: it has
ALAG1, D1 and ka but **no mixing fraction**, which a parallel model would require.

**It does not mention that its LSD source is an outlier.** Dolder 2017's half-life of
2.6 h is shorter than the same group's later measurements: Holze 2024 (85 and 170 µg,
n = 28) reports 3.7 and 4.0 h, and the 2025 crossover 3.8 h. A model built on CL/F
12.3 L/h and V/F 46 L therefore clears LSD roughly 45% faster than the later data. This
is now stated in the drug file, the help page and the registry.

## 3. Numbers that could not be traced to a primary source

These were **not coded**, and should not be without reading the paper:

- **Mendelson 2006 d-methamphetamine clearances 0.257 and 0.259 L/h/kg.** Not in the
  abstract and not traceable. Suspiciously close to Harris's 0.272 L/h/kg from the same
  research group.
- **Cook's fast and slow absorption rate constants, 36.9 ± 7.5 and 0.693 ± 0.102 /h.**
  Only the existence of a two-input model in four of six subjects is confirmable. Note
  that 0.693 /h is exactly ln 2, i.e. a one-hour half-time, which may be a transcription
  rather than a fitted value.
- **"21.8 mg methamphetamine HCl = 17.5 mg base."** The 21.8 mg is confirmed as the mean
  inhaled dose; the arithmetic of the conversion checks out, but no source states it.
- **Madsen's subjective-intensity EC50 of 4.5 ng/mL, and its R² of 0.35.** The EC50 traces
  only to secondary sources; no source at all was found for the R².
- **Madsen's occupancy EC50 lower confidence limit of 1.17 ng/mL**; primary-derived
  snippets give 1.16.
- **Brown 2017's psilocin V/F of 298 L** is contradicted by a 2025 systematic review,
  which gives a range of 505-1267 L across oral studies. Its glucuronide rate constants
  were inferred from the shape of the psilocin curve, with no measured glucuronide data.
- **THC's smoked bioavailability "fixed at 0.25 as an identifying assumption."** Plausible
  and important if true, since every absolute volume and clearance in that model would be
  conditional on it, but the full text could not be read here.
- **The salt basis of the doses in Chow 1985, Coe 2018 and Harris 2003.** Not stated in
  any accessible text. For Harris this largely cancels, because bioavailability is
  measured against an intravenous dose of the same labelled material.

One further caution, not an error in the specification: Dolder's peak concentrations of
1.3 and 3.1 ng/mL are **model-predicted** geometric means, and the paper says its model
predicts lower peaks than were observed. They are not observed values to validate against.

## 4. What was implemented

Four new drugs, all in the opt-in `Illicit drugs` category, all plasma only (`tPeak` 0,
`MEAC` 0, no band), each offering **only** the routes whose input model the evidence
identifies:

| Drug | Analyte | Route offered | Parameters |
|---|---|---|---|
| cocaine | parent cocaine | intranasal | Chow's disposition as one compartment, Jeffcoat's absorption half-time 11.7 min and bioavailability 0.80 |
| methamphetamine | parent d-methamphetamine | intranasal | Harris's clearance and steady-state volume as one compartment, bioavailability 0.79, absorption rate derived from his observed 2.7 h peak |
| LSD | parent LSD | oral | Dolder's 100 µg arm: ka 1.4 /h, CL/F 12.3 L/h, V/F 46 L, no lag |
| psilocybin | free plasma psilocin | oral | Holze's nominal 15 mg arm: ka 0.89 /h, lag 0.30 h, CL/F 172 L/h, V/F 448 L, rescaled from a psilocin-equivalent to a psilocybin dose basis |

Each reproduces its source's published summaries, and the tests pin that rather than only
the parameters: cocaine's half-life falls between Javaid's 41.4 min and Chow's 48 min;
methamphetamine's is 10.70 h against Harris's 10.7-11.4 h, with the intranasal peak at
2.7 h; LSD's is 2.59 h with a peak at 1.46 h against the published 2.6 h and 1.4 h;
psilocybin's is 1.81 h with a peak at 1.96 h and 11.6 ng/mL for the 15 mg arm's assayed
content, against the published 1.8 h, "an average of 2 hours" and 11 ng/mL (95% CI 10-13).

Alongside them, `inst/extdata/illicitRouteRegistry.csv` records **every** route these five
drugs (including diamorphine) have in the literature, offered or not, with its
`route_status`, `parameter_status`, analyte, matrix, both dose bases, population and
source. `test-illicit-registry.R` enforces the rule the specification asked for: a route
may carry dose units only if its row says the input model is identified, and a route the
engine cannot represent is recorded and refused rather than aliased to another.

## 5. What was deliberately not built, and why

- **MDMA.** The published fit's input is zero-order into the depot followed by first-order
  absorption, which the closed-form engine cannot represent, and MDMA inhibits its own
  CYP2D6 metabolism, so a linear model is confined to the single-dose calibration. Its
  dose basis is now settled (hydrochloride), so what remains is engine work.
- **Δ9-THC.** The implementable model is a three-compartment fit to **smoked** cannabis
  with two metabolite levels. stanpumpR has neither a pulmonary route nor two metabolite
  levels, and a route that cannot be represented must not be aliased to the intranasal
  one.
- **GHB.** Non-linear absorption and elimination, with no identified human parameter set
  covering route, volume and the non-linear terms together. It needs a non-linear engine
  first.
- **Diamorphine's full parent → 6-MAM → morphine cascade**, the fentanyl product route
  layer and the ketamine parent-norketamine extension. All three need the engine work the
  specification calls Stage 2 (more routes, multiple simultaneous input paths, two
  metabolite levels), not new parameters.
- **Intravenous routes for all four new drugs.** For LSD and psilocybin because apparent
  parameters cannot cross to the intravenous route; for cocaine and methamphetamine
  because a one-compartment model whose volume is a steady-state volume misstates the
  first minutes after a bolus.
- **Every pharmacodynamic end point.** Chow's cocaine biophase needs a first-order
  attenuation term the single-ke0 effect site cannot carry; methamphetamine's effects lead
  plasma by hours; Dolder's LSD fits are specific to one rating scale and one dose arm;
  psilocin occupancy is a direct function of plasma, not an equilibration delay. The
  published values are described on each help page but none is plotted.
