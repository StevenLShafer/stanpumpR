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
| The smoked cannabis model (Sempio 2020, *Br J Clin Pharmacol* 86(3):611-619) | Steady-state volume 3401 L and clearance 0.72 L/min for THC, 50% of THC clearance going to 11-OH-THC, 11-OH-THC volume 415.2 L, THC-COOH volume 29.1 L and clearance 0.12 L/min, six participants, plasma, 168 h |

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
basis. No rescaling is needed if the dose is entered as salt. The implementation takes its
dose that way.

**Its MDMA absorption question resolves two-to-one.** The specification is right that the
paper contradicts itself. The Methods list "sequential zero-order, followed by
first-order" among the models evaluated, the Results say "MDMA had a parallel first- and
zero-order absorption model", and the Discussion says "sequential zero- and first-order
absorption". Two of three say sequential, and so does the reported parameter set: it has
ALAG1, D1 and ka but **no mixing fraction**, which a parallel model would require. The
sequential reading is the one implemented.

**Its engine objection to MDMA was right but not decisive.** The specification treats the
zero-order depot filling as a blocker, and it is true that no first-order depot reproduces
it. But the parameters are all published, so only the arithmetic needed reorganising: the
dose is split across the input window and absorbed through the two depots the engine
already has. Measured against a numerical evaluation of the exact solution, two half doses
at the quarter points reproduce the peak to 0.01% and the whole curve to 0.53%, against a
clearance confidence interval of about ±10%. A blocker worth testing before accepting.

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

Five new drugs, all in the opt-in `Illicit drugs` category, all plasma only (`tPeak` 0,
`MEAC` 0, no band), each offering **only** the routes whose input model the evidence
identifies. The first four shipped together; MDMA followed, on 2026-10-17, once a way to
carry its input exactly enough had been measured rather than assumed.

| Drug | Analyte | Route offered | Parameters |
|---|---|---|---|
| cocaine | parent cocaine | intranasal | Chow's disposition as one compartment, Jeffcoat's absorption half-time 11.7 min and bioavailability 0.80 |
| methamphetamine | parent d-methamphetamine | intranasal | Harris's clearance and steady-state volume as one compartment, bioavailability 0.79, absorption rate derived from his observed 2.7 h peak |
| LSD | parent LSD | oral | Dolder's 100 µg arm: ka 1.4 /h, CL/F 12.3 L/h, V/F 46 L, no lag |
| psilocybin | free plasma psilocin | oral | Holze's nominal 15 mg arm: ka 0.89 /h, lag 0.30 h, CL/F 172 L/h, V/F 448 L, rescaled from a psilocin-equivalent to a psilocybin dose basis |
| MDMA | parent MDMA | oral | Huestis's fit on a hydrochloride dose basis: CL/F 41.5 L/h, V/F 462 L, ka 1.17 /h, with the published zero-order depot filling (lag 0.322 h, duration 0.304 h) discretised as two lagged half doses |

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

- **Δ9-THC.** Still not built. The reason has changed: it is no longer one
  missing route, it is that the published evidence splits into models that fix THC's
  disposition on a route stanpumpR does not have, and a model on the route it does have
  that cannot fix the disposition. A full survey of the population models was done on
  2026-10-17 and is recorded here so it does not have to be repeated.

  | Source | Route(s) | Structure | What blocks it |
  |---|---|---|---|
  | Heuberger 2015, *Clin Pharmacokinet* 54(2):209-219 ([10.1007/s40262-014-0195-5](https://doi.org/10.1007/s40262-014-0195-5)) | oral + intravenous + pulmonary, one fit | three compartments, absolute, clearance 38.8 L/h, apparent terminal half-life 21.5 h, built to characterise the long terminal phase | **Nothing but access.** This is the model the library wants: three compartments is what the closed form has, and an absolute parameter set fitted across oral and intravenous doses would let both routes be offered. Springer is unreachable from this container and the parameter table is not in the abstract. |
  | Li & An 2025, *J Clin Pharmacol* 65(5):535-549 ([10.1002/jcph.6181](https://doi.org/10.1002/jcph.6181)) | oral (with three intravenous studies in the fit) | **four** compartments for the parent: central, two peripherals, and an adipose pool off the second peripheral through a permeability-surface-area product (5.5 L/h) and a partition coefficient (14); two further levels for 11-OH-THC and THC-COOH | Structure and identifiability. Four compartments is one more than the closed form. The central volume is 204 L with an **RSE of 186%** — the source could not estimate it — while every other parameter in the table carries an RSE under 9% and most under 1%, which is the signature of a fit to digitised *mean* curves rather than to subject data (the paper says so). The implied terminal half-life is 203 h against a literature value of 21.5 h, and the paper concedes it "could not fully capture the accumulation characteristics of THC". |
  | Marsot 2017, *J Pharmacol Toxicol Methods* 85:49-54 ([10.1016/j.vascn.2017.02.003](https://doi.org/10.1016/j.vascn.2017.02.003)) | pulmonary (smoked) | three compartments in micro-constants, central volume 19.1 L (39.7% RSE); the abstract prints five rate constants — 0.408, 4.070, 0.022, 1.070 and 1.060 /h — but the retrievable text strips their subscripts, so which is which is an inference from the order, not something read | Route, and a dose denominator. Subjects smoked *ad libitum*, so every parameter is apparent on an unmeasured pulmonary bioavailability. No pulmonary route exists, and smoke must never be aliased to the intranasal route. |
  | Sempio 2020, *Br J Clin Pharmacol* 86(3):611-619 ([10.1111/bcp.14170](https://doi.org/10.1111/bcp.14170)) | pulmonary (smoked) | three compartments with both metabolites, steady-state volume 3,401 L, clearance 0.72 L/min, 6 subjects, 168 h | Route, as above, plus two metabolite levels the engine does not carry. |
  | Alvarez 2021, *Br J Clin Pharmacol* 87(8):3139-3149 ([10.1111/bcp.14724](https://doi.org/10.1111/bcp.14724)) | pulmonary (smoked) | three compartments with **zero-order** absorption, two-compartment metabolites, 15 chronic and 15 occasional users | Route, as above. Wiley is unreachable here in any case. |
  | Storgaard 2025, *Br J Clin Pharmacol* 92(2):504-514 ([10.1002/bcp.70284](https://doi.org/10.1002/bcp.70284)) | oromucosal (Sativex) | one compartment with **three transit compartments**, apparent: V/F 1,669 L, total CL/F 927 L/h | Not the drug's disposition. Sampling stopped 4 h after the last dose, so the terminal phase was never observed; the authors say the apparent clearance reflects distribution still in progress, the fit is unstable (33.8% of bootstrap runs converged), and the input is an Erlang transit chain the engine does not have. |
  | Wolowich 2019, *Eur J Drug Metab Pharmacokinet* 44:691-711 ([10.1007/s13318-019-00559-7](https://doi.org/10.1007/s13318-019-00559-7)) | intravenous + oral | minimal PBPK: hepatic extraction, liver blood flow, a hepatic diffusional barrier, CYP2C9 genotype | Not a mammillary model, so not reducible to the engine without refitting it, which would make the numbers this library's rather than the source's. Springer, so unreachable too. |

  Two things follow, and both are worth writing down.

  First, **the spread, and how much of it is real.** Three of these models report
  *apparent* parameters on an unmeasured pulmonary bioavailability (Marsot, Sempio,
  Alvarez) and one on an unmeasured oromucosal one (Storgaard), so their volumes and
  clearances cannot be set beside an absolute number without knowing the F they hide:
  Sempio's 3,401 L and Marsot's implied 3,572 L are Vss/F, not Vss. Stripping those out
  leaves exactly two absolute parameter sets, and they still disagree badly — Heuberger
  at a clearance of 38.8 L/h on a steady-state volume of 236 L, against Li & An at 87 L/h
  on 1,608 L (counting the adipose pool at its partition coefficient). A factor of 2.2 in
  clearance and 6.8 in volume. So there is no consensus parameter set to fall back on, and
  no way to assemble one: taking absorption from the oral paper and disposition from a
  smoked one would be an invention with a citation attached, which is the single thing
  this scheme exists to prevent.

  Second, **Li & An was tested before it was refused**, so the refusal is not an argument
  from the abstract. Both readings of its topology (the adipose pool off the second
  peripheral, as the figure legend describes, and off the central compartment, as one
  sentence in the results implies — the displayed differential equations are images and
  could not be read) were built and integrated numerically. They agree to within 5% through
  8 h and diverge by 60% at 24 h. The oral predictions are reasonable where the model was
  fitted: 10 mg orally gives a peak of 3.6 ng/mL at 1.06 h, which matches the published
  oral curves. The disposition behind them is not: the four rate constants are
  0.29, 1.09, 2.95 and 202.9 h in half-life, the slowest carrying 5.3% of the area, and a
  5 mg intravenous dose would start at 24.5 ng/mL where the intravenous literature implies
  a central volume several times smaller. A model may be offered with a documented caveat
  — LSD's half-life is shorter than later studies found, and cocaine's intranasal
  absorption is one path where deconvolution shows two — but not when the source itself
  reports that it could not estimate the parameter the offered route's first hour depends
  on.

  There is also a structural point about where THC could be *recorded*. Unlike the
  antiseizure, anticoagulant and antihypertensive registries, which carry unimplemented
  entries as an implementation queue, `illicitRouteRegistry.csv` is keyed to drugs the
  library offers: `test-illicit-registry.R` asserts that the registry covers *exactly* the
  illicit category and that every drug in it offers at least one route. So an unoffered THC
  has no row to live in, and this document and the help page are its record. That is the
  right boundary for a registry whose job is to justify the routes that *are* offered, and
  it should not be widened to make room for a drug with no curve.

  Supplying Heuberger 2015 would unblock a full implementation, with no new engine and no
  reduction: three compartments, fitted across the oral and intravenous routes the library
  already has.
- ~~**GHB.** Non-linear absorption and elimination, with no identified human parameter set
  covering route, volume and the non-linear terms together. It needs a non-linear engine
  first.~~ **Built, 2026-10-17.** Both halves of this were wrong by the time it was
  written. The non-linear engine already existed — `advanceMichaelisMenten.R` landed for
  phenytoin on 2026-10-10 — and it needed only an optional peripheral compartment, which
  leaves every other drug on it bit-for-bit unchanged. And a complete human parameter set
  does exist: Chen 2020 (*Clin Transl Sci* 13:932-940) fitted two compartments with
  Michaelis-Menten elimination to 3,775 observations from 174 people, and its Table S4
  gives Vmax, Km, the central volume, both distribution rate constants, the absorption
  rate constant and the allometric exponents. The table is in the supplement rather than
  the paper, which is why a search of abstracts found nothing.

  Two cautions the registry now carries. Bioavailability was *fixed* at 1 while the label
  reports 88%, so the parameters are apparent and the route is oral only. And the dose
  basis genuinely does differ from the plotted analyte — the source doses and reports
  sodium oxybate, the literature reports the free acid, 21% apart — so the unit names the
  salt and the mass-carrying parameters are rescaled exactly.

  What is still missing is the *saturable absorption* Palatini documented: Chen fitted a
  single first-order rate, so above about 4.5 g the model peaks earlier and higher than
  the real drug. That is recorded on the help page rather than patched.
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
