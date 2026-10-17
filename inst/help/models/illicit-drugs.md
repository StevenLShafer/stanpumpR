## Research models of drugs of abuse

A few drugs in the library are **research models of drugs of abuse**. They are hidden until you turn on **Show illicit drugs** above the dose table, which is off by default and remembered between sessions, and when shown their names appear in red. A fresh session never opens with one, and they are never offered in the startup menu.

Every one of them plots a *modelled plasma concentration* for research and teaching. None of them is dosing advice, a safety threshold, an impairment or intoxication prediction, or a prediction for an individual. The published studies behind them used pharmaceutical material of known content in small groups of volunteers. Purity, adulterants, delivery losses, tolerance, interactions and disease routinely matter more, in the real world, than anything a concentration curve shows.

### Why these models are plasma only

Each of these drugs is plotted as plasma concentration alone: no effect site, no MEAC, no concentration band and no time until threshold. That is deliberate. For some the effect is carried by metabolites this engine does not resolve; for others the measured responses lead or lag plasma in ways one equilibration constant cannot represent, or the published effect fits are specific to one rating scale and one dose arm. Where a source *did* publish an effect model, each drug's own page says what it was and why it is not plotted.

### The route is the model, not the drug

The most important thing these pages are trying to teach is that a parameter set belongs to an **analyte, a dose basis, a route, a formulation, a matrix and a source** — not to a drug name. The same drug can have a matched population fit for one route, a bioavailability with no absorption model for the next, and nothing but case reports for a third. Substituting one for another is how a simulator comes to produce a confident number with no evidence behind it.

So stanpumpR offers dose units **only** for a route whose input model is identified, and records, beside each route it refuses, exactly what is known and what is missing. That record is `inst/extdata/illicitRouteRegistry.csv`, and `test-illicit-registry.R` enforces it: a route that is not offered cannot acquire units without its registry row changing too.

Each row carries a **route status**:

- **calibrated** — a matched quantitative human fit for that route.
- **anchored** — a human numeric bioavailability or time-course anchor, but not a complete input fit.
- **observed** — human exposure reported, with no transferable quantitative parameters.
- **unsupported** — no suitable human calibration in the sources used.

and a **parameter status**: *published fit*, *measured summary*, *derived approximation* or *not identified*.

### What the library offers today

| Drug | Analyte plotted | Routes offered | Routes recorded but not offered |
|---|---|---|---|
| [diamorphine](help:drugs/diamorphine) | parent diamorphine | intramuscular, intranasal | intravenous, smoked, oral |
| [cocaine](help:drugs/cocaine) | parent cocaine | intranasal | intravenous, oral, smoked |
| [methamphetamine](help:drugs/methamphetamine) | parent d-methamphetamine | intranasal | intravenous, oral, smoked |
| [LSD](help:drugs/LSD) | parent LSD | oral | intravenous |
| [psilocybin](help:drugs/psilocybin) | free plasma psilocin | oral | intravenous |

### Three reasons a route is refused

**Apparent parameters cannot cross to the intravenous route.** LSD and psilocybin are fitted as clearance and volume *over an unmeasured bioavailability*. Those predict oral concentrations correctly, because the unknown factor cancels, and intravenous ones wrong by 1/F. Both are oral only.

**A one-compartment reduction cannot start at the needle.** Cocaine's and methamphetamine's disposition parameters come from intravenous studies that published summaries — a steady-state volume, a clearance, a terminal half-life — rather than micro-constants. Used as a one-compartment model, those reproduce an absorption-limited curve well and the first minutes after an intravenous bolus badly, because the true central volume is much smaller than the steady-state volume. So the intranasal route is offered and the intravenous route is not.

**Smoke is not an intranasal dose.** stanpumpR has no pulmonary route. Adding one is not a matter of relabelling: how much drug survives heating depends on the device and the technique, which is why smoked methamphetamine has been reported three ways — 67% of the *delivered* dose, 37.4% of the nominal *pipe load*, 90.3% of the *inhaled* dose. Three denominators, never to be multiplied together. Until a pulmonary input exists, smoked routes stay recorded and unoffered.

### Drugs that are not here, and why

Models for several further drugs were specified but not implemented, each for a stated reason rather than an oversight:

- **MDMA.** The published population model (Huestis MA, Smith WB, Leonowens C, et al. *CPT Pharmacometrics Syst Pharmacol* 2025;14(2):376-388) is a one-compartment fit with a **zero-order input followed by first-order absorption**, which the closed-form engine cannot represent, and MDMA inhibits its own CYP2D6 metabolism, so a linear model is confined to the single-dose calibration. Its dose basis is settled: the paper doses MDMA hydrochloride throughout.
- **Δ9-THC.** The directly implementable model is a three-compartment fit to *smoked* cannabis with its parent and two metabolites, and stanpumpR has neither a pulmonary route nor two metabolite levels. Its absolute bioavailability was also fixed as an identifying assumption, so every absolute volume and clearance in it is conditional on that assumption.
- **GHB.** Absorption and elimination are both non-linear across ordinary doses, and no identified human parameter set covering a route, a volume and the non-linear terms together was found. A non-linear engine would be needed first.

### Reading these curves

Read them as exposure, within the studied population, route, formulation and dose range, and nothing else. Where a number in a source is an anchor rather than a model — a bioavailability with no absorption rate, a peak time with no rate constant — stanpumpR's rule is to record it and refuse the route, rather than to fill the gap with an assumption that would look exactly like evidence on the plot.
