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
| [MDMA](help:drugs/MDMA) | parent MDMA | oral | intravenous, intranasal; the MDA metabolite |
| [GHB](help:drugs/GHB) | GHB free acid | oral | intravenous |

### The pharmacokinetics, side by side

These seven models differ from one another more than the rest of the library does: one compartment or two, linear elimination or saturable, a parent or a metabolite plotted, apparent parameters or absolute. The table below is built from the models themselves, at the reference adult (40 years, 70 kg, 170 cm, male) with *Adjust weight to fat-free mass* as the app ships it, so it cannot drift from the code. Each drug's own page gives the same parameters at six reference patients, with the source and its limits.

<!-- generated: illicit-pk-table -->

Two of these need reading carefully. **GHB's elimination saturates**, so it has no fixed clearance and no fixed half-life — both depend on the dose, which is the whole point of the drug, and its page says what the numbers in the table above mean. **Every one of these is plasma only**: no effect site, no MEAC and no band, so none of them plots an effect.

### Three reasons a route is refused

**Apparent parameters cannot cross to the intravenous route.** LSD, psilocybin and GHB are fitted as clearance (or a maximum metabolic rate) and volume *over an unmeasured bioavailability*. Those predict oral concentrations correctly, because the unknown factor cancels, and intravenous ones wrong by 1/F. All three are oral only. GHB's source fixed bioavailability at 1 while its label reports 88%, which is the same situation reached by a different route — and it holds even though GHB was once given intravenously as an anaesthetic.

**A one-compartment reduction cannot start at the needle.** Cocaine's and methamphetamine's disposition parameters come from intravenous studies that published summaries — a steady-state volume, a clearance, a terminal half-life — rather than micro-constants. Used as a one-compartment model, those reproduce an absorption-limited curve well and the first minutes after an intravenous bolus badly, because the true central volume is much smaller than the steady-state volume. So the intranasal route is offered and the intravenous route is not.

**Smoke is not an intranasal dose.** stanpumpR has no pulmonary route. Adding one is not a matter of relabelling: how much drug survives heating depends on the device and the technique, which is why smoked methamphetamine has been reported three ways — 67% of the *delivered* dose, 37.4% of the nominal *pipe load*, 90.3% of the *inhaled* dose. Three denominators, never to be multiplied together. Until a pulmonary input exists, smoked routes stay recorded and unoffered.

### Drugs that are not here, and why

Models for several further drugs were specified but not implemented, each for a stated reason rather than an oversight:

- **Δ9-THC.** Not a curve, and after a full survey of the published population models (2026-10-17) the reason is no longer one missing route but a split in the evidence. Every model that fixes THC's *disposition* credibly is calibrated on **smoked or vaporised** cannabis: a three-compartment fit in 12 occasional smokers parameterised in micro-constants on a 19.1 L central volume, a three-compartment fit in 6 subjects carrying both metabolites over 168 h, and a three-compartment fit with zero-order absorption in chronic and occasional users. All three are apparent on an unmeasured pulmonary bioavailability, and the engine has no pulmonary route to put them on. The one **oral** model that can be read in full fits *four* compartments — a central compartment, two peripherals, and an adipose pool reached through a permeability-surface-area product and a partition coefficient — which is one more than the closed form has; and it reports its central volume with a relative standard error of **186%**, so the source could not identify the parameter the first hour after a dose turns on, while every other number in the same table carries an RSE under 9%. The terminal half-life its parameters imply is 203 h against the 21.5 h the literature reports, and the paper itself says it "could not fully capture the accumulation characteristics of THC".

  How wide the disagreement is, is the point. Most of these models are apparent on an unmeasured pulmonary or oromucosal bioavailability, so their volumes cannot even be compared with an absolute one; and the only two *absolute* THC parameter sets in the literature disagree by a factor of 2.2 in clearance and 6.8 in steady-state volume. There is no consensus set to fall back on, and no way to assemble one — absorption from the oral paper and disposition from a smoked one would be an invention with a citation attached. The model that fits stanpumpR exactly is Heuberger 2015 (*Clin Pharmacokinet* 54(2):209-219): three compartments, oral, intravenous and pulmonary doses in a single fit, a clearance of 38.8 L/h, and built for the express purpose of characterising THC's long terminal phase. Its parameter table is behind a paywall this container cannot reach. A copy of that paper would settle it.

GHB was on this list until 2026-10-17 for a reason that turned out to be wrong twice over, which is worth recording: the non-linear engine already existed, and a complete human parameter set existed too, in a supplementary table rather than in the paper. Searching abstracts found nothing because the numbers were never in one.

### Reading these curves

Read them as exposure, within the studied population, route, formulation and dose range, and nothing else.

Where a number in a source is an anchor rather than a model, stanpumpR's rule turns on whether the anchor **identifies** what is missing. A peak time does, once the model is chosen and its disposition is fixed: in a one-compartment model with a known elimination rate, the time of the peak determines the absorption rate uniquely, which is how methamphetamine's intranasal route is offered. A bioavailability with no absorption rate does not, and neither does a peak time measured on a different formulation from the one being simulated. Where the anchor does not identify the missing parameter, the rule is to record it and refuse the route, rather than to fill the gap with an assumption that would look exactly like evidence on the plot. Either way the registry says which it was, and a derived parameter is marked `derived_approximation` rather than a published fit.

A different case is an input *shape* the engine cannot express, where every parameter is published and only the arithmetic has to be reorganised. MDMA's dose fills its depot at a constant rate over a fixed window before first-order absorption begins, which no single first-order depot reproduces. There stanpumpR **discretises** rather than approximates: the dose is split across the window, and the result is measured against a numerical evaluation of the exact solution with the agreement pinned in the drug's test. That is a port, not an anchor, so it stays a published fit.

A third case is a *structure* the library does not yet have, and there the answer is to build it rather than to approximate around it. GHB's source is two compartments with saturable elimination, so the numerical engine that already existed for phenytoin gained an optional peripheral compartment, with every other drug on that engine left bit-for-bit unchanged. Saturable elimination is not a detail that could have been smoothed away: it is why a second gram of GHB adds more than the first did.

### The dose basis, and where the unit names it

A model's parameters are fitted on a particular *mass*, and that mass is not always the one the plot reports or the one a reader has in mind. Three of these drugs make the point. Psilocybin is swallowed but psilocin is measured, and the two differ by their molecular weights. MDMA's source doses the hydrochloride throughout, so entering a free-base mass would under-predict. GHB is the sharpest: its source doses *and reports* sodium oxybate, while every independent study and the whole forensic literature report the free acid, 21% apart.

Where the gap is large and both bases are in common use, the **unit itself names the basis** — GHB is dosed in `g PO oxybate`, not a bare `g PO` — and the model's mass-carrying parameters are rescaled onto the plotted analyte by exact arithmetic that the drug's test pins. The registry records the input dose basis and the parameter dose basis as separate fields for every route, offered or not, so a mismatch is visible without reading any code.

### Validating against something the model never saw

Where independent human data exist, a new model here is checked against studies it was **not** fitted to, because that is what catches a dose-basis error — the error that makes a model wrong by a constant factor while every internal consistency check still passes. GHB's model reproduces its own source's simulated adult peak, which is reassuring but circular; the test that matters is that it also lands within 6% and 1% of the two peak concentrations Liechti measured in 32 volunteers a decade earlier, and tracks Palatini's 1993 dose-ranging series well enough to show exactly where its missing saturable absorption begins to bite.
