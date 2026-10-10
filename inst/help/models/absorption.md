A dose with `PO`, `SL`, `IM`, `IN` or `RA` in its unit is not injected into the central compartment. It is placed in an absorption depot from which it enters the central compartment by first-order kinetics, after a lag, with only a fraction of the dose arriving at all.

The one exception is a **rate** with a route word, `mg/day PO`, which only [amiodarone](help:drugs/amiodarone) offers. It is a constant-rate (zero-order) oral input, the way Pollak and colleagues modelled a daily oral dose: there is no depot, absorption rate constant, lag or bioavailability, and the daily dose enters the central compartment evenly over the day, on the drug's apparent oral parameters, until the drug's next rate row, exactly as an infusion would.

## The model

For each route a drug may define three parameters:

| Parameter | Meaning |
|---|---|
| ka | First-order absorption rate constant (1/min). The absorption half-time is ln(2)/ka. |
| bioavailability (F) | The fraction of the dose that reaches the systemic circulation |
| tlag | A delay before absorption begins (min) |

The depot empties into the central compartment at rate ka × (amount remaining), so the plasma concentration after an oral dose is a sum of the three disposition exponentials plus one more with exponent ka, and the effect site adds ke0: five exponentials in all, each still closed-form. When ka is slower than the disposition exponents, as it is for oral opioids, the rise and the peak are set by absorption rather than by distribution: the curve is "flip-flop" kinetics, and the time of peak plasma concentration is roughly the absorption half-time plus a little.

Boluses of the same drug given intravenously add to the same compartments, so oral and intravenous doses can be mixed in one simulation.

## Which drugs have routes

The table is built from the drug library, the same list the dose table's Units selector reads, so it shows every oral, sublingual, intramuscular, intranasal and regional anesthesia unit on offer. Most of these drugs also offer their units as repeating doses (`mg PO bid` and so on; see [The dose table](help:dose-table)), which are not listed separately.

<!-- generated: route-table -->

Each drug's page shows the current absorption rate, bioavailability and lag, and where each route's parameters come from. They are not all of the same standing:

- **Fitted with the intravenous model.** Clindamycin's oral parameters were fitted together with its intravenous ones, so its bioavailability is absolute.
- **Cross-study additions.** For metronidazole, hydrocortisone, methylprednisolone and dexamethasone some or all of the oral parameters (and for dexamethasone the intramuscular ones) come from studies other than the disposition model's. Methylprednisolone's absorption rate constant is provisional: its oral exposure does not depend on it, its peak does.
- **Apparent oral parameters.** Cefalexin and amiodarone were fitted to oral data alone with the bioavailability unknown, which predicts oral concentrations correctly and intravenous ones wrongly, so both are offered by mouth only; intravenous amiodarone is the separate [amiodarone IV](help:drugs/amiodaroneIV) row. Prednisone is oral only because no routine intravenous product was verified.
- **Reduced to one input.** An oral dose of prednisolone or prednisone reaches the circulation partly as each steroid, which a single input cannot carry; effective coefficients make the oral exposures exact and the shapes approximate.
- **Derived rather than fitted.** Naloxone's intranasal bioavailability and absorption rate are derived from a published model of the concentrated spray that has no absolute bioavailability, and are initial values rather than estimates.
- **Apparent oral models.** Alprazolam, clonazepam and zolpidem were fitted to oral data alone, so their volumes and clearances are apparent (divided by the unknown bioavailability, which is carried as 1) and they are offered by mouth only.
- **Absorption added to an intravenous model.** Lorazepam's oral and intramuscular routes come from a separate five-route crossover; temazepam's oral route from a separate oral study and the label's bioavailability, on a disposition fitted to intravenous data.
- **Reduced to one input, fitted to the published curve.** Buprenorphine's sublingual source has two parallel pathways (a fast burst and a slow mucosal tail) and a bioavailability that falls with dose. A single absorption rate constant was fitted to the published input at 16 mg, which keeps the time to peak and the exposure but puts the peak about 20% low; each dose is scaled by its own fraction absorbed (see *Saturable absorption* below). Its intranasal route, from a nine-volunteer spray study, is research only.
- **Chosen to match a peak height.** Diazepam's oral and intramuscular absorption rates were chosen so that the typical peak matches the observed mean peak; the typical curves then peak earlier (oral) and later (intramuscular) than observed.
- **Chosen to match a time of peak.** The oxycodone ka was chosen to reproduce the time of peak concentration seen in published studies (about 30 to 45 minutes) rather than taken from a fitted absorption model. Hydromorphone's oral parameters are provisional, and its intramuscular route has no human pharmacokinetic study behind it: its bioavailability of 1 and its 30-minute peak are a judgement.

Hydromorphone's intramuscular and intranasal absorption was revised so that each route peaks at its intended time (about 20 minutes intranasal, from Coda's data, and 30 minutes intramuscular): the delay is now carried by the absorption rate constant rather than by a lag, which also keeps the time-until-threshold readout correct, since during a lag the engine has no effect-site state to count down. Gabapentin (0.31 h) and pregabalin (0.32 h) keep the lags their sources estimated, so time until threshold reads blank for those minutes after each of their doses. Clonazepam keeps the 0.369 h lag dos Santos and colleagues estimated for its tablets. Zolpidem's source absorbed it through a chain of transit compartments, which delivers the dose almost as a pure delay; it is represented by a lag of 0.25 h, the mean transit time, followed by the published absorption rate. A drug's time to peak effect after an oral dose is counted from the dose, lag included.

## Regional anesthesia (RA)

`RA` is a local anesthetic injected into tissue: a peripheral nerve block or a wound infiltration. The dose enters a tissue depot and is absorbed into the systemic circulation by first-order kinetics, exactly as an oral dose is absorbed from the gut, and the curve is the total (bound plus unbound) concentration in plasma. [Lidocaine](help:drugs/lidocaine), [bupivacaine](help:drugs/bupivacaine), [ropivacaine](help:drugs/ropivacaine) and [mepivacaine](help:drugs/mepivacaine) offer it, each also as an intravenous `mg`, which shows what an unintended intravascular injection does.

A drug may also declare a **slow second depot** (`ka_RA_slow` and `fraction_RA_slow` in its model): each RA dose is then split between a fast and a slow depot absorbing in parallel, each first-order, which is how absorption from tissue usually looks. [Mepivacaine](help:drugs/mepivacaine) uses it; the other local anesthetics have one depot, because no study of a peripheral block gives both rates for them against an intravenous reference.

Each drug's absorption is site- and solution-specific: it was chosen from one study of one block, with or without epinephrine, as each drug's page records, and the bioavailability is assumed to be 1. Absorption from tissue is really faster at some sites (intercostal, epidural) than others (subcutaneous, femoral and sciatic), slowed by epinephrine at some sites and not others, and often biphasic, a fast and a slow process in parallel, which a drug with one depot does not represent. Use the curves for the shape and the scale of systemic exposure, not to predict toxicity: that depends on the unbound concentration and the patient as well. RA is a single injection; a perineural catheter infusion is not offered.

## Saturable absorption

Gabapentin is absorbed by a carrier in the small intestine that saturates, so the larger the dose, the smaller the fraction absorbed. Such a drug declares the saturation, and each oral dose is scaled by its own fraction absorbed before it reaches the engine:

```
fraction absorbed = 1 - Imax × D / (ID50 + D)      D = dose in mg
```

For gabapentin, Imax is 0.906 and ID50 571 mg (Tran and colleagues, 2017): 0.69 of a 300 mg dose is absorbed, 0.54 of 600 mg and 0.39 of 1200 mg. The drug's bioavailability is then the limit for a very small dose. Once scaled, each dose is an ordinary first-order input, so doses still add, and the drug's page shows the fraction at several doses. What is not represented is saturation shared between doses: two doses entered as separate rows at the same time are each scaled by their own size, not by their sum, and absorption from doses taken close together does not compete. Buprenorphine's sublingual bioavailability falls with the dose too, and its sublingual doses are scaled by the same expression: Björnsson and colleagues reported a power law, which was fitted to this form over the 2 to 32 mg of the marketed tablets (see [buprenorphine](help:drugs/buprenorphine)). Pregabalin, a close relative of gabapentin, is about 90 per cent absorbed whatever the dose (Bockbrader and colleagues, 2010): its absorption is linear, and [the pregabalin scenario](help:scenarios/pregabalin-linear-absorption) sets the two side by side.

## Bioavailability that rises with the dose, and exposure more than proportional to it

The same per-dose scaling carries two other shapes. Sertraline's bioavailability, in Alhadab and Brundage's meta-analysis of single doses, **rises** with the dose, as 0.639 × D / (15.5 + D): 0.49 of a 50 mg dose and 0.59 of 200 mg. Paroxetine's apparent clearance in Kim and colleagues' model **falls** with the daily dose, as (D / 25)<sup>-0.363</sup>; a linear engine cannot change its clearance dose by dose, so the dose is scaled by (D / 25)<sup>0.363</sup> instead, which gives the same steady-state exposure for a once-daily dose. Its half-life is then that of 25 mg a day at every dose. Both are described on the drugs' own pages and on [Antidepressant models and their limits](help:models/antidepressants).

## What to look for

Give oxycodone 10 mg PO and turn the plasma line on. The concentration rises over about an hour and the effect site, with its own slow ke0, peaks later still. Compare it with the sharp peak of an intravenous bolus of any opioid. [The oral oxycodone scenario](scenario:oral-oxycodone) shows two doses six hours apart.

## Limits

First-order absorption from a depot is the only absorption model, scaled per dose where it saturates. The exception is amiodarone's `mg/day PO`, a constant-rate input on apparent oral parameters that runs as an infusion; a zero-order absorption model for an oral dose given as an amount is not offered. Enterohepatic recirculation and saturation shared between overlapping doses are not modelled. An inverse Gaussian absorption model is drafted in the repository (`R/ig_absorption.R`) but is not wired in. Oral doses are invisible to Suggest Dosing and to [target-controlled infusion](help:tci), both of which treat them, as a real pump would, as unexpected additions.
