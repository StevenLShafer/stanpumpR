A dose with `PO`, `IM` or `IN` in its unit is not injected into the central compartment. It is placed in an absorption depot from which it enters the central compartment by first-order kinetics, after a lag, with only a fraction of the dose arriving at all.

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

| Drug | Routes |
|---|---|
| oxycodone | PO |
| hydromorphone | PO, IM, IN |
| codeine, hydrocodone, oxymorphone, tramadol | PO |
| gabapentin | PO, with saturable absorption |
| amiodarone | PO, as a constant daily rate (`mg/day PO`) |

Each drug's page shows the current absorption rate, bioavailability and lag. The oxycodone ka was chosen to reproduce the time of peak concentration seen in published studies (about 30 to 45 minutes) rather than taken from a fitted absorption model. Hydromorphone's intramuscular and intranasal absorption was revised so that each route's peak matches the measured time (about 20 minutes intranasal, 30 minutes intramuscular): the delay is now carried by the absorption rate constant rather than by a lag, which also keeps the time-until-threshold readout correct, since during a lag the engine has no effect-site state to count down.

## Saturable absorption

Gabapentin is absorbed by a carrier in the small intestine that saturates, so the larger the dose, the smaller the fraction absorbed. Such a drug declares the saturation, and each oral dose is scaled by its own fraction absorbed before it reaches the engine:

```
fraction absorbed = 1 - Imax × D / (ID50 + D)      D = dose in mg
```

For gabapentin, Imax is 0.906 and ID50 571 mg (Tran and colleagues, 2017): 0.69 of a 300 mg dose is absorbed, 0.54 of 600 mg and 0.39 of 1200 mg. The drug's bioavailability is then the limit for a very small dose. Once scaled, each dose is an ordinary first-order input, so doses still add, and the drug's page shows the fraction at several doses. What is not represented is saturation shared between doses: two doses entered as separate rows at the same time are each scaled by their own size, not by their sum, and absorption from doses taken close together does not compete.

## What to look for

Give oxycodone 10 mg PO and turn the plasma line on. The concentration rises over about an hour and the effect site, with its own slow ke0, peaks later still. Compare it with the sharp peak of an intravenous bolus of any opioid. [The oral oxycodone scenario](scenario:oral-oxycodone) shows two doses six hours apart.

## Limits

First-order absorption from a depot is the only absorption model, scaled per dose where it saturates. The exception is amiodarone's `mg/day PO`, a constant-rate input on apparent oral parameters that runs as an infusion; a zero-order absorption model for an oral dose given as an amount is not offered. Enterohepatic recirculation and saturation shared between overlapping doses are not modelled. An inverse Gaussian absorption model is drafted in the repository (`R/ig_absorption.R`) but is not wired in. Oral doses are invisible to Suggest Dosing and to [target-controlled infusion](help:tci), both of which treat them, as a real pump would, as unexpected additions.
