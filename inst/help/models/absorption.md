A dose with `PO`, `IM` or `IN` in its unit is not injected into the central compartment. It is placed in an absorption depot from which it enters the central compartment by first-order kinetics, after a lag, with only a fraction of the dose arriving at all.

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

| Drug | Routes | ka (1/min) | F | Lag |
|---|---|---|---|---|
| oxycodone | PO | 0.06 | 0.5 | 0 |
| hydromorphone | PO, IM, IN | 0.01 for each | 0.6 for each | 0, 90 and 180 min |

Each drug's page shows the current values. The oxycodone ka was chosen to reproduce the time of peak concentration seen in published studies (about 30 to 45 minutes) rather than taken from a fitted absorption model; the hydromorphone values are provisional, and their intramuscular and intranasal lag times are described in the code as placeholders. The active-metabolite work in development revises hydromorphone's absorption; see [In development](help:in-development).

## What to look for

Give oxycodone 10 mg PO and turn the plasma line on. The concentration rises over about an hour and the effect site, with its own slow ke0, peaks later still. Compare it with the sharp peak of an intravenous bolus of any opioid. [The oral oxycodone scenario](scenario:oral-oxycodone) shows two doses six hours apart.

## Limits

Only first-order absorption is supported. Zero-order (constant-rate) absorption, enterohepatic recirculation, and absorption that saturates are not modelled. An inverse Gaussian absorption model is drafted in the repository (`R/ig_absorption.R`) but is not wired in. Oral doses are invisible to Suggest Dosing and to the target-controlled infusion in development, both of which treat them, as a real pump would, as unexpected additions.
