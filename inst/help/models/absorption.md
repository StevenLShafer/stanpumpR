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

| Drug | Routes |
|---|---|
| oxycodone | PO |
| hydromorphone | PO, IM, IN |
| codeine, hydrocodone, oxymorphone, tramadol | PO |

Each drug's page shows the current absorption rate, bioavailability and lag. The oxycodone ka was chosen to reproduce the time of peak concentration seen in published studies (about 30 to 45 minutes) rather than taken from a fitted absorption model. Hydromorphone's intramuscular and intranasal absorption was revised so that each route's peak matches the measured time (about 20 minutes intranasal, 30 minutes intramuscular): the delay is now carried by the absorption rate constant rather than by a lag, which also keeps the time-until-threshold readout correct, since during a lag the engine has no effect-site state to count down.

## What to look for

Give oxycodone 10 mg PO and turn the plasma line on. The concentration rises over about an hour and the effect site, with its own slow ke0, peaks later still. Compare it with the sharp peak of an intravenous bolus of any opioid. [The oral oxycodone scenario](scenario:oral-oxycodone) shows two doses six hours apart.

## Limits

Only first-order absorption is supported. Zero-order (constant-rate) absorption, enterohepatic recirculation, and absorption that saturates are not modelled. An inverse Gaussian absorption model is drafted in the repository (`R/ig_absorption.R`) but is not wired in. Oral doses are invisible to Suggest Dosing and to [target-controlled infusion](help:tci), both of which treat them, as a real pump would, as unexpected additions.
