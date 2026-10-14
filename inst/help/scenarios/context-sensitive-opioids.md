## What to look for

Four opioids are infused at constant rates for three hours and stopped. The curves are **normalized to each drug's peak effect-site concentration**, so the infusion rates do not matter; only the shapes are compared.

During the infusion, remifentanil reaches 90 per cent of its plateau within about 20 minutes. The other three are far slower: alfentanil, fentanyl and sufentanil are each at about two thirds of their final level at an hour, and are still climbing at three hours.

After the infusions stop at 180 minutes, remifentanil is gone in minutes. Sufentanil falls to half in under half an hour and alfentanil in about three quarters of an hour. Fentanyl is slowest, nearly an hour, and falls further behind as time goes on: its large slow compartment, which it has spent three hours filling, now returns drug to the plasma and holds the concentration up, so that at 360 minutes it is still at a third of its peak and sufentanil at a seventh. Hover on the fentanyl and sufentanil lines at 240 and 360 minutes.

This is the **context-sensitive half-time** of Hughes, Glass and Jacobs made visible: the time to fall by half after an infusion depends on how long the infusion ran, and it differs by an order of magnitude between drugs that have similar terminal half-lives.

## Try next

- Shorten all four infusions to 30 minutes (change the four stop times to 30) and set *Max time* to 2 hours. Now fentanyl falls to half a little faster than alfentanil: after a short infusion, redistribution still works in its favour.
- Set *Normalize to* back to *none*, turn on **Time until threshold**, and watch the four black lines through the infusion. Remifentanil's is flat; fentanyl's climbs.
- Replace sufentanil with morphine 1 mg/hr and see a drug whose effect site lags so much that it is still rising after the infusion stops.

## Background

[Normalization](help:models/normalization) explains why only one line per drug is shown; [Time until threshold](help:models/recovery) covers the context-sensitive half-time. The models: [fentanyl](help:drugs/fentanyl), [alfentanil](help:drugs/alfentanil), [sufentanil](help:drugs/sufentanil), [remifentanil](help:drugs/remifentanil).
