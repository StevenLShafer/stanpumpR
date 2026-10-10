## What to look for

The time axis runs for 24 hours. The effect site peaks at about **12 minutes**, as fast as fentanyl, and the plasma falls steeply for the first hour as methadone distributes into tissue. Then both lines flatten: methadone's clearance is about 0.1 L/min against a volume of several hundred litres, and its terminal half-life is well over a day.

Hover at 1 hour and at 24 hours. The concentration is about 48 ng/mL at 1 hour and still about 17 ng/mL at 24 hours, a third of it, although almost all of the fall in the first hour was distribution, not elimination.

A single 10 mg dose in a 70 kg patient peaks at about 56 ng/mL in the effect site, just short of MEAC (60 ng/mL), yet the drug is still largely in the body a day later. A second dose adds to what remains. This is why methadone accumulates with repeated dosing and why its steady state takes days to reach.

## Try next

- Add 10 mg at 8 hours and 16 hours (480 and 960 minutes) and apply. The troughs rise each time: about 25 ng/mL before the second dose, 45 before the third, and 62 at 24 hours, above MEAC.
- Set *Time units* to days (the dose table is converted for you) and *Max time* to 7 days, then give 10 mg every 8 hours for the first three days: a `mg tid` row at `0`, and a `0 mg tid` row at `3` to stop it. See how long after the last dose the concentration stays above the threshold: nearly two days.
- Change the dose's units to `mg PO` and apply: the same 10 mg by mouth peaks at about 3 hours at about 30 ng/mL, half the intravenous effect-site peak, but by 24 hours the two curves are almost the same.
- Load [the opioid MEAC scenario](scenario:opioid-meac) for the opposite extreme: fentanyl, gone within an hour.

## Background

[Methadone](help:drugs/methadone) is Henthorn and Kharasch's model of the two enantiomers, summed to racemic methadone. Methadone's half-life varies several-fold between individuals, more than for most drugs; the curve is a typical patient's.
