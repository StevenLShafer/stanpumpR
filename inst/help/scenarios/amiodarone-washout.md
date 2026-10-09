## What to look for

Amiodarone is given on Pollak's stepped regimen for 26 weeks (182 days) and then stopped. The plot runs for a year, in **days**, and **Time until threshold** is on: the black line is how long serum amiodarone would take to fall below 1.0 mg/L, the bottom of the therapeutic window, if dosing stopped at that moment.

After half a year of therapy the tissues are full: serum and peripheral compartment are in equilibrium, and from the moment dosing stops drug flows back from the tissues into the serum. The serum level still falls quickly at first, by a quarter in about 2 days and below the window after about 8 days, because elimination empties the small central volume (882 L, cleared at 229 L/day) faster than the tissues can refill it. Then it slows almost to a stop. Halving takes about 31 days and a three-quarter fall about 87 days, because by now the peripheral compartment holds most of the drug and returns it to the serum for months. Desethylamiodarone, with a terminal half-life of about 60 days, is still measurable at the end of the year.

This is Pollak's context-sensitive decrement: a short interruption of dosing (two or three days) produces a useful fall in concentration even at steady state, but reversing a real overload takes weeks to months.

The paper's text quotes 3, 36 and 98 days for the 25, 50 and 75% decrements at steady state. The published parameters give 2.2, 31.2 and 86.6 days, which is what the simulator shows; the source of the difference is not known.

## Try next

- Stop after four weeks instead: change the 400 mg/day row at day 28 to 0, and delete the two rows below it (343 mg/day at day 90 and the stop at day 182). Rows at the same time add up, so simply moving the stop row to day 28 would leave the 400 mg/day running. After only four weeks the 50% decrement takes about 9 days instead of a month: the tissues have not yet filled, and drug is still moving into them when dosing stops.
- Hover on the black line at different times during the loading phase and see how the time until threshold grows as therapy goes on.
- Change *Time units* to **weeks** to read the washout in weeks. The dose times convert (day 2 becomes 0.2857142857 weeks) and Max time becomes 52 weeks, the longest weeks choice, which is a day short of a year; switch back to days and the times return exactly.

## Background

[Time until threshold](help:models/recovery) explains the black line. The model: [amiodarone](help:drugs/amiodarone).
