A published pharmacokinetic model is usually reported as a table: half-lives, a clearance, a volume or two. It is tempting to read such a table the way one reads a list of drug properties, a longer half-life meaning slower recovery and a bigger volume meaning more drug to get rid of. That reading is wrong often enough to be useless. How fast a concentration falls depends on every number in the table at once, through mathematics that nobody can do in their head, and the only reliable way to know what a model predicts is to compute it. That is the reason stanpumpR exists.

In 1992 Steven Shafer and Donald Stanski made the point with a drug that does not exist. Their editorial, *Improving the clinical utility of anesthetic drug pharmacokinetics* (*Anesthesiology* 1992;76:327-330), appeared beside the paper in which Hughes, Glass and Jacobs introduced the context-sensitive half-time (see [TCI and the offset of drug effect](help:offset)). Its Table 1 describes **Duzitol**, a fictional drug whose name is a joke ("a drug that does it all"), in normal subjects and in hepatic failure, beside propofol.

## Table 1: three models, as published

| | Duzitol, normal subjects | Duzitol, hepatic failure | Propofol, normal subjects |
|---|---|---|---|
| Half-lives (min) | 1.0, 10.0, 100 | 4.0, 40.0, 400 | 0.8, 7.8, 198 |
| Metabolic clearance (L/min) | 0.68 | 0.51 | 1.9 |
| V1 (L) | 7.6 | 5.0 | 6.3 |
| Vdss (L) | 66 | 93 | 284 |

Each column is a complete three-compartment model: six numbers for six unknowns (the appendix below shows how they determine the model).

## What the table seems to say

Read column by column, the table tells a clear story.

- **Hepatic failure should recover slowest.** Every half-life is four times longer than in normal subjects, the clearance is 25% lower, and the steady-state volume is 41% larger. Each of these, taken alone, points to a drug that lingers.
- **Propofol should recover more slowly than Duzitol.** Its terminal half-life is twice Duzitol's (198 against 100 minutes) and its steady-state volume more than four times larger (284 against 66 L). Its clearance is nearly three times larger, but against a volume that size it seems unlikely to make up the difference.

## What the models predict

Both conclusions are wrong. Built and simulated, the three models give these times for the plasma concentration to fall by half after an infusion long enough to reach steady state, as the editorial printed them: **41 minutes** for Duzitol in normal subjects, **10 minutes** in hepatic failure and **20 minutes** for propofol (the models give 41.3, 10.1 and 21.8; propofol's difference is within the rounding of Table 1, as explained below). Hepatic failure, with every half-life four times longer, recovers four times faster. Propofol, with the longer terminal half-life and the far larger volume, recovers about twice as fast as Duzitol.

The table below is computed from Table 1 every time this page is shown. The 50% decrement times are what the models give; the editorial's are beside them.

<!-- generated: duzitol-model-table -->

The editorial's Figure 1 plotted the same comparison against the length of the infusion: the context-sensitive half-time, the time for a 50% fall after a target-controlled infusion held the plasma concentration constant for that long. The upper panel below is that figure, recomputed. The lower panel follows the plasma concentration after a steady-state infusion stops.

<!-- generated: duzitol-figure -->

## Why

None of the six numbers in a column is the speed of any one process. The three half-lives are properties of the whole system, the roots of a cubic equation whose coefficients mix every volume and clearance together, and the table says nothing about how much of the concentration each of them governs. That is what decides how fast the plasma concentration falls, and it has to be computed.

The row *Share of the concentration in each exponential* above does that. When a steady-state infusion stops, the plasma concentration is the sum of three exponentials, and the shares are their sizes. In hepatic failure the fastest exponential, with its 4-minute half-life, carries about 57% of the concentration: the concentration can fall by half almost entirely by that one phase, and it does so in 10 minutes. In normal subjects the slowest exponential carries about 65%, so the concentration cannot fall by half until the 100-minute phase has done much of the work, and that takes 41 minutes. Propofol's two fast exponentials carry about 48%, just short of half, so the last two per cent of the fall wait on the slow 198-minute phase, which is why propofol's time, 22 minutes, sits between the other two.

The derived model shows the physiology behind those shares. In hepatic failure the intercompartmental clearances are tiny, 0.14 and 0.20 L/min against 2.6 and 0.80 in normal subjects: the large peripheral volume holds a great deal of drug, but hands it back to the plasma so slowly that it cannot hold the concentration up, while the small central volume, 5 L cleared at 0.51 L/min, empties with a half-time of under 7 minutes. In normal subjects the fast peripheral compartment returns its drug to the plasma with a half-time of about three minutes, propping up the concentration while the drug is eliminated. No column of Table 1 shows either of these things; both emerge only when the model is built.

## The answer depends on the question

The 50% decrement is only one question. A patient may need the concentration to fall by 20% to wake, or by 80%. For an 80% fall after a steady-state infusion the same three models give 169 minutes for normal subjects, 235 minutes in hepatic failure and 271 for propofol, and the order changes again: hepatic failure is now slower than normal. The drug stored in the slowly exchanging peripheral volume, which did not matter for a fall by half, sets the pace once the concentration must fall further. The lower panel of the figure shows the curves crossing. Which drug or which patient recovers faster has no answer in general; it has an answer for a particular regimen and a particular decrease, and it is found by simulation. This is the idea that Shafer and Elizabeth Youngs generalised in 1994 as the decrement time ([TCI and the offset of drug effect](help:offset)).

## Seeing it in the simulator

Duzitol is not in the drug library. It is fictional, and the library offers only models of real drugs, so it cannot be dosed in the simulator; the figure and table on this page are drawn instead from the same three-compartment mathematics the simulator uses ([The three-compartment model](help:models/three-compartment)). For real drugs the simulator answers the same question directly: *Time until threshold* shows, at every moment, how long the concentration would take to fall to a chosen level if all drug were stopped then ([Time until threshold](help:models/recovery)), and a *Plasma target* holds the concentration constant exactly as the editorial assumed ([Target-controlled infusion](help:tci)). The [context-sensitive decrement scenario](scenario:context-sensitive-opioids) compares four real opioids this way.

## How exact the printed numbers are

The models reproduce the editorial's 41 and 10 minutes. For propofol they give 21.8 minutes where the editorial printed 20. Table 1 prints its numbers to two or three figures, and moving each of propofol's six numbers to either end of its last printed digit moves the computed time anywhere from 17.3 to 26.8 minutes, so the printed 20 is within what the table's own rounding allows. That spread is itself a lesson: propofol's two fast exponentials carry just under half of the concentration, so its time to a 50% fall hangs on whether the slow phase is needed at all, and small changes in the parameters decide it.

## Appendix: a three-compartment model from CL, V1, Vss and three half-lives

This is the algebra of the spreadsheet Shafer used to build the editorial's three models from Table 1, written down at his request. It reproduces that spreadsheet to every printed digit, and it is what `threeCompartmentFromHalfLives()` in `R/duzitol.R` computes.

For a mammillary three-compartment model with rate constants k10, k12, k13, k21 and k31, the exponents λ1, λ2, λ3 (each ln 2 divided by a half-life) are the roots of the characteristic polynomial s³ + a2 s² + a1 s + a0, whose coefficients are both symmetric functions of the λs and sums of products of the rate constants:

```
a2 = λ1 + λ2 + λ3          = k10 + k12 + k13 + k21 + k31
a1 = λ1λ2 + λ1λ3 + λ2λ3    = k10 k21 + k10 k31 + k21 k31 + k12 k31 + k13 k21
a0 = λ1 λ2 λ3              = k10 k21 k31
```

The table gives k10 = CL/V1, the three λs, and Vss, which in a mammillary model is V1 (1 + k12/k21 + k13/k31). Four unknowns remain: k12, k13, k21 and k31.

1. From a0, the product of the return rates: `k21 k31 = λ1λ2λ3 / k10 = g0`.
2. From Vss: Vss/V1 − 1 = k12/k21 + k13/k31 = (k12 k31 + k13 k21) / (k21 k31), so `k12 k31 + k13 k21 = (Vss/V1 − 1) g0 = g1`.
3. From a1, which contains both: a1 = k10 (k21 + k31) + k21 k31 + (k12 k31 + k13 k21), so the sum of the return rates is `k21 + k31 = (a1 − g0 − g1) / k10 = f`.
4. k21 and k31 have sum f and product g0, so they are the two roots of x² − f x + g0 = 0, that is (f ± √(f² − 4 g0)) / 2, the larger taken as k21 so that compartment 2 is the faster.
5. From a2: `k12 + k13 = a2 − k10 − k21 − k31 = i`.
6. k12 and k13 solve the linear pair k12 + k13 = i and k12 k31 + k13 k21 = g1: substituting k12 = i − k13 gives `k13 = (g1 − i k31) / (k21 − k31)`, and k12 = i − k13.

The volumes and clearances follow: V2 = V1 k12/k21, V3 = V1 k13/k31, CL2 = V1 k12, CL3 = V1 k13. The six numbers describe a real model only when f² > 4 g0 and k12, k13, k21 and k31 all come out positive; otherwise they describe none, and the function refuses them.

To simulate the context-sensitive half-time, the plasma concentration is held at 1 for T minutes from a drug-free start, as a target-controlled infusion would hold it. The peripheral amounts when the infusion stops are then V2 (1 − e^(−k21 T)) and V3 (1 − e^(−k31 T)), with V1 in the central compartment, and the decline from that state is a sum of three exponentials in the same λs, whose coefficients are the shares in the table above. The half-time is the time at which that sum reaches 0.5.
