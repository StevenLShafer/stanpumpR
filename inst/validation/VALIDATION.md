# Gas Man validation record

A running record of comparisons between this package's Gas Man baseline
(`R/advanceGasManBaseline.R`, and the standalone
`inst/validation/gasman_baseline_standalone.R`) and Gas Man itself, run through
the Gas Man API.

The point of the baseline is to *be* Gas Man, so that any later divergence is a
deliberate, documented change rather than a transcription error. This file
records what has actually been checked, and — just as importantly — what has
not.

---

## 2026-09-04 — first concordance run

**Run by:** Richard Epstein, using the Gas Man API.
**Scenario:**

```r
gasman_simulate(
  agents = list(list(name = "Sevoflurane",   del = 2.0),
                list(name = "Nitrous Oxide", del = 50)),
  fgf = 8, va = 4, co = 5, weight = 70, minutes = 30)
```

semi-closed circuit, `dt_ms = 6000`. Values at 30 minutes, sevoflurane:

| column | ours (double) | Gas Man (float32) | relative difference |
|---|---|---|---|
| CKT | 1.896201603 | 1.896202445 | 4.4e-07 |
| ALV | 1.689227888 | 1.689230442 | 1.5e-06 |
| ART | 1.689227888 | 1.689230442 | 1.5e-06 |
| VRG | 1.685521021 | 1.685523272 | 1.3e-06 |
| MUS | 0.298360667 | 0.298367709 | 2.4e-05 |
| FAT | 0.017531043 | 0.017541863 | 6.2e-04 |
| VEN | 1.335752758 | 1.335756302 | 2.7e-06 |
| Uptake | 0.433974079 | 0.434033245 | 1.4e-04 |
| Delivered | 4.800000000 | 4.799994946 | 1.1e-06 |

### The residual is Gas Man's float32 accumulation, not a modelling difference

`Delivered` proves it. That column has no model and no parameters in it: it is
300 additions of `2.0 * 8 * 0.1 / 100 = 0.016`. The exact answer is 4.8. Gas Man
returns 4.799994946, a relative error of 1.1e-06 on a pure sum of identical
terms, where no partition coefficient, volume or blood flow appears at all.

Gas Man stores results in C++ `float` (`typedef float COMP_ARRAY[MAX_COMPART]`).
Float32 epsilon is 1.19e-07 and the run is 300 ticks. No change to any parameter
can move that number.

The rest of the residual has the same signature: the error grows as the quantity
gets smaller and slower, worst at FAT (6.2e-04), whose 0.0175 is assembled from
300 minute increments where float32 has least headroom. **Our double-precision
values are the more accurate ones.**

### What this run establishes

* The transcription of `GasDoc.cpp::Calc` and `CalcUptake` is correct for this
  scenario, to the limit of Gas Man's own arithmetic.
* **The cross-gas uptake coupling is validated.** This scenario ran nitrous
  oxide at 50% alongside sevoflurane, so `totUptake` carried both agents and the
  "Correct for constant lung capacity" term was exercised. That term is the
  concentration and second gas effect, and it is the one an earlier reading of
  the source wrongly concluded Gas Man did not implement.
* **The tissue coefficients are tissue:GAS.** Epstein hard-coded the per-agent
  constants in his scenario; this side let `gasman.ini` supply them. Agreement to
  1e-06 settles a question that could not be settled by reading, because Gas
  Man's own scenario template documents `lambdaVrg` / `lambdaMus` / `lambdaFat`
  as tissue:*blood* while supplying tissue:*gas* values.
* **`GetVA` reports inspired ventilation, not the setting.** Gas Man returned
  VA 4.170708179 against a setting of 4; the difference, 0.170708 L/min, is the
  summed uptake rate, matching `GetVA`'s `totalUptakeRate + m_fVA`. A reporting
  difference, not an error. `gasman_compare()` does not compare VA.
* Sevoflurane parameters are identical on both sides: Lambda 0.65, VRG 1.1,
  MUS 2.4, FAT 34, MAC 2.1.

### What this run does NOT establish

One scenario, at Gas Man's default flows, at 70 kg, on a semi-closed circuit,
for 30 minutes, with settings constant throughout. Untested:

* Low fresh gas flow and near-closed circuits, where the circuit equation
  dominates and rebreathing matters most.
* Open and ideal circuits. The ideal circuit carries an explicit threshold at
  FGF = VA that the semi-closed differential form does not have.
* Any weight other than 70, where the weight scaling corrected in `4185455`
  becomes live.
* Reduced or raised cardiac output.
* Agents other than sevoflurane and nitrous oxide.
* Settings that change during the run.
* Runs long enough for the fat compartment to matter.

`inst/validation/gasman_validation_grid.R` covers the first four of these and
writes the Gas Man scenario files for them.

### Open question

Epstein initially reported uptake "MUCH faster" in this code than in Gas Man.
That report preceded this run and is not reproduced by it. The base tick was
wrong at the time (1000 ms against Gas Man's 6000), but measurement put that at
about 6% at one minute and under 0.2% by twenty — too small to be the whole
cause. What changed between the two runs has not been established.

---

## 2026-09-04 — the five-case grid, our side

**Run by:** this repository, on `newryzen`, via
`inst/validation/gasman_export_results.R`. Gas Man has **not** yet been run
against these; this section records our answers so that when it is, the
comparison is against a fixed, dated reference rather than a moving one.

Every case: 70 kg, semi-closed, 30 minutes, `dt_ms = 6000`, settings constant,
uptake coupling and recirculation on.

Alveolar tension of the primary agent, percent of one atmosphere:

| case | agent | dial | FGF | VA | CO | 1 min | 5 min | 15 min | 30 min |
|---|---|---|---|---|---|---|---|---|---|
| 1 | sevoflurane | 2.0 | 8.0 | 4 | 5.0 | 0.440 | 1.185 | 1.518 | 1.593 |
| 2 | sevoflurane + 70% N2O | 2.0 | 8.0 | 4 | 5.0 | 0.466 | 1.376 | 1.692 | 1.731 |
| 3 | isoflurane | 1.2 | 2.0 | 4 | 5.0 | 0.066 | 0.271 | 0.473 | 0.566 |
| 4 | desflurane | 6.0 | 0.5 | 4 | 5.0 | 0.122 | 0.768 | 1.806 | 2.570 |
| 5 | sevoflurane + 70% N2O | 2.0 | 2.0 | 6 | 2.5 | 0.192 | 0.860 | 1.438 | 1.677 |

Case 2 minus case 1 is the second gas effect in isolation: identical dial, flow
and ventilation, differing only in whether nitrous oxide is running and so
whether `totUptake` carries a second gas. It reaches 1.376 against 1.185 at
five minutes, a ratio of 1.16.

### Checks that passed before Gas Man is involved

* **`Delivered` is exact.** All five cases reproduce `dial x FGF x t / 100` to
  between 0 and 2e-15. That column has no model, no parameters and no
  integration scheme in it, so it isolates input handling from modelling: if it
  ever disagrees with Gas Man, the dial or the flow is being read differently
  and nothing downstream is worth looking at until that is fixed.
* Every tension finite, non-negative, and never above the dial that produced it.
* `VA` reports inspired ventilation in every case, above the setting by the
  summed uptake rate: 4.011, 4.238, 4.008, 4.012 and 6.162 against settings of
  4, 4, 4, 4 and 6. Case 2 is the largest excess, as it should be, being the
  case with the most nitrous oxide being taken up.

### Still not established

Gas Man has not been run on any of these. Cases 3, 4 and 5 are the first to
exercise low flow, a near-closed circuit, a soluble agent, desflurane and
reduced cardiac output, none of which the 2026-09-04 concordance run touched.
Weight is still 70 throughout, so the scaling corrected in `4185455` remains
untested, and every case holds its settings constant.

### Correction, same day: the reported VA depended on the output grid

The `VA` column in the first version of this grid export was wrong, and the CSV
sent to Epstein on 2026-09-04 carries the wrong values.

`gasman_simulate()` reconstructed the uptake rate by interpolating *cumulative*
uptake on the **output** grid, at `t` and `t - dt`. That makes a reported number
depend on how often output happens to be written, which it must not. Measured on
case 1, identical model run, VA at 30 minutes:

| output spacing | VA reported (before) | VA reported (after) |
|---|---|---|
| 1 s | 4.010966406 | 4.010966406 |
| 5 s | 4.002193300 | 4.010966406 |
| 30 s | 4.010977700 | 4.010966406 |
| 60 s | 4.010991900 | 4.010966406 |

The fix records the uptake increment over each tick as the run proceeds, which
is the window `GetVA` actually uses:
`(sum over gases of UPT(t) - UPT(t - one tick)) / dt + m_fVA`.

Two things this does not change. The model is untouched — every tension, and
`Uptake` and `Delivered`, are identical before and after; only the reported `VA`
moves. And the agreement with Gas Man stands: on Epstein's scenario the
corrected figure is 4.170709095 against his 4.170708179, a relative difference
of 2.2e-07, now independent of output spacing where before it was not.

The bug was confined to `inst/validation/gasman_baseline_standalone.R`.
`R/advanceGasManBaseline.R` takes VA as an input and never reconstructs it.
