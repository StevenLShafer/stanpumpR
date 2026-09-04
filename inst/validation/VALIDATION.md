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
