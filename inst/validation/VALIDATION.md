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

## 2026-10-05 — defaults aligned with Gas Man; deliberate differences recorded

Recorded by Claude Code (Claude Fable 5.1) at the direction of S. Shafer, who
stated the policy: *identical with Gas Man for now, to be updated with more
current data later.* The Gas Man source was read directly for the first time on
this machine, from `github.com/rasman/gasmanonline`, `gasman_api/gasmanAPI`
(`gasman.ini`, `GasDoc.cpp`, `GasGlobal.h`).

### Changed to match Gas Man

| Quantity | Was | Now | Gas Man source |
|---|---|---|---|
| Default alveolar ventilation | none (0, i.e. apnea, unless a row was entered) | 4 L/min at 70 kg x (weight/70)^0.75 | `[Defaults] VA=4`; `m_fVA = m_fDfltVA * factor` |
| Cardiac output | 75 mL/kg, linear (5.25 L/min at 70 kg) | 5 L/min at 70 kg x (weight/70)^0.75 | `[Defaults] CO=5`; `m_fCO = m_fDfltCO * factor` |

where `factor = sqrt(sqrt(f * f * f))` with `f = weight / 70`. Compartment
volumes were already scaled linearly with weight on both sides
(`fWtFactor = fWeight / STD_WEIGHT`), and the circuit volume is unscaled on both.

The earlier concordance runs in this file forced cardiac output to 5.0 on our
side, so their results are unaffected by the change of default.

### Deliberate differences, confirmed and retained

Shafer reviewed these on 2026-10-05 and judged each a good decision. They are
not defects to be reconciled, and a comparison against Gas Man must allow for
them:

1. **Age-adjusted MAC.** `macForAge()` applies Mapleson's relation; Gas Man uses
   `m_fMAC` raw. Compare at age 40, where the adjustment is exactly 1.
2. **Summed MAC.** One MAC series, additive across potent agents; Gas Man emits
   one row per agent and never sums them. Nitrogen is excluded from the sum.
3. **Oxygen is modelled**, with metabolic consumption (3.5 mL/kg/min) and a
   floor at zero; Gas Man does not model oxygen, so there is no reference for
   it. Oxygen is excluded from the uptake coupling, as before.
4. **Starting nitrogen is 78.07%**, alongside 20.93% oxygen, rather than Gas
   Man's `Ambient=80`.
5. **Exact integration.** Each sub-step is advanced by matrix exponential where
   Gas Man splits it. The two converge as the step shrinks and do not agree
   digit for digit at a fixed step; see `tests/testthat/test-gas-convergence.R`.

### Not established by this entry

No new run against Gas Man itself was made today. In particular the allometric
defaults have been transcribed from the source and unit-tested, but not checked
against Gas Man output at a weight other than 70 kg.

---

## 2026-10-05 — five scenarios for the app engine

Recorded by Claude Code (Claude Fable 5.1) at the request of S. Shafer, after
reading the correspondence with Epstein from 2026-09-01 onward.

Everything above this entry validates the **baseline** — Gas Man's stepping
restated in R. The app runs a different routine, `advanceClosedFormGas()`. These
five scenarios put that routine, at the resolution the app uses (601 points),
beside Gas Man. They are defined in `gasman_engine_scenarios.R`; the numbers are
in `gasman_engine_scenarios_results.csv`, and the settings to enter in Gas Man
are in `gasman_engine_scenarios_settings.csv`.

| # | Scenario | Why |
|---|---|---|
| 1 | Sevoflurane 2%, FGF 8, 30 min | The anchor; Epstein's Scenario 1 |
| 2 | Sevoflurane 2% + N2O 70% delivered, FGF 8, 30 min | Second gas effect; Epstein's Scenario 2 |
| 3 | Sevoflurane over 180 min: 2% at FGF 6, 3% from 30 min, 1.5% at FGF 2 from 60 min, vaporiser off at FGF 10 from 150 min | Setting changes and emergence, proposed by Epstein 2026-09-06; nothing before this was other than constant-setting wash-in |
| 4 | Desflurane 6% at FGF 4 for 10 min, then 8% at FGF 0.5 to 60 min | Low flow, where the circuit equation dominates |
| 5 | 100 kg, isoflurane 1.2%, FGF 2, VA 5.227, CO 6.534, 30 min | First comparison away from 70 kg; Gas Man's allometric defaults |

All: uptake and return on, ventilation 4 L/min and cardiac output 5 L/min
except scenario 5. The tables in this entry are for the **semi-closed** circuit,
which was the engine's only circuit when they were made; the ideal circuit was
added later the same day and has its own entry below.

### Result

Three values per compartment and time: **gasman**, the baseline at Gas Man's
native 6-second tick; **limit**, what the baseline converges to as the tick
shrinks (Richardson extrapolation from 0.1/16 and 0.1/32 min); **engine**, the
app engine. Worst difference over all five compartments and all reported times,
as a percentage of that compartment's peak:

| # | Agent | engine vs limit | gasman vs limit |
|---|---|---|---|
| 1 | sevoflurane | 0.001% | 2.1% |
| 2 | sevoflurane | 0.08% | 2.4% |
| 2 | nitrous oxide | 0.08% | 2.9% |
| 3 | sevoflurane | 0.17% | 1.2% |
| 4 | desflurane | 0.004% | 2.3% |
| 5 | isoflurane | 0.001% | 1.7% |

The engine sits on the limit. The 1–3% between the engine and Gas Man at its
native tick is Gas Man's own distance from that limit: it is largest in the
first minutes after a setting changes and decays (scenario 1, alveolar: 2.1% of
peak at 1 min, 0.07% at 30 min). The engine's own worst case, 0.17%, is the
circuit one minute after the vaporiser is turned off in scenario 3, where the
app's output step is 0.3 min; alveolar at that instant is within 0.12%.

### What this does and does not establish

* When this table was first written no run of Gas Man itself had been made;
  "gasman" above is the baseline. Gas Man itself was run later the same day --
  see the next section.
* **Nitrogen must be added as an agent in Gas Man for a like-for-like run.** The
  app engine always carries nitrogen and its washout feeds the uptake coupling.
  Gas Man does so only if nitrogen is one of the agents. Epstein's September
  runs of Scenarios 1 and 2 did not include it, so they are not directly
  comparable with the "gasman" column here: with nitrogen, alveolar sevoflurane
  in scenario 1 is 1.1791 at 5 min and 1.5887 at 30 min; without, as in the
  September grid, 1.1852 and 1.5931. That is a sixth difference between the app
  and Gas Man as usually run, beyond the five recorded earlier today. **Shafer
  reviewed it the same day and decided the app will retain nitrogen**; it is
  recorded with the other five in `docs/users-guide.md`.
* For the comparison the baseline's nitrogen starts at 78.07%, as the engine's
  does, not Gas Man's 80%. A run in Gas Man itself would start at 80 unless
  `Ambient` is edited.
* Oxygen is not part of the comparison; Gas Man does not model it.
* `tests/testthat/test-gas-scenarios.R` guards a fast subset: all five against
  the native tick (within 4%), and scenarios 1 and 5 against the limit (within
  0.1%).

### Run against Gas Man itself, the same day

Shafer asked for scenarios 3, 4 and 5 to be tested against the Gas Man C++.
Claude Code (Claude Fable 5.1) built Gas Man's own command-line runner,
`gasman_run`, from source -- `github.com/rasman/gasmanonline`, `gasman_api`,
commit `d3a2dd3` -- on `Grey` with the Rtools 4.5 g++ and cmake, and ran all
five scenarios through it. Neither the source nor the binary is in this
repository; the scenario files given to it are in `scenarios_engine/`, and
`runGasEngineScenarios(gasmanExe = , gasmanIni = )` repeats the run.

Conditions: `dt_ms` 6000, semi-closed, nitrogen added as an agent at 0%
delivered, and `Ambient` for nitrogen set to 78.07 in a private copy of
`gasman.ini` so that both sides start from room air. Output read at the 6-second
ticks; `gasman_run` prints six significant figures.

**Check that the build is Gas Man.** Scenario 1 without nitrogen, on the stock
`gasman.ini`, gives alveolar sevoflurane 1.1852 at 5 min and 1.59315 at 30 min,
which are Epstein's September values (1.185200, 1.593143) from the web edition.

**The R baseline restates Gas Man on the new scenarios too.** Worst difference
between `advanceGasManBaseline()` and `gasman_run`, as a percentage of each
compartment's peak:

| # | CKT | ALV | VRG | MUS | FAT |
|---|---|---|---|---|---|
| 1 | 0.0003 | 0.0003 | 0.0004 | 0.0024 | 0.061 |
| 2 | 0.0002 | 0.0003 | 0.0002 | 0.0022 | 0.061 |
| 3 | 0.0004 | 0.0005 | 0.0006 | 0.0018 | 0.060 |
| 4 | 0.0004 | 0.0005 | 0.0004 | 0.0008 | 0.036 |
| 5 | 0.0001 | 0.0002 | 0.0002 | 0.0031 | 0.060 |

That is the same picture as September -- agreement to the printed precision
everywhere but fat, where Gas Man's float32 accumulation shows -- now extended
to setting changes, emergence, low flow after a flow change, and 100 kg. These
had been listed as untested since the first entry in this file.

**The app engine against Gas Man itself.** Worst difference over all
compartments and times, percent of peak:

| # | Agent | engine vs Gas Man C++ | engine vs limit |
|---|---|---|---|
| 1 | sevoflurane | 2.1% | 0.001% |
| 2 | sevoflurane | 2.4% | 0.08% |
| 2 | nitrous oxide | 2.9% | 0.08% |
| 3 | sevoflurane | 1.1% | 0.17% |
| 4 | desflurane | 2.3% | 0.004% |
| 5 | isoflurane | 1.6% | 0.001% |

These are the baseline figures of the table above to two digits, as they must
be given how closely the baseline tracks the real program. Scenario 3, alveolar
sevoflurane, percent of one atmosphere:

| min | engine | Gas Man C++ | limit |
|---|---|---|---|
| 30 | 1.5514 | 1.5500 | 1.5514 |
| 60 | 2.3875 | 2.3861 | 2.3875 |
| 150 | 1.2157 | 1.2156 | 1.2157 |
| 155 | 0.3735 | 0.3849 | 0.3734 |
| 180 | 0.1390 | 0.1396 | 0.1390 |

The largest gap in relative terms is in emergence: five minutes after the
vaporiser is turned off Gas Man reads 3% above the engine, and the engine is on
the limit. That is Gas Man's 6-second tick lagging a fast change, not a
modelling difference.

**Observed in passing:** `gasman_run` looked for `gasman.ini` in the current
directory even when `--ini` gave another path, so the runner here changes to the
work directory first.

**Still not established.** The web and desktop editions were not run, only the
API build; Epstein has reported the API about 8e-05 from the web edition. Open,
closed and ideal circuits, liquid injection and flush remain untested, as do
agents other than sevoflurane, isoflurane, desflurane and nitrous oxide.

---

## 2026-10-05 — the ideal circuit becomes the app engine's default

Recorded by Claude Code (Claude Fable 5.1) at the direction of S. Shafer: "the
ideal circuit is real life."

### What changed and why

Until today the app engine implemented only Gas Man's default, "Semi-closed", in
which the whole breathing circuit is a single well-mixed 8 L volume:

    V_circ dF_circ/dt = Q (F_fgf - F_circ) + VA (F_alv - F_circ)

Fresh and exhaled gas are stirred together before any is vented, so some exhaled
gas is rebreathed at any fresh gas flow. With fresh gas flow equal to alveolar
ventilation the patient still inspires half exhaled gas. This was noticed when
the "no rebreathing" reference for the time-until-threshold tests needed a flush
of 100,000 L/min to converge.

A circle system does not behave like that: once fresh gas flow reaches minute
ventilation there is no rebreathing. Gas Man has this as its "Ideal" circuit,

    Q >= VA:   F_circ = F_fgf
    Q <  VA:   F_circ = f F_fgf + (1 - f) F_alv,   f = Q / VA

and `advanceClosedFormGas()` now implements it and uses it by default. The
semi-closed model remains available as `circuit = "semi-closed"`. This is a
seventh deliberate difference from Gas Man *as shipped* -- its default -- though
not from Gas Man, which offers both.

### Validation

`gasman_run` (built from `d3a2dd3`, as above) was run on all five scenarios with
`circuit` set to `Ideal`, alongside the baseline and the app engine with the
ideal circuit. The scenario files are `scenarios_engine/scenario_N_ideal.csv`;
the semi-closed ones are now `scenario_N_semiclosed.csv`. Results for both
circuits are in `gasman_engine_scenarios_results.csv`, which has gained a
`Circuit` column.

**The R baseline restates Gas Man's Ideal circuit.** Worst difference between
`advanceGasManBaseline(circuit = "ideal")` and `gasman_run`, percent of each
compartment's peak, over all five scenarios:

| CKT | ALV | VRG | MUS | FAT |
|---|---|---|---|---|
| 0.0004 | 0.0006 | 0.0005 | 0.003 | 0.062 |

The same as for the semi-closed circuit. The ideal circuit had been listed as
untested since the first entry in this file.

**The app engine, ideal circuit.** Worst difference over all compartments and
times, percent of peak:

| # | Agent | engine vs Gas Man C++ (Ideal) | engine vs limit |
|---|---|---|---|
| 1 | sevoflurane | 1.1% | 0.002% |
| 2 | sevoflurane | 1.4% | 0.25% |
| 2 | nitrous oxide | 2.2% | 0.28% |
| 3 | sevoflurane | 0.64% | 0.13% |
| 4 | desflurane | 1.5% | 0.02% |
| 5 | isoflurane | 1.2% | 0.0004% |

As before, the gap to Gas Man at its 6-second tick is Gas Man's distance from
the limit of its own equations, and the engine sits on the limit. The gap is
smaller than with the semi-closed circuit because there is no circuit volume to
integrate.

### What the change of default does to the answers

It is large. App engine, alveolar concentration of the primary agent, percent:

| # | min | ideal | semi-closed |
|---|---|---|---|
| 1 | 1 | 1.09 | 0.47 |
| 1 | 30 | 1.71 | 1.59 |
| 3 | 60 | 2.61 | 2.39 |
| 3 | 155 (5 min after vaporiser off) | 0.27 | 0.37 |
| 4 | 1 | 3.77 | 0.93 |
| 4 | 60 (low flow, 0.5 L/min) | 4.89 | 4.53 |
| 5 | 30 | 0.62 | 0.50 |

Wash-in and washout are both faster, most of all in the first minutes, because
the patient inspires the dial setting at once instead of waiting for an 8 L
volume to fill.

Reference supplied by Shafer: Feldman JM, Lampotang S, Hendrickx J. Is rebreathing prevented when FGF equals MV? APSF, 20 October 2022. https://www.apsf.org/article/is-rebreathing-prevented-when-fgf-equals-mv/

### Not established

* Whether the absence of ANY circuit volume is right. The ideal circuit has no
  lag between the vaporiser and the inspired gas; a real circuit has some, and
  the reference above says so.
* Where the threshold belongs was open when this entry was first written: the
  reference puts it at MINUTE ventilation, Gas Man's Ideal circuit at the
  ALVEOLAR ventilation it is given. Resolved the same day; see the next entry.
* The oxygen model with the ideal circuit has no Gas Man counterpart, as before.
  Below fresh gas flow = ventilation its alveolar steady state is the delivered
  fraction less 100 x VO2 / fresh gas flow, by mass balance.

---

## 2026-10-05 — minute ventilation, dead space, and exact time until threshold

Recorded by Claude Code (Claude Fable 5.1) at the direction of S. Shafer: "The
user sets minute ventilation, not alveolar ventilation. Let's set dead space at
30% of minute ventilation."

### Ventilation and dead space

The "ventilation" row of the dose table is now MINUTE ventilation, MV. Alveolar
ventilation is VA = MV (1 - d) with d = 0.3. Gas Man has no dead space; what it
calls ventilation is alveolar ventilation. This is an eighth deliberate
difference.

The ideal circuit with a dead space. Below the threshold the patient inspires
all the fresh gas and makes up the rest with exhaled gas, and exhaled gas is
alveolar gas diluted by the dead-space gas that came back unchanged:

    MV F_circ    = Q F_fgf + (MV - Q) F_exhaled
    MV F_exhaled = VA F_alv + d MV F_circ

    =>  F_circ = f F_fgf + (1 - f) F_alv,   f = Q / (VA + Q d)   for Q < MV
        F_circ = F_fgf                                           for Q >= MV

f reaches exactly 1 at Q = MV, so the threshold is at minute ventilation, as the
APSF reference has it. With d = 0 this is Gas Man's ideal circuit, f = Q / VA.

The default ventilation, used when a gas is entered without one, is the minute
ventilation whose alveolar part is Gas Man's default alveolar ventilation:
4 / 0.7 = 5.7 L/min at 70 kg, scaled by (weight / 70)^0.75.

**Every comparison with Gas Man in this directory is run with `deadSpace = 0`**,
so that the ventilation in a scenario is alveolar on both sides. The earlier
results in this file are therefore unchanged. The dead space itself has no Gas
Man counterpart to check against; it is covered by the engine's own tests: the
fresh-gas fraction and its limits, agreement with an independent integration,
and the oxygen mass balance (below the threshold the mixed-expired oxygen
settles at the delivered fraction less 100 x VO2 / fresh gas flow).

### Time until threshold, by simulation

The "time until threshold" lines for the inhaled agents and MAC were first
computed with the coupling between gases left out, which let the washout be
written as a sum of exponentials. Checked against making the same change in the
engine, that read long whenever nitrous oxide was washing out: with 70% nitrous
oxide after two hours, 18% for the nitrous oxide line and 10% for MAC.

Every line is now computed by `gasCoupledRecovery()`: the agent is turned off at
each time point in turn and the washout integrated forward with the engine's
equations, coupling included, in the limit of no rebreathing. All the time
points are integrated together as rows of one matrix, so a line costs a few
hundredths of a second. Against the engine with the same change made in the dose
table (70 kg, sevoflurane 2% with 70% nitrous oxide, minute ventilation 4):

| Line | at 30 min | at 119 min | engine |
|---|---|---|---|
| sevoflurane, vaporiser off alone | 11.96 | 17.31 | 11.96, 17.31 |
| nitrous oxide, turned off alone | 6.18 | 8.95 | 6.18, 8.95 |
| MAC, everything off | 11.19 | 49.47 | 11.19, 49.48 |

The reference run uses a flush of 100,000 L/min. That figure dates from the
semi-closed circuit, in which a finite flush always left some rebreathing
(22.6 min for MAC at 100 L/min against 20.08 in the limit, with alveolar
ventilation 4); with the ideal circuit any flow at or above the minute
ventilation is the limit.

---

## 2026-10-05 — oxygen consumed shrinks the gas volume

Recorded by Claude Code (Claude Fable 5.1) at the direction of S. Shafer: "as
oxygen is consumed, the gas volume shrinks. CO2 is added as oxygen is consumed,
but is removed by the CO2 absorber." Oxygen consumption is 3.5 mL/kg/min. A
ninth deliberate difference from Gas Man, which has no oxygen.

### The problem

Oxygen was a sink in the oxygen fraction with no effect on the gas volume. At
high flows that hardly matters. At low flows the fractions stopped adding up.
Shafer's case, 60 kg, 0.3 L/min oxygen with 1 L/min nitrous oxide, minute
ventilation 5.1, settled at alveolar oxygen 5.2%, nitrous oxide 73.8% and
nitrogen 0.4%: 79% in all.

### The model

Oxygen consumed is volume lost, like agent taken up, and joins the same coupling
term. Carbon dioxide is accounted for where it goes, with a respiratory quotient
of 0.8 (Claude's figure; not yet confirmed by Shafer):

* In the alveoli, carbon dioxide replaces most of the oxygen, so alveolar gas
  shrinks by VO2 - VCO2. That is added to the summed uptake of the other gases,
  and oxygen itself now takes the coupling.
* Carbon dioxide is not carried as a gas. Its alveolar fraction is taken at its
  steady value, 100 VCO2 / VA, about 5%.
* The absorber removes carbon dioxide from rebreathed exhaled gas, which shrinks
  by the fraction it held, VCO2 / MV; the ideal-circuit blend allows for that.
* The patient inspires the minute ventilation plus what is taken up, so the
  fresh gas flow that stops rebreathing is MV + uptake.

Equations are in the header of `R/advanceClosedFormGas.R`, section (4a).

### Checks

The same case, run for 24 hours:

| | oxygen | nitrous oxide | nitrogen | sum |
|---|---|---|---|---|
| inspired, % | 12.0 | 88.0 | 0.0 | 100.0 |
| alveolar, % | 6.3 | 89.0 | 0.0 | 95.3, plus carbon dioxide 4.7 = 100.0 |

Mass balance: 1.3 L/min in, 0.21 L/min of oxygen consumed, so 1.09 L/min leaves,
0.09 of it oxygen. Exhaled gas, carbon dioxide aside, is 8.26% oxygen and 91.74%
nitrous oxide in the model, which is 0.09/1.09 and 1/1.09.

Pure oxygen at 1 L/min: once nitrogen has washed out, alveolar gas is oxygen
plus carbon dioxide and nothing else, and inspired gas is 100% oxygen.

These are in `tests/testthat/test-gas-engine.R`.

### Effect on the Gas Man comparisons

None. They are run with `oxygenUptake = FALSE`, as they are with `deadSpace = 0`,
because Gas Man has neither. The app's own defaults now differ from Gas Man in
both respects, and at low flows by a good deal.

### Not established

* The respiratory quotient, and whether carbon dioxide at its steady value is
  good enough during rapid changes in ventilation.
* Where the pop-off sits relative to the absorber. Vented gas is taken to leave
  before the absorber, carrying its carbon dioxide with it.

