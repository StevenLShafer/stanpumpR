A simulator that is wrong is worse than no simulator. This page says what has been checked, and how.

## The unit tests

The repository has a test file for every drug (a metabolite or prodrug is tested in its parent's file) and test files for the engines, the input handling, the server and the help, run with `devtools::test()` and, on every pull request, by the R-CMD-check workflow on Linux, macOS and Windows.

- **Drug tests** pin each model's parameters at reference patients to the values first computed from the paper, so that a later edit cannot silently change a model. A drug that switches models on a covariate is pinned on both sides of the switch.
- **Engine tests** check the closed-form advance against known results: a single compartment against its analytic solution, a bolus followed by an infusion against the sum of the two, state carried across an event boundary against a simulation that never had the boundary, oral absorption against the limit of fast absorption.
- **Michaelis-Menten tests** check the numerical engine of the saturable drugs (phenytoin, GHB, exenatide and the gliptins) against the closed-form engine when elimination is effectively linear, against the analytical solution after a bolus, against the steady state of a constant infusion, and by mass balance.
- **Effect-site tests** check that the ke0 solved from a time to peak effect does produce a peak at that time.
- **Time-until-threshold tests** check the recovery calculation against stopping the drug in the simulation itself and reading off when it crosses the threshold.
- **TCI tests** check that every TCI drug reaches an effect-site target without overshoot and holds it, and that a closed loop of calls to [the TCI API](help:tci-api) reproduces the app's TCI schedule rate for rate.
- **Table and input tests** cover the parsing of times and doses, the cleaning of the dose table, and the gas rules.
- **Help tests** render every help page, resolve every internal link, require a narrative for every drug, and check every teaching scenario against the drug library and run it through the engine.

## The inhaled-gas engine

The gas engine was validated in three layers.

1. **Against the equations.** The closed-form advance agrees with an independent fourth-order Runge-Kutta integration of the same differential equations; mass is conserved; single-compartment and steady-state limits are reproduced by hand calculation.
2. **Against a transcription of GasMan.** `R/advanceGasManBaseline.R` is a line-by-line transcription of GasMan's own update, kept as a reference. The engine and the baseline converge to the same answer as the step size shrinks (`test-gas-convergence.R`); where they did not, the difference was traced to a parameter (the blood-flow fractions) and corrected.
3. **Against GasMan itself.** Richard Epstein ran GasMan through its API on the same scenarios. In the first run (sevoflurane 2 per cent with nitrous oxide 50 per cent, 8 L/min, 30 minutes) the two agreed on alveolar sevoflurane to 1.5 parts in a million, with the residual shown to be GasMan's single-precision arithmetic: its "delivered" column, which has no model in it at all, carried the same rounding. Five further scenarios covered low flows, the ideal and semi-closed circuits, other weights, other agents and changing settings.

The running record, including what has *not* been checked, is `inst/validation/VALIDATION.md` in the repository, with the scenario drivers and their results beside it. The deliberate differences from GasMan, which are not errors and will remain, are listed under [Where the gas engine differs from GasMan](help:models/gas-differences).

## What has not been validated

- The models themselves are only as good as their papers. stanpumpR checks that it computes what the paper says, not that the paper is right for your patient.
- Several times to peak effect are described in the code as guesses (ketamine, infant dexmedetomidine, oxytocin). Their drug pages say so.
- The opioid-MAC interaction is an approximate placeholder fitted to nine points.
- The intramuscular parameters for hydromorphone have no direct citation and are provisional.
- The regional anesthesia (RA) absorption of each local anesthetic is fitted to, or matched to the peak of, a single published block; bupivacaine's and ropivacaine's rates are labelled provisional in the code.
- The oxytocin human model is unpublished data.

## Reporting a problem

Please open an issue at the [GitHub repository](https://github.com/StevenLShafer/stanpumpR/issues), with the URL of a simulation that shows it (see [Sharing a simulation by URL](help:sharing)), or write to steven.shafer@stanford.edu.
