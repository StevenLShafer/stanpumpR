*Suggest Dosing* takes a drug, a set of target effect-site concentrations each holding from its time, and an end time, and returns a dose table that approximately achieves them. This page describes what it does; [Suggest Dosing](help:suggest-dosing) describes how to use it.

## Cleaning the targets

Times and targets are parsed as the dose table parses them. Rows with a zero time or a zero target are dropped, rows at or after the end time are dropped, and the rest are sorted. Any target lower than the one before is raised to the previous value: decreasing targets are not supported.

## The shape of the regimen

For each target time the regimen gets a **bolus** at that time and **two infusion rates** in the interval that follows: one from the target time plus tPeak, and one a fifth of the way from there to the next target time. The last infusion is set to zero at the end time. The bolus is in the drug's bolus units and the infusions in its infusion units, from the drug library.

The idea is the one behind the Shafer-Gregg effect-site targeting algorithm: a bolus reaches the target at the time of peak effect, and an infusion then holds it; a second rate soon after lets the regimen correct for the bolus's redistribution.

## Proportional correction

All doses start at 1. Ten times over, the regimen is simulated and each dose is multiplied by the ratio of its target to the effect-site concentration just before the next dose. This gets each dose into the right range quickly.

## Non-linear regression

The regimen is then refined with R's `nlm()` optimiser, minimising the sum over the time grid of the squared difference between the simulated effect-site concentration and the target in force at that time. Doses are clamped at zero. The result is rounded and written into the dose table, replacing the drug's previous rows.

## Why it is "good but not provably optimal"

The objective is a least-squares fit, which treats overshoot and undershoot alike and spreads error evenly in time. It does not minimise the time to target, limit the peak plasma concentration, or guarantee no overshoot, and a different parameterisation of the regimen (more rate changes, or rates at different times) would do better. The dialog says so. For the drugs that will support it, the target-controlled infusion in development computes the loading and maintenance infusion exactly from the kinetics, every ten seconds, and is the right tool for a target; see [In development](help:in-development).

## Reference

Shafer SL, Gregg KM. Algorithms to rapidly achieve and maintain stable drug concentrations at the site of drug effect with a computer-controlled infusion pump. *J Pharmacokinet Biopharm* 1992;20:147-169.
