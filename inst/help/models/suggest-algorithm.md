*Suggest Dosing* takes a drug, a set of target effect-site concentrations each holding from its time, and an end time, and returns a dose table that approximately achieves them. This page describes what it does; [Suggest Dosing](help:suggest-dosing) describes how to use it.

## Which drugs

The dialog lists only the drugs the search can target: those whose model has an effect site (ke0 greater than zero), with intravenous bolus and infusion units in the drug library. A drug with no effect site (the antibiotics, the corticosteroids, mannitol, and the prodrugs codeine and tramadol) has no effect-site concentration to fit; an oral-only drug or an inhaled agent has no bolus and infusion to fit it with.

## Cleaning the targets

Times and targets are parsed as the dose table parses them. Rows with a blank time, or a blank or zero target, are dropped; a time of 0 is a target at the start. Rows at or after the end time are dropped. Of several rows at the same time, the last one entered is kept, as for a [target-controlled infusion](help:tci). The rest are sorted, and any target lower than the one before is raised to the previous value: decreasing targets are not supported.

The search then works from the first target, which it treats as time zero; nothing of the drug is given before it, because the suggestion replaces the drug's previous rows.

## The shape of the regimen

For each target time the regimen gets a **bolus** at that time and up to **two infusion rates** in the interval that follows, which runs to the next target time or to the end time: one from the target time plus tPeak, and one a fifth of the way from there to the end of the interval. Both are rounded to whole minutes from the first target. A rate change that would fall at or after the end of its interval is left out, so the rates of one interval never run into the next, and an interval shorter than tPeak gets its bolus alone. A drug whose model gives ke0 directly, with no tPeak, starts its infusion with the bolus.

The regimen ends with a single infusion rate of **zero at the end time**. It is not part of the search, and as every other rate change is before the end it is the only infusion row there. That matters because the simulator adds the rates of infusion rows entered at the same time: a zero alongside a non-zero rate would not stop the infusion. The bolus is in the drug's bolus units and the infusions in its infusion units, from the drug library.

The idea is the one behind the Shafer-Gregg effect-site targeting algorithm: a bolus reaches the target at the time of peak effect, and an infusion then holds it; a second rate soon after lets the regimen correct for the bolus's redistribution.

## Reading the effect site

The kinetics are linear in the doses, so the effect-site concentration of any regimen of this shape is the sum of each row's own curve, for that row at a unit dose and every other row at zero, scaled by the row's dose. Each row's curve is simulated once, with all the rows present, so that every dose time is a point on the simulation's timeline and the effect site is read there exactly. The search then evaluates a regimen as a weighted sum of those curves rather than by simulating it again, and the final, rounded regimen is simulated directly as a check.

## Proportional correction

All doses start at 1. Ten times over, each dose is multiplied by the ratio of the target in force at its time to the effect-site concentration at the next later rate change (or at the end time): a bolus is judged where the infusion takes over, an infusion where the next rate replaces it. A dose whose effect site reads zero there is left as it is. This gets each dose into the right range quickly.

## Non-linear regression

The regimen is then refined with R's `nlm()` optimiser. The objective is a concentration and nothing else: the squared difference between the effect-site concentration and the target in force, at 100 evenly spaced times from the first target to the end time, with the concentration between the simulation's own time points interpolated linearly, as the plot is. It is divided by the number of times and by the square of the highest target. That does not move the minimum, but it puts the objective on the same scale for every drug and concentration unit, so that the optimiser's stopping rules mean the same thing for each; each dose is likewise scaled by its starting value, because a bolus in mg and a rate in mcg/kg/min can differ by orders of magnitude. A negative dose counts as zero. The optimiser stops when its gradient or its steps fall below its default tolerances, or after 1000 iterations. The doses are rounded to three significant figures and written into the dose table, replacing the drug's previous rows.

## Why it is "good but not provably optimal"

The objective is a least-squares fit, which treats overshoot and undershoot alike and spreads error evenly in time. It does not minimise the time to target, limit the peak plasma concentration, or guarantee no overshoot, and a different parameterisation of the regimen (more rate changes, or rates at different times) would do better. It is the best fit only for the rate-change times above, only at the 100 times it is judged at, and only to the optimiser's tolerance and the rounding of the doses. The dialog says so. For the drugs that offer *Plasma target* and *Effect site target* units, a [target-controlled infusion](help:tci) computes the loading dose and maintenance infusion directly from the kinetics at every update, and is the right tool for a target.

## Reference

Shafer SL, Gregg KM. Algorithms to rapidly achieve and maintain stable drug concentrations at the site of drug effect with a computer-controlled infusion pump. *J Pharmacokinet Biopharm* 1992;20:147-169.
