**Suggest Dosing**, the link in the header of the dose table, works backwards: you say what effect-site concentration you want and when, and it finds doses that get you there.

## Using it

1. Click **Suggest Dosing**. It is offered when the time unit is minutes or hours.
2. Choose the **Drug**. Only drugs it can target are listed: those with an effect site and with intravenous bolus and infusion units. Drugs plotted as plasma concentrations only (the antibiotics, the corticosteroids, mannitol, and the prodrugs codeine and tramadol), oral-only drugs and the inhaled agents are not.
3. In the table, enter pairs of **Time** and **Target** effect-site concentration, with the times in the dose table's time unit. Each target holds from its time until the next target's time.
4. Enter an **End Time** for the regimen. The OK button appears once you have.
5. Press **OK**. After a moment the dose table is replaced with a bolus at each target time and infusion rates between them, ending with a rate of zero at the end time, and the plot redraws.

The suggested regimen is applied directly to the dose table, bypassing the draft. Your previous doses for that drug are replaced; doses of other drugs are kept.

## How the targets are read

- **A row with a blank time is ignored**, and so is a row whose target is blank or zero. A time of **0** is a real target, at the start of the plot.
- **Targets at or after the End Time are ignored.** If none is left, no doses are suggested.
- **Two rows at the same time:** the one lower in the table is used, as for a target-controlled infusion.
- **Decreasing targets are not supported.** A row that asks for a lower concentration than the one before is raised to the previous value. Falling to a lower concentration cannot be hurried by dosing, only by waiting, and the search does not model turning the infusion off and on again.
- **Nothing runs past the End Time.** Every rate change falls before it, and the last row is an infusion rate of zero at the End Time. A target that leaves less time before the next target (or the end) than the drug takes to reach its peak effect gets a bolus with no infusion of its own.

## Limitations, as the dialog states them

- **Decreasing targets are not supported**, as above.
- **It takes a moment.** The doses are found by non-linear regression on the simulated effect site.
- **The result is good, not provably optimal.** Better algorithms exist. For a target-controlled regimen computed exactly, the method of Shafer and Gregg is in the dose table as the *Plasma target* and *Effect site target* units; see [Target-controlled infusion](help:tci).

## How it searches

The regimen is seeded with a unit bolus at each target time and unit infusions in the intervals, scaled in ten rounds of proportional correction, then refined by minimizing the squared difference between the simulated and the target effect-site concentration. The details are under [How Suggest Dosing searches](help:models/suggest-algorithm).

## Example

With the time unit in minutes, choose remifentanil, enter a target of 4 ng/mL at 1 minute and 6 ng/mL at 30 minutes, and an end time of 60. For the default patient the suggestion is a bolus of about 72 mcg at 1 minute; an infusion of about 0.18 mcg/kg/min from 3 minutes, easing to 0.16 mcg/kg/min at 8 minutes; a second, smaller bolus of about 35 mcg at 30 minutes, with the rate raised to about 0.24 and then 0.23 mcg/kg/min; and a rate of zero at 60 minutes. The effect site reaches each target within a couple of minutes, holds it to within a few per cent, and falls once the infusion stops.
