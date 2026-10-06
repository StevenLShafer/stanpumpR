**Suggest Dosing**, the link in the header of the dose table, works backwards: you say what effect-site concentration you want and when, and it finds doses that get you there.

## Using it

1. Click **Suggest Dosing**.
2. Choose the **Drug**.
3. In the table, enter pairs of **Time** and **Target** effect-site concentration. Each target holds from its time until the next target's time.
4. Enter an **End Time** for the regimen. The OK button appears once you have.
5. Press **OK**. After a moment the dose table is replaced with a bolus at each target time and infusion rates between them, and the plot redraws.

The suggested regimen is applied directly to the dose table, bypassing the draft. Your previous doses for that drug are replaced; doses of other drugs are kept.

## Limitations, as the dialog states them

- **Decreasing targets are not supported.** A row that asks for a lower concentration than the one before is raised to the previous value. Falling to a lower concentration cannot be hurried by dosing, only by waiting, and the search does not model turning the infusion off and on again.
- **It takes a moment.** The doses are found by non-linear regression on repeated simulations.
- **The result is good, not provably optimal.** Better algorithms exist. For a target-controlled regimen computed exactly, the method of Shafer and Gregg is being added to the dose table as *Plasma target* and *Effect site target* units; see [In development](help:in-development).

## How it searches

The regimen is seeded with a unit bolus at each target time and unit infusions in the intervals, scaled in ten rounds of proportional correction, then refined by minimizing the squared difference between the simulated and the target effect-site concentration. The details are under [How Suggest Dosing searches](help:models/suggest-algorithm).

## Example

Choose remifentanil, enter a target of 4 ng/mL at time 0 and 2 ng/mL at 30 minutes, and an end time of 60. The suggestion is a bolus and a high initial rate, falling to a maintenance rate, with a step down at 30 minutes that the effect site reaches only gradually.
