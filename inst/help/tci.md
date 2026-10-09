A target-controlled infusion (TCI) pump holds a concentration rather than a rate: you set the concentration you want in the plasma or at the site of drug effect, and the pump's pharmacokinetic model works out the infusion needed to reach it quickly and then hold it. stanpumpR simulates such a pump for propofol, remifentanil, alfentanil, sufentanil, fentanyl, lidocaine, hydromorphone, etomidate and ketamine. The method is Shafer and Gregg's (*J Pharmacokinet Biopharm* 1992;20:147), as implemented in the original STANPUMP.

## Setting a target

Enter a dose-table row for the drug with units **Plasma target** or **Effect site target**. The dose is then the target concentration, in the drug's concentration units per millilitre (mcg/mL for propofol, ng/mL for the opioids). From that time the controller takes over and recomputes the rate every ten seconds. On plots longer than about five and a half hours the interval lengthens once the plasma is being held steady, growing by a tenth at each update up to 1/2000 of the plot (43 seconds on a 24-hour plot, at most ten minutes); it returns to ten seconds at every new target, bolus or change of mode.

- **Plasma target.** The pump gives the bolus that fills the central compartment and then the infusion that holds the plasma at the target.
- **Effect site target.** The pump gives the larger bolus that makes the effect-site concentration *peak* at the target, without overshoot. The peak is found on a one-second grid, so the effect site can exceed the target very slightly: by less than 0.1 per cent in the drugs and patients tested. The plasma overshoots, falls while the effect site rises, and the two meet at the target at the time of peak effect. After that, holding the plasma at the target holds the effect site there, so that is what the controller does: it hands off from effect-site to plasma control once the effect site is within 5 per cent of the target. (The effect-site solution is ill-conditioned at steady state and would otherwise alias between a huge rate and zero.)

## Changing and stopping

- A **higher** target gives another, smaller loading dose.
- A **lower** target turns the pump off until the concentration has fallen to the new target, then resumes.
- A target of **0 stops** the TCI infusion.
- **Boluses are allowed** during a TCI infusion: the concentration rises, and the controller gives no more drug until it is back at the target.
- **Manual infusions are not**: setting a target zeroes any infusion running for that drug, and entering an infusion row stops the TCI infusion (the rate panel drops to zero there, and the manual rate runs alone). The two cannot run together.

## The rate panel

A **TCI rate panel** appears below the concentration panels for each drug under TCI, in the drug's colour, showing the pump rate the controller ran. Hover over it for the rate at any moment. The loading dose is written as a number rather than drawn, because its rate over a ten-second interval would flatten the rest of the panel. These rate rows are kept out of the dose table, where the ten-second changes would make it unusable, but they are merged back in when a slide is emailed, so the exported dose table lists the pump's complete programme.

## Two things the pump does not model

It does not know about oral, intramuscular or intranasal doses of the same drug, which it treats, as a real pump would, as an unexpected addition. And it has no upper limit on its rate, so the loading dose is always delivered within one ten-second interval.

## TCI and Suggest Dosing

[Suggest Dosing](help:suggest-dosing) is the older way to work backwards from a concentration, by searching over repeated simulations. For a drug that offers target units, a TCI target row gives a better answer, computed directly from the closed-form model rather than by search, and faster. See [the propofol TCI scenario](scenario:tci-propofol).
