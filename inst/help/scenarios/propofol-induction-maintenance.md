## What to look for

The bolus takes the effect site to about 9 mcg/mL within two minutes. The infusion alone, at 150 mcg/kg/min, would have taken much longer: trace the effect-site line after the bolus has redistributed, at around 10 to 15 minutes, and see that it is being held up by the infusion, not by the bolus. This is what a loading dose is for: it bridges the gap while the infusion fills the peripheral compartments.

The infusion never reaches a plateau. Over the first hour the concentration keeps drifting upward as the slow compartment fills. The step down to 100 mcg/kg/min at 30 minutes is what a clinician does by feel: the same infusion rate that was right at 15 minutes is too much at 45.

The **time until threshold** line (thin, black, read on the right axis) climbs through the infusion. At 10 minutes, stopping the infusion would bring the effect site down to 1 mcg/mL in about 14 minutes; at 30 minutes it would take about 23, and at 90 minutes, after an hour at the lower rate, about 25, because the peripheral compartments are now full and return drug to the plasma as it falls. Hover on the line at 30 and 90 minutes and compare.

When the infusion stops at 90 minutes, the effect site falls to the threshold at the time the line predicted.

## Try next

- Set *Max time* to 4 hours and extend the infusion to 3 hours. Watch the time-until-threshold line keep climbing.
- Remove the step down at 30 minutes and see how far the concentration drifts above the band.
- Add remifentanil (1 mcg/kg at 0, 0.15 mcg/kg/min from 0) and the **Interaction** panel. The propofol concentration needed falls; see [the TIVA scenario](scenario:tiva-remifentanil-propofol).

## Background

[Time until threshold](help:models/recovery) explains the black line and the context-sensitive half-time it generalises. The model is [Eleveld's propofol model](help:drugs/propofol).
