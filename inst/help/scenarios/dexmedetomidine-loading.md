## What to look for

A 70-year-old woman given dexmedetomidine 1 mcg/kg as a bolus and 0.5 mcg/kg/hr for two hours. The plasma line (dashed) spikes with the bolus; the effect site (solid, tPeak 10 minutes) rises smoothly to a peak in the band at about ten minutes and is then held by the infusion. In practice the loading dose is given over ten minutes rather than as a bolus, precisely because the plasma spike causes hypertension and bradycardia while the effect site is still catching up; the plot shows why the slow infusion loses nothing in onset.

After the infusion stops at 120 minutes the effect site falls out of the band in about an hour, and the **time until threshold** line shows the fall to 0.4 ng/mL. Dexmedetomidine's slow intercompartmental clearance to its large slow compartment gives it a long tail: the plasma is still well above zero at four hours.

## Try next

- Replace the bolus with an infusion of 6 mcg/kg/hr for the first 10 minutes (rows: 6 mcg/kg/hr at 0, 0.5 mcg/kg/hr at 10). The effect-site curve is almost the same; the plasma peak is a third as high.
- Change the age to 30. Nothing changes: the adult model has no covariates. The elderly's greater sensitivity to dexmedetomidine's hemodynamic effects is pharmacodynamic and not represented.
- Change the age to 6 months (set the unit to months) and the weight to 7 kg, add the Events panel, and enter *CPB Start* at 30 minutes and *CPB End* at 90. The infant model has separate parameters on bypass, and the concentration rises while the clearance is suppressed.

## Background

[Dexmedetomidine](help:drugs/dexmedetomidine) has two models, adult and infant, and its times to peak effect are described in the code as guesses. [Events that change the kinetics](help:models/pk-events) explains the bypass parameters.
