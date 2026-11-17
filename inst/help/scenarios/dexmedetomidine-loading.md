## What to look for

A 70-year-old woman is given dexmedetomidine 1 mcg/kg as a bolus, then 0.5 mcg/kg/hr for two hours. The plasma line (dashed) spikes with the bolus. The effect site (solid, tPeak 14.9 minutes) rises smoothly to about 0.94 ng/mL, just above the band, at about 17 minutes. It then drifts down to 0.70 ng/mL by the end of the infusion, as drug continues to move into the peripheral compartments. In practice the loading dose is given over ten minutes rather than as a bolus. The plasma spike causes hypertension and bradycardia while the effect site is still catching up, and the plot shows that the slow infusion loses little in onset.

After the infusion stops at 120 minutes the effect site falls slowly. The **time until threshold** line puts its fall to 0.4 ng/mL, the bottom of the band, about an hour after the infusion stops. The plasma is still above 0.2 ng/mL at four hours.

## Try next

- Replace the bolus with an infusion of 6 mcg/kg/hr for the first 10 minutes (rows: 6 mcg/kg/hr at 0, 0.5 mcg/kg/hr at 10). The effect site peaks a little later and a little lower (0.90 ng/mL at about 24 minutes), and the plasma peak is about 60% as high.
- Change the age to 30. Almost nothing changes. Apart from size, the model's only age term is the maturation of clearance, which is complete in adults. The elderly's greater sensitivity to dexmedetomidine's hemodynamic effects is pharmacodynamic and not represented.
- Change the age to 6 months (set the unit to months) and the weight to 7 kg. The same model applies at every age, with the clearance scaled for size and maturation. To see bypass, change the drug to *dexmedetomidineInfantCPB*, add the Events panel, and enter *CPB Start* at 30 minutes and *CPB End* at 90. That model has separate parameters on bypass, and the concentration rises while clearance is suppressed.

## Background

[Dexmedetomidine](help:drugs/dexmedetomidine) uses Morse and colleagues' universal model, with the time to peak effect from Colin and colleagues' sedation data. [Dexmedetomidine in infants on bypass](help:drugs/dexmedetomidineInfantCPB) is Zuppa's model, and [Events that change the kinetics](help:models/pk-events) explains its bypass parameters.
