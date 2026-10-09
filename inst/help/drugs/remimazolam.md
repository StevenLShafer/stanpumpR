### The model

Remimazolam's parameters are from Eleveld and colleagues (*Br J Anaesth* 2025;135:206-217), a pooled population model. For a 70 kg, 170 cm, 35-year-old man the volumes are 4.31, 12.3 and 18.6 L and the clearances 1.12, 1.45 and 0.298 L/min.

### Covariates

- **Size.** By default, with *Adjust weight to fat-free mass* ticked, the volumes scale with the patient's fat-free mass relative to the 70 kg, 170 cm reference man and the clearances with that ratio to the 0.75 power, so height, age and sex affect them through fat-free mass. The published model scales on total body weight instead: volumes in proportion to weight / 70 and clearances to (weight / 70)^0.75. Unticking the box restores the published scaling.
- **Age** increases V3 exponentially (about 0.7 per cent per year from 35).
- **Sex**: women have a higher elimination clearance (by a factor of e^0.163, about 18 per cent) and a larger V3 (by about 33 per cent).

The intercompartmental clearances follow their volumes to the 0.75 power, so the age and sex terms on V3 raise Q3 as well.

Two covariates in the published model are deliberately ignored in the code. One is the reduction of clearance with co-administered opioids. The other is hepatic impairment, which in the published model does not act on clearance: a Pugh-Child score above 8 increases V3 by a factor of e^0.824 (about 2.3), and Q3, which follows V3, by about 1.9. The simulation assumes normal hepatic function.

### Effect site

The time to peak effect is 2.5 minutes.

### Typical concentrations

The shaded band is 0.3 to 0.6 mcg/mL, with the *Typical* value that draws the *Mid* band at its midpoint, 0.45 mcg/mL. The recovery threshold is 0.2 mcg/mL. See [the remimazolam sedation scenario](scenario:remimazolam-sedation).

### Where to be careful

Remimazolam is hydrolysed by tissue esterases to an inactive metabolite, which gives it a short and context-insensitive offset compared with midazolam. The model will show this. Its pharmacodynamic interaction with opioids, which is strong, is not in the interaction panel, which is for propofol only.
