### The model

Remimazolam's parameters are from Eleveld and colleagues (*Br J Anaesth* 2025;135:206-217), a pooled population model. At 70 kg and 35 years the volumes are 4.31, 12.3 and 18.6 L and the clearances 1.12, 1.45 and 0.298 L/min.

### Covariates

- **Weight** scales the volumes linearly and the clearances allometrically to the 0.75 power.
- **Age** increases V3 exponentially (about 0.7 per cent per year from 35).
- **Sex**: women have a higher elimination clearance (by a factor of e^0.163, about 18 per cent) and a larger V3 (by about 33 per cent).

Two covariates in the published model are deliberately ignored in the code: the reduction of clearance with co-administered opioids, and the reduction with hepatic impairment by Child-Pugh class.

### Effect site

The time to peak effect is 2.5 minutes.

### Typical concentrations

The shaded band is 0.3 to 0.6 mcg/mL, with the *Typical* value that draws the *Mid* band at its midpoint, 0.45 mcg/mL. The recovery threshold is 0.2 mcg/mL. See [the remimazolam sedation scenario](scenario:remimazolam-sedation).

### Where to be careful

Remimazolam is hydrolysed by tissue esterases to an inactive metabolite, which gives it a short and context-insensitive offset compared with midazolam. The model will show this. Its pharmacodynamic interaction with opioids, which is strong, is not in the interaction panel, which is for propofol only.
