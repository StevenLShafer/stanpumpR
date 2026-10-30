### The model

Epinephrine's parameters come from Abboud and colleagues (*Crit Care* 2009;13:R120), a study of 38 consecutive adults with septic shock in a French medical intensive care unit, in whom epinephrine was the only catecholamine. Infusions started at 0.15 mcg/kg/min and were titrated to a mean arterial pressure of 65 to 75 mmHg, and at sampling ranged from 0.026 to 1.7 mcg/kg/min. Each patient gave one sample before the infusion and one at steady state, and a population analysis found a one-compartment model with linear elimination; saturable elimination did not improve the fit, so the concentration is proportional to the rate across that 65-fold range. The final model is a clearance of 127 L/h × (weight/70)^0.60 × (SAPS II/50)^-0.67 and a volume of 7.9 L, a half-life of 2.6 minutes.

### What is plotted

Plasma epinephrine in ng/mL, **above the patient's own**. The model's endogenous input implies a baseline of 0.34 nmol/L (0.06 ng/mL) at the reference patient, which is exactly the median the study measured before the infusion; it is under 1% of a typical infusion concentration and is not drawn. Doses are epinephrine base, as labelled in the US; the paper's dose rates are base too.

### Covariates

Weight, on clearance only, as the published power of 0.60; with [the scaling switch](help:models/fat-free-mass) on it is applied to the fat-free-mass weight, and the 7.9 L volume, which had no size term, is scaled by the library's volume factor. Severity of illness (SAPS II) lowered clearance, but it is not a Patient Profile field, so the model is evaluated at its reference score of 50. The study's patients averaged 64, at which clearance is 15% lower than plotted.

### Where to be careful

The data are steady-state concentrations, so the plateau a given rate reaches is well determined (clearance standard error 6%) and how quickly it gets there is not: the volume's standard error is 36%. A cardiac-arrest bolus is far outside the infusion data. The effect site peaks **1 minute** after a bolus, the clinical time to the immediate tachycardia; no study has measured it more finely. No band is drawn: the study fitted no concentration-effect relation, and the paper's own starting point is how unpredictable the pressure response to a given rate is in septic shock. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
