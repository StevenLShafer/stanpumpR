### The model

Intravenous diltiazem's parameters come from Höglund and Nilsson (*Ther Drug Monit* 1988;10:401-409), who gave 20 mg of carbon-14-labelled diltiazem over 20 minutes to ten healthy volunteers and fitted two compartments. The abstract reports half-lives of 0.40 and 2.77 hours, a clearance of 992 mL/min, a central volume of 119 L and a steady-state volume of 209 L.

As for [labetalol](help:drugs/labetalol), five numbers fix a four-parameter model with one to spare: built from the clearance, the central volume and the two half-lives, it has a peripheral volume of 84.6 L and an intercompartmental clearance of 1.22 L/min, and a steady-state volume of 203.6 L, 2.6% from the 209 printed. The metabolites were too low to analyse after this intravenous dose, so the parent alone is plotted.

### Covariates

None in the source; the parameters are those of the study's adults, scaled to fat-free mass when [the scaling switch](help:models/fat-free-mass) is on.

### Where to be careful

Diltiazem's clearance **falls as the infusion rate rises**: the US label gives 64 L/h at 4.8 mg/h and 48 L/h at 13.2 mg/h in healthy volunteers, and in atrial fibrillation Dias and colleagues (*Circulation* 1992) found 42 L/h at 10 mg/h and 31 L/h at 15 mg/h. This model's 59.5 L/h is the low-dose value, so it suits a bolus and a short or modest infusion, and a long infusion at 15 mg/h will run higher than drawn. The abstract does not say whether the dose was the hydrochloride (as labelled, and assumed here) or the base, 9% apart. The source correlated the PQ interval with the logarithm of the concentration but gives no coefficients, so no effect is drawn. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
