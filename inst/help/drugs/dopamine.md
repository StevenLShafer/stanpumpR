### The model

Dopamine's parameters come from MacGregor and colleagues at Wake Forest (*Anesthesiology* 2000;92:338-346): nine healthy men given 10 mcg/kg/min for 10 minutes and then 3 mcg/kg/min for 90 minutes, with 32 arterial samples each, fitted in NONMEM. The best model is two compartments with the central volume per kilogram: rate constants k10 1.10, k12 0.244 and k21 0.070 per minute and a central volume of 0.241 L/kg, which give a clearance of 0.265 L/kg/min, a peripheral volume of 0.84 L/kg and half-lives of 0.5 and 12 minutes. These reproduce every derived value the paper prints, including its steady-state rule, 3.77 ng/mL for each 1 mcg/kg/min.

### What is plotted

Plasma dopamine in ng/mL, for doses of dopamine hydrochloride as labelled (the paper does not state the basis). The model has no endogenous production term, so it is the drug given; baselines in these men ranged from almost nothing to 8 ng/mL. The half-minute lag between the infusion arm and the sampling artery is not drawn.

### Where to be careful

The authors' own conclusion is that weight-based dosing "does not yield predictable blood concentrations": at the same rate, concentrations varied 10- to 75-fold between these healthy, homogeneous men, half the measurements were more than 59% from the model's prediction, and the model overpredicted the 10 mcg/kg/min infusion, suggesting dose-dependent kinetics (as Le Corre and colleagues found in patients). Read the curve as the typical healthy man, never as a patient's concentration. Every parameter is per kilogram, applied to the fat-free-mass weight when [the scaling switch](help:models/fat-free-mass) is on. No effect is drawn. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
