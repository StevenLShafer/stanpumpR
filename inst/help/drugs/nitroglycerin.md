### The model

Intravenous nitroglycerin's parameters come from Armstrong, Watts and Moffat (*Br J Clin Pharmacol* 1983;16:385-390), who infused 21 mcg/min into ten patients with congestive cardiac failure. Plasma nitroglycerin rose to about 4 ng/mL by 30 minutes and stayed there, and a one-compartment model fitted to the data gave a clearance of 6.2 L/min and a time constant of 8.1 minutes for the approach to steady state. A one-compartment model has just those two parameters: the volume is the clearance times the time constant, 50.2 L, and the half-life 5.6 minutes. The plateau it predicts at the study's rate, 3.4 ng/mL, matches the one observed.

### Covariates

None in the source; the parameters are those of the study's adults, scaled to fat-free mass when [the scaling switch](help:models/fat-free-mass) is on.

### Where to be careful

Nitroglycerin's plasma kinetics are notoriously erratic. In eight healthy volunteers McNiff and colleagues (*J Pharm Sci* 1981) found an apparent clearance of 0.3 to 1 L/min/kg, **three to ten times** the one drawn here, a volume of about 3 L/kg and a half-life of about 3 minutes, with two subjects' concentrations oscillating too much to analyse. Clearance falls with cardiac output, which is low in heart failure, and depends on where the blood is drawn. So this is the one fitted, consistent set of numbers the literature offers, in the patients most likely to receive the drug, not a precise one. The samples were **arterial**, and the same group found venous nitroglycerin about 60% lower. The infusion ran through Teflon tubing, which does not adsorb the drug, so the slow rise is real. But the one-compartment model has a limit: after an infusion stops the authors measured half-lives of 1 to 3 minutes, faster than the model's 5.6, so the curve falls more slowly after a stop than the drug does. The pulmonary pressures followed the concentration with the same time constant, so there is no delay to model, but no coefficients are given and no effect is drawn. Tolerance develops within a day of continuous infusion. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
