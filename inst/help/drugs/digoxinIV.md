### The model

Intravenous digoxin's plasma model comes from Cheng and colleagues in Philadelphia (*Pharmacotherapy* 1997;17:584-590): eighteen adults in three groups by measured creatinine clearance (below 10, 10-50 and above 50 mL/min), each given 7 or 10 mcg/kg intravenously and sampled for five to seven days. Two compartments were fitted to each group, and the paper's regressions on creatinine clearance give the clearance, 0.0676 + 0.0003 × CLcr L/h/kg, and the steady-state volume, 5.289 + 0.0115 × CLcr L/kg. The central volume is 0.6 L/kg; the intercompartmental clearance, which the paper does not print, is solved (0.615 L/h/kg) so that the normal-renal group's own means give its printed distribution rate, and it then returns that group's terminal half-life, about 46 hours against 49. Renal function is the Cockcroft-Gault estimate from the Patient Profile's serum creatinine, held to 0-140 mL/min.

### The effect site

Hornestam and colleagues (*Eur J Clin Pharmacol* 2003;58:747-755) related the fall in ventricular rate to an effect compartment in 105 patients given intravenous digoxin for acute atrial fibrillation. With their own plasma model, that compartment peaks **87 minutes** after a bolus, and the effect site here is solved from that time to peak. Their plasma model is not used because its renal slope was fixed at the boundary that keeps clearance non-negative, giving zero clearance at a creatinine clearance of 20 mL/min; Cheng's clearance has a non-renal floor.

### What is plotted

Serum digoxin in ng/mL and the effect site, with the 0.5-2 ng/mL range shaded; that range applies after distribution, six hours or more after a dose, when the effect site and the plasma have nearly equilibrated. The oral, apparent model is [digoxin](help:drugs/digoxin); children's is [intravenous digoxin in children](help:drugs/digoxinIVPediatric).

### Where to be careful

Eighteen subjects, with clearance explaining only 41% of the variance. Amiodarone, quinidine, verapamil and spironolactone all lower digoxin clearance and are not covariates. Every parameter is per kilogram, applied to the fat-free-mass weight when [the scaling switch](help:models/fat-free-mass) is on. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
