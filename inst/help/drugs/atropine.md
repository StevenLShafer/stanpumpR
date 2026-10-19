### The model

Intravenous atropine's parameters come from Hinderling, Gundert-Remy and Schmidlin in Basel (*J Pharm Sci* 1985;74:703-710): three healthy young men, each given 1.35 and 2.15 mg as a 3-minute infusion weeks apart, with plasma atropine measured by GC-MS for 10 hours and urine for 36. A linear two-compartment model was fitted together with heart rate and saliva flow, and the means of the six experiments are a central volume of 19.1 L, a clearance of 1175 mL/min (renal 656, hepatic 519) and distribution rate constants of 0.560 and 0.0557 per minute, so a peripheral volume of 192 L. The paper's other means check the model: a steady-state volume of 211 L against 210, and a terminal half-life of 136 minutes against 132.

### Dose basis

The paper's doses are atropine **base**; US injections are labelled as **atropine sulfate**, of which 83.3% is base. A dose here is entered as labelled, and the model is rescaled for the salt, which is exact for a linear model.

### Where to be careful

Three young men. The authors found the distribution dose-dependent, with both volumes smaller at the higher dose, so the volumes are averages; clearance falls and the half-life lengthens with age, and renal clearance rises with urine flow, none of which is a covariate here. The racemate is measured, though only l-hyoscyamine is active. The companion paper (Part II) related heart rate and saliva flow to the amount in the **peripheral** compartment, not to an effect site, so no effect is drawn. The parameters are fixed values, scaled to fat-free mass when [the scaling switch](help:models/fat-free-mass) is on. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
