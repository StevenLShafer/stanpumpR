### The model

Intravenous labetalol's parameters come from Kanto and colleagues (*Int J Clin Pharmacol Ther Toxicol* 1981;19:41-44), who gave 1.5 mg/kg to four healthy volunteers and fitted two compartments. The abstract reports a distribution half-life of 4.9 minutes, an elimination half-life of 4.9 hours, a clearance of 24.8 mL/min/kg, a central volume of 1.1 L/kg and a steady-state volume of 9.41 L/kg.

That is five numbers for a model with four, so one of them is a check. The model is built from the clearance, the central volume and the two half-lives, which gives a peripheral volume of 7.92 L/kg and an intercompartmental clearance of 0.117 L/min/kg; their sum with the central volume is 9.02 L/kg, 4% from the 9.41 printed. Building it from the steady-state volume instead would divide by a difference of two nearly equal numbers and miss the measured central volume by a third, so the measured central volume is used.

### Covariates

None in the source. Every parameter is per kilogram, which is this model's own size term; with [the scaling switch](help:models/fat-free-mass) on it is applied to the fat-free-mass weight, and with it off to total body weight.

### Where to be careful

This model rests on the **abstract**: the full text, which would settle whether the doses were the hydrochloride (as labelled, and assumed here) or the base, 11% apart, and how the drug was assayed, could not be read. Four healthy volunteers are not a population, and surgical patients clear labetalol about half as fast: after aortobifemoral bypass Chauvin and colleagues (*Br J Anaesth* 1987) measured 13.1 mL/kg/min and a plateau of 264 ng/mL on 0.2 mg/kg/h, about twice what this model draws. The source related the fall in blood pressure to the peripheral compartment, but gives no coefficients, so no effect site or band is drawn. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
