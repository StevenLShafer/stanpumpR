### The model

Hydralazine's parameters come from Reece and colleagues (*Clin Pharmacol Ther* 1980;28:769-778), who gave 0.375 mg/kg intravenously over 5 minutes to ten healthy volunteers (five slow and five fast acetylators, nine of them men, aged 22 to 39) and measured plasma hydralazine for 10 hours with a **specific** assay, one that separates the drug from its pyruvic acid hydrazone. Earlier assays measured the two together as "apparent hydralazine", which made the half-life look like hours. A two-compartment model was fitted to each subject. Acetylator phenotype, which governs oral first-pass metabolism, made no difference intravenously, so the model is the mean of all ten fits: a central volume of 2.77 L/kg, a peripheral volume of 3.13 L/kg, a clearance of 8.3 L/h/kg and an intercompartmental clearance of 9.4 L/h/kg. Its half-lives are 5 minutes and 38 minutes.

The central volume is far larger than the plasma volume, and the clearance (about 9.7 L/min at 70 kg) is more than the cardiac output. Both are real: hydralazine reacts with pyruvate in the blood within minutes and binds to the walls of arteries, so it leaves the plasma faster than any organ could clear it.

### What is plotted

Plasma hydralazine in ng/mL. The pyruvic acid hydrazone, most of what circulates after a dose, is not plotted. Doses are entered as labelled, as hydralazine hydrochloride. The paper does not say whether its milligrams were the salt or the base; if they were the base, every concentration is 23% high.

### Covariates

None was estimated. The parameters are per kilogram; with [the scaling switch](help:models/fat-free-mass) on they are scaled to fat-free mass, and with it off linearly in weight. Acetylator phenotype is not a covariate of the intravenous drug.

### Where to be careful

The other study with a specific assay, Ludden and colleagues' eight older hypertensive patients (1980), found **half this clearance** (4.4 L/h/kg) and did not report a distribution phase. The authors' 1982 review could not say whether the difference is the subjects or the methods. The same group's later study (1983, read in full) gave four doses each to six hypertensive men aged 47 to 57. At 0.3 mg/kg it found a clearance of 5.5 L/h/kg in plasma terms, two-thirds of Reece's, and no consistent change in clearance with dose up to 0.45 mg/kg. This model is of young, healthy volunteers, and may underpredict the concentrations in older hypertensive patients by about a third to a half.

The plasma curve is not the time course of the effect. In Reece's subjects the diastolic pressure fell 15 to 30 minutes after the infusion, and the fall lasts for hours after the plasma hydralazine has gone, which is attributed to binding in the arterial wall. The effect site peaks **15 minutes** after a bolus: that is clinical experience, not a measurement (the label gives 10 to 80 minutes to the largest fall), and an effect site on this plasma curve cannot reproduce how long the effect lasts. No band is drawn and no pressure is predicted. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
