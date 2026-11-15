### The model

Treprostinil's parameters come from the FDA's pharmacometric review of Remodulin (NDA 21-272, 2002), fitted to healthy volunteers and to the 233 patients with pulmonary hypertension treated in the two pivotal 12-week studies of continuous subcutaneous infusion. Two compartments, scaled by ideal body weight: clearance 29.7 L/h, central volume 13.6 L, intercompartmental clearance 25.3 L/h and peripheral volume 37.1 L at 70 kg ideal body weight, the clearances scaling as (ideal body weight/70)<sup>1.52</sup> and the volumes in proportion. The source ignored subcutaneous absorption ("rapid enough to be ignored"), and the label reports intravenous and subcutaneous infusions bioequivalent at steady state, so one rate, **ng/kg/min**, serves both.

### Checks

The label gives a systemic clearance of "approximately 30 L/hour for a 70 kg person", a central volume of "approximately 14 L/70 kg ideal body weight", and concentrations of about 2,000 ng/L (2 ng/mL) at an average 9.3 ng/kg/min. This model gives 1.4 ng/mL at that rate for a 70 kg, 170 cm man. In 15 healthy subjects given 15 ng/kg/min intravenously for 150 minutes the peak was 1.57 ng/mL; this model gives 1.8 ng/mL.

### Covariates

Ideal body weight (Devine's formula; the review does not name one) from height and sex. Below 5 feet, where Devine's formula is not defined, the weight used is the pharmacokinetic weight. Total body weight does not enter, so [the scaling switch](help:models/fat-free-mass) has no effect. Hepatic impairment (clearance reduced by up to 80%) and CYP2C8 inhibitors (gemfibrozil doubles exposure) are not modelled.

### What is not offered

Inhaled treprostinil (Tyvaso) needs a pulmonary route the library does not have, and oral treprostinil (Orenitram) is an osmotic extended-release tablet with a bioavailability of about 17%.

### Dose and band

Remodulin is labelled in mg of treprostinil and dosed in ng/kg/min. No therapeutic range has been established; doses are titrated to symptoms and adverse effects.
