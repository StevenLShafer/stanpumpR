### The model

Jullien and colleagues (*J Antimicrob Chemother* 2017;72:181-189, [doi:10.1093/jac/dkw352](https://doi.org/10.1093/jac/dkw352)) studied the micafungin arm of the EMPIRICUS trial: 99 adults in 19 French intensive-care units, on mechanical ventilation, with sepsis and *Candida* colonisation, given 100 mg over 1 hour. They were 29.9 to 92.7 years old and weighed 48 to 141 kg. Two compartments describe them, at 84 kg:

- clearance 1.34 L/h × (weight / 84)^0.59
- central volume 11.8 L × (weight / 84)^0.61
- peripheral volume 7.68 L × (weight / 84)^0.67
- intercompartmental clearance 4.67 L/h

A serum albumin of 25 g/L (2.5 g/dL) or less raises the clearance and both volumes by 14%, and a SOFA score of 10 or more lowers the clearance by 25%.

### When stanpumpR uses it

This is one of [two micafungin models](help:drugs/micafungin). stanpumpR uses it for most adults.

### Covariates

- **Weight** on the clearance and both volumes, as fitted. With [the scaling switch](help:models/fat-free-mass) on, the weight terms read the pharmacokinetic weight, and the intercompartmental clearance is scaled to fat-free mass at the 70 kg reference man (the source prints no height for its cohort).
- **Serum albumin** from the Patient Profile. Blank means above 2.5 g/dL, the model's reference, although the cohort's median was 21 g/L.
- **SOFA score** has no field and is held below 10. A patient with a SOFA score of 10 or more is drawn about a quarter too low.
