### The model

Wasmann and colleagues (*J Antimicrob Chemother* 2019;74:978-985, [doi:10.1093/jac/dky554](https://doi.org/10.1093/jac/dky554)) gave single 100 or 200 mg doses over 1 hour to 24 adults, half of them women: 8 of normal weight and 16 morbidly obese, the obese on the day of bariatric surgery. They were 22 to 61 years old and weighed 61.5 to 184 kg. Two compartments describe them, at 70 kg:

- clearance 0.690 L/h × (weight / 70)^0.74
- central volume 5.84 L × (weight / 70)^1.17
- peripheral volume 6.96 L × (weight / 70)^0.71
- intercompartmental clearance 7.15 L/h

Age, sex, lean body weight and BMI were tested and not retained.

### When stanpumpR uses it

This is one of [two micafungin models](help:drugs/micafungin). stanpumpR uses it for adults younger than 29.9 years, for patients heavier than 141 kg up to 61 years, and, as the nearer by age, for children, whom neither source studied.

### Covariates

- **Weight** on the clearance and both volumes, as fitted. With [the scaling switch](help:models/fat-free-mass) on, the weight terms read the pharmacokinetic weight, and the intercompartmental clearance is scaled to fat-free mass, anchored at the source's own three groups.
