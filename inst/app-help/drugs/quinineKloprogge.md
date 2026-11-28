### The model

Kloprogge and colleagues (*J Antimicrob Chemother* 2014;69:3033-3040) studied 22 pregnant women in the second or third trimester (13 to 37 weeks) with uncomplicated falciparum malaria in Mbarara, Uganda: 18 to 37 years (median 21), 44 to 71 kg (median 56.5). They took quinine sulfate 10 mg/kg three times daily for 7 days, with plasma sampled to 184 hours.

The model has two compartments with first-order absorption and no lag. Its parameters are apparent oral ones in quinine base, with the bioavailability fixed at 100%:

- absorption rate 0.817 /h;
- clearance 10.4 L/h and intercompartmental clearance 10.7 L/h, each × (weight/56.5)^(2/3);
- central volume 174 L and peripheral volume 54.3 L, each × weight/56.5.

The paper does not print the weight its allometric terms are divided by. stanpumpR uses the cohort's median, 56.5 kg; the paper's simulations use a 56 kg typical patient, a difference of 0.6% in clearance.

### The malaria covariates

Two disease terms were kept, and stanpumpR holds both at the paper's reference:

- **Parasitaemia on bioavailability**: 38.9% more per log10 parasites/µL, as a time-varying covariate. The paper does not print the parasitaemia at which bioavailability is 100%, so it is held at that 100%.
- **Admission temperature on clearance**: clearance falls exponentially with fever, 51.8% lower at 39 °C than at 36 °C. It is held at the cohort's median, 37.2 °C.

The paper's own post hoc estimates fall on either side of the curve this gives. The first dose's AUC over 8 hours is 13.2 mg·h/L against the paper's median of 26.6, and the peak after the last dose on day 7 is 6.2 mg/L against its 4.06. That is what a bioavailability that rises with parasitaemia gives, high while the patient is acutely ill and lower once the parasites are gone. The curve sits between the two, and no single curve is the paper's typical patient. The terminal half-life, 16.2 hours, agrees with the paper's median of 15.3.

### Who it is used for

Every adult. The [quinine](help:drugs/quinine) page explains why a model fitted in pregnant women is used for everyone.

### Body size

The paper's own weight terms are applied. With [the scaling switch](help:models/fat-free-mass) on, the weight in them is the pharmacokinetic weight, as for every model with its own weight covariate.
