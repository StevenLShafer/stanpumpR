### The model

Tadalafil's parameters come from Ferguson-Sells and colleagues (*Clin Pharmacokinet* 2022;61:249-262, doi 10.1007/s40262-021-01052-8). They fitted one model to 305 adults with pulmonary arterial hypertension from PHIRST-1 (2.5 to 40 mg once daily) and 19 children aged 2.5 to 17 years. It has one compartment with first-order absorption, all values apparent (absolute bioavailability has never been measured):

- absorption rate constant 0.860 /h;
- apparent clearance 1.88 L/h for a patient not taking bosentan (3.23 L/h with it);
- apparent volume 88.1 L × weight/70.

Relative bioavailability falls with the dose and rises slightly with age, as (dose/16.27 mg)<sup>−0.227</sup> × (age/52.4)<sup>0.100</sup>. That is 0.82 at 40 mg and 0.95 at 20 mg for a patient of 52, so a 40 mg dose gives about 1.7 times the exposure of 20 mg. The Adcirca label reports "approximately 1.5-fold".

The plotted patient is assumed not to take bosentan, which induces tadalafil's metabolism.

### Checks

These come from the label, which draws on the same PHIRST-1 data, so they are not independent. In pulmonary hypertension without bosentan, the Adcirca label gives an apparent clearance of 1.6 L/h and a terminal half-life of 35 hours. This model's half-life is 32.5 hours.

### Healthy subjects and erectile dysfunction

Healthy subjects clear tadalafil faster. The Cialis label gives an apparent clearance of 2.5 L/h and a half-life of 17.5 hours. The Adcirca label gives 3.4 L/h and 15 hours after 40 mg. This model therefore over-predicts a healthy man's exposure, by about a third.

### Covariates

- **Weight** acts on the volume. With [the scaling switch](help:models/fat-free-mass) on it is the pharmacokinetic weight; with it off, total weight. Clearance has no weight term.
- **Age** acts on bioavailability. It is held at 2.5 years at younger ages, the youngest age the source studied.
- **Renal impairment** is not modelled. The label reports that exposure doubled in mild or moderate renal impairment.

### Dose and band

Adcirca tablets contain 20 mg of tadalafil (no salt). The dose in pulmonary hypertension is 40 mg once daily. No therapeutic range has been established.
