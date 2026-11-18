### Two populations

The Patient Profile's **Pulmonary hypertension** field switches between two populations, whose pharmacokinetics are very different. It is shown while tadalafil is in the dose table.

| | Pulmonary hypertension: Yes | No (the default) |
|---|---|---|
| Source | Ferguson-Sells 2022, a population model in patients with PAH | The FDA's population model in men with erectile dysfunction (study LVBF) |
| Apparent clearance | 1.88 L/h | 2.24 L/h |
| Apparent volume, 70 kg | 88.1 L | 60 L |
| Absorption rate | 0.86 /h | 1.75 /h |
| Half-life | 32.5 h (Adcirca label: 35 h) | 18.6 h (Cialis label: 17.5 h) |

Patients with pulmonary arterial hypertension, often with severe heart disease, clear tadalafil more slowly, and its half-life is about 1.7 times the other.

### The model with pulmonary hypertension

Tadalafil's parameters come from Ferguson-Sells and colleagues (*Clin Pharmacokinet* 2022;61:249-262, doi 10.1007/s40262-021-01052-8). They fitted one model to 305 adults with pulmonary arterial hypertension from PHIRST-1 (2.5 to 40 mg once daily) and 19 children aged 2.5 to 17 years. It has one compartment with first-order absorption, all values apparent (absolute bioavailability has never been measured):

- absorption rate constant 0.860 /h;
- apparent clearance 1.88 L/h for a patient not taking bosentan (3.23 L/h with it);
- apparent volume 88.1 L × weight/70.

Relative bioavailability falls with the dose and rises slightly with age, as (dose/16.27 mg)<sup>−0.227</sup> × (age/52.4)<sup>0.100</sup>. That is 0.82 at 40 mg and 0.95 at 20 mg for a patient of 52, so a 40 mg dose gives about 1.7 times the exposure of 20 mg. The Adcirca label reports "approximately 1.5-fold".

The plotted patient is assumed not to take bosentan, which induces tadalafil's metabolism.

### Checks

These come from the label, which draws on the same PHIRST-1 data, so they are not independent. In pulmonary hypertension without bosentan, the Adcirca label gives an apparent clearance of 1.6 L/h and a terminal half-life of 35 hours. This model's half-life is 32.5 hours.

### The model without pulmonary hypertension

This is the population model of study LVBF, 179 men with erectile dysfunction given 2 to 25 mg, as printed in the FDA's Biopharmaceutics Review of Cialis (NDA 21-368, 2002-2003):

- one compartment with first-order absorption and elimination, and no lag;
- apparent clearance 2.24 L/h, in men without "current active cardiovascular conditions";
- apparent volume 60.0 L;
- an absorption rate of 1.75 /h, fixed at a "value estimated from healthy volunteers".

Men with active cardiovascular disease cleared tadalafil 29% more slowly (1.59 L/h). The Patient Profile has no field for that, so it is not applied. The reviewers' own refit added creatinine clearance to the clearance, which is not applied either. Of the review's four population analyses, this is the one printed in full.

Compared with the Cialis label's healthy subjects (17.5 hours, a median peak at 2 hours, about 1.6-fold accumulation on a daily dose), this model gives a half-life of 18.6 hours, a peak at 2.2 hours and 1.69-fold accumulation. After 20 mg it peaks at 306 ng/mL, where Forgue and colleagues' integrated analysis of 237 healthy subjects (*Br J Clin Pharmacol* 2006;61:280-288, doi 10.1111/j.1365-2125.2005.02553.x, abstract read) reports a mean peak of 378 ng/mL. The label's 25% higher exposure in men of 65 or more is not applied.

### Covariates

- **Weight** acts on the volume. With pulmonary hypertension it is the source's own weight term, at the pharmacokinetic weight with [the scaling switch](help:models/fat-free-mass) on and total weight with it off, and clearance has no weight term. Without it, the library's standard size scaling applies to both.
- **Age** acts on bioavailability with pulmonary hypertension. It is held at 2.5 years at younger ages, the youngest age the source studied.
- **Renal impairment** is not modelled. The label reports that exposure doubled in mild or moderate renal impairment.

### Dose and band

Adcirca tablets contain 20 mg of tadalafil (no salt). The dose in pulmonary hypertension is 40 mg once daily. No therapeutic range has been established.
