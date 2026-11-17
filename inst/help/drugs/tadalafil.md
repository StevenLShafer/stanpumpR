### Two populations

The Patient Profile's **Pulmonary hypertension** field switches between two populations, whose pharmacokinetics are very different. It is shown while tadalafil is in the dose table.

| | Pulmonary hypertension: Yes | No (the default) |
|---|---|---|
| Source | Ferguson-Sells 2022, a population model in patients with PAH | Derived from the Cialis label: no population model was found |
| Apparent clearance | 1.88 L/h | 2.5 L/h |
| Apparent volume, 70 kg | 88.1 L | 63 L |
| Absorption rate | 0.86 /h | 2.0 /h |
| Half-life | 32.5 h (Adcirca label: 35 h) | 17.5 h (Cialis label: 17.5 h) |

Patients with pulmonary arterial hypertension, often with severe heart disease, clear tadalafil more slowly, and its half-life is about twice the healthy one.

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

No population model of tadalafil in healthy subjects or in men with erectile dysfunction was found. The FDA's review of Adcirca says such a model, in men with erectile dysfunction, informed the structure of the PAH model, but it is in the Cialis submission, which has not been obtained. So this model is derived from the Cialis label (section 12.3, healthy subjects):

- one compartment ("concentrations declined nearly monoexponentially");
- apparent clearance 2.5 L/h and apparent volume 63 L, the label's means;
- an absorption rate of 2.0 /h, solved so the peak falls at the label's median of 2 hours, with no lag;
- no dose term: the label says exposure is proportional to dose from 2.5 to 20 mg.

Its half-life is 17.5 hours and its accumulation on a daily dose 1.63, both following from those values, as the label says (17.5 hours, "approximately 1.6-fold"). After 20 mg it peaks at 293 ng/mL, where Forgue and colleagues' integrated analysis of 237 healthy subjects (*Br J Clin Pharmacol* 2006;61:280-288, doi 10.1111/j.1365-2125.2005.02553.x, abstract read) reports a mean peak of 378 ng/mL. A one-compartment model built from a terminal volume puts the peak low; a second compartment would raise it, but no source gives one. The label's 25% higher exposure in men of 65 or more is not applied.

### Covariates

- **Weight** acts on the volume. With pulmonary hypertension it is the source's own weight term, at the pharmacokinetic weight with [the scaling switch](help:models/fat-free-mass) on and total weight with it off, and clearance has no weight term. Without it, the library's standard size scaling applies to both.
- **Age** acts on bioavailability with pulmonary hypertension. It is held at 2.5 years at younger ages, the youngest age the source studied.
- **Renal impairment** is not modelled. The label reports that exposure doubled in mild or moderate renal impairment.

### Dose and band

Adcirca tablets contain 20 mg of tadalafil (no salt). The dose in pulmonary hypertension is 40 mg once daily. No therapeutic range has been established.
