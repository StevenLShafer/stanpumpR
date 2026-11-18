### The model

Ambrisentan's parameters come from Okour and colleagues (*J Clin Pharmacol* 2023;63:593-603, doi 10.1002/jcph.2199). They fitted 39 children aged 8 to 16 with pulmonary arterial hypertension, who took 2.5 to 10 mg once daily. The model has two compartments with first-order absorption after a lag. Weight is its only covariate, scaled allometrically about 70 kg:

- apparent clearance 1.17 L/h × (weight/70)<sup>0.75</sup>;
- central volume 12.3 L × weight/70;
- intercompartmental clearance 0.457 L/h × (weight/70)<sup>0.75</sup>;
- peripheral volume 81.3 L × weight/70;
- absorption rate constant 2.46 /h, after a lag of 0.525 h.

All values are apparent, because the absolute bioavailability is unknown.

### One model for every age

This model was fitted in children. The adult population model it was compared with is in the paper's supplement, which has not been read. The paper found the adult model's clearance, central volume and absorption similar, and steady-state exposures similar once weight is allowed for. At 10 mg daily the AUC was 9.15 µg·h/mL in children against 9.12 in adults. Its peripheral volume differed from the adult model's, and the slow phase it gives here (half-life about 174 hours) is probably the children's estimate rather than the drug's. Until the adult model can be read, this model is used at every age.

### Checks

| | Letairis label (adults with PAH) or Okour's adult model | This model, 70 kg, 10 mg daily |
|---|---|---|
| Apparent clearance | 19 mL/min (1.14 L/h) | 1.17 L/h |
| Time to peak | about 2 h | 1.7 h |
| Trough as a share of peak, steady state | about 15% | 17% |
| Accumulation | about 1.2 | 1.43 |
| AUC over 24 h, steady state | 9.12 µg·h/mL (Okour's adult model) | 8.5 µg·h/mL |

### Covariates

Weight acts on every parameter. With [the scaling switch](help:models/fat-free-mass) on it is the pharmacokinetic weight; with it off, total weight. Cyclosporine doubles exposure (label), which is not modelled.

### Dose and band

Letairis tablets contain 5 or 10 mg of ambrisentan (no salt), taken once daily. Ambrisentan is contraindicated in pregnancy. No therapeutic range has been established; Okour found no relation of exposure to the 6-minute walk distance or to adverse events.
