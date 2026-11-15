### The model

Riociguat's parameters come from Saleh and colleagues (*Pulm Circ* 2016;6(Suppl 1):S86-S96, doi 10.1086/685404). They fitted one compartment to sparse concentrations from 698 patients with pulmonary arterial hypertension (PATENT-1/2) or chronic thromboembolic pulmonary hypertension (CHEST-1/2), who took 0.5 to 2.5 mg three times a day:

- absorption rate constant 2.17 /h;
- clearance 1.81 L/h;
- volume 32.3 L.

The values are apparent, but the absolute bioavailability is about 94%, so they are close to the true values.

### Covariates

The source prints each covariate as a percentage change per unit above or below the median.

- **Volume** changes by 0.619% per kg of body weight from 68 kg.
- **Clearance** changes by 0.394% per mL/min of creatinine clearance from 81.2 mL/min.

The paper does not say which equation gave the creatinine clearance, nor whether the terms add or multiply. Here they multiply, and the creatinine clearance is Cockcroft-Gault's, computed from the Patient Profile's **Serum creatinine**. A blank field means an assumed normal creatinine for age and sex. The creatinine clearance is held within the range studied, 15.4 to 187 mL/min.

With [the scaling switch](help:models/fat-free-mass) on, both terms use the pharmacokinetic weight; with it off, total weight.

The patient is assumed to be a non-smoker, not taking bosentan, with a median bilirubin of 0.58 mg/dL. In the source, smoking raised clearance by 120%, bosentan by 35.6%, and each mg/dL of bilirubin by 13.6%.

### Checks

The Adempas label gives a clearance of "about 1.8 L/h in patients with PAH", a terminal half-life of "about 12 hours in patients", and a steady-state volume of about 30 L. This model gives 1.81 L/h, 12.4 hours and 32.3 L for the median patient.

Healthy subjects clear riociguat about twice as fast (label: 3.4 L/h and a half-life of 7 hours), so this model over-predicts a healthy subject's exposure about twofold.

### Metabolite

M1 has a third to a tenth of the parent's potency and reaches about half its concentration in pulmonary hypertension. It is not plotted. Saleh fitted it as a separate one-compartment model from the dose (absorption 0.26 /h, clearance 3.16 L/h, volume 124 L), not formed from the parent.

### Dose and band

Adempas tablets contain 0.5 to 2.5 mg of riociguat (no salt), taken three times a day. Riociguat is contraindicated in pregnancy. No therapeutic range has been established; Saleh found no relation of the 6-minute walk distance to exposure.
