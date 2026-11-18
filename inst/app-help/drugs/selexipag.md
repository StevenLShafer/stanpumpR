### The model

Selexipag's parameters come from Krause and colleagues (*CPT Pharmacometrics Syst Pharmacol* 2017;6:477-485, doi 10.1002/psp4.12202). They fitted one model to 512 patients with pulmonary arterial hypertension in the GRIPHON trial, who took 200 to 1,600 mcg twice a day. Both selexipag and its active metabolite, [ACT-333679](help:drugs/ACT333679), have two compartments. Selexipag is absorbed after a lag and converted to the metabolite at a first-order rate. All values are apparent (oral):

- lag 0.67 h, absorption rate constant 0.71 /h;
- central volume 12.9 L × (weight/70)<sup>1.2</sup>;
- clearance other than conversion 19.1 L/h × (weight/70)<sup>0.61</sup>, at a bilirubin of 10 µmol/L;
- transfer rate constants 0.09 /h out to the peripheral compartment and 0.06 /h back;
- conversion to ACT-333679 at 0.67 /h.

Selexipag's total clearance is therefore 27.7 L/h for a 70 kg patient.

The metabolite carries the effect: it is 37 times as potent at the prostacyclin receptor. At steady state its exposure is 3.8 times selexipag's (the label says "approximately 3- to 4-fold").

### Checks

Kaufmann and colleagues (*Am J Cardiovasc Drugs* 2015, doi 10.1007/s40256-015-0117-4) gave a single 400 mcg dose to healthy, fasting men. This model was not fitted to them.

| | Kaufmann 2015 (means) | This model, 70 kg man |
|---|---|---|
| Selexipag peak | 5.98 ng/mL | 5.75 ng/mL |
| Selexipag AUC | 12.35 ng·h/mL | 14.4 ng·h/mL |
| ACT-333679 peak | 8.18 ng/mL | 5.55 ng/mL |
| ACT-333679 AUC | 53.65 ng·h/mL | 46.4 ng·h/mL |

### Covariates

Weight acts on the volumes and on clearance. With [the scaling switch](help:models/fat-free-mass) on it is the pharmacokinetic weight; with it off, total weight.

Sex acts on the metabolite's elimination. Bilirubin is held at 10 µmol/L; each doubling lowered clearance by about a quarter.

The patient is assumed to take no other drug for pulmonary hypertension. An endothelin receptor antagonist, a PDE5 inhibitor, or both sped the metabolite's elimination, lowering its exposure by up to 30%.

### What is not offered

Intravenous selexipag (Uptravi for injection) is not offered. After intravenous doses the label reports similar exposure to the metabolite but about twice the exposure to selexipag. Much of the oral metabolite must therefore be formed before selexipag reaches the circulation, which this oral model does not represent.

### Dose and band

Uptravi tablets contain 200 to 1,600 mcg of selexipag (no salt), taken twice a day. No therapeutic range has been established.
