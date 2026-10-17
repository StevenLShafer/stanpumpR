### The model

Baron and colleagues' analysis of 2761 adults with type 2 diabetes (*Diabetes Ther* 2016): two compartments with first-order absorption after a lag fixed at 0.5 h; apparent clearance 10.6 L/h, central volume 3.14 L, intercompartmental clearance 6.34 L/h, peripheral volume 70.6 L, absorption 0.196/h (the paper prints its unit as L/h, a typographical error). The central volume is poorly identified and data from the first hour were excluded, so the early curve is uncertain; the terminal half-life is about 12 hours.

### Renal function and other covariates

Each covariate enters as a power of its value over the reference patient's (a 50-year-old man, BMI 25, eGFR 100 mL/min/1.73 m², by MDRD). Renal function is applied: clearance is multiplied by (eGFR / 100)<sup>0.333</sup>, with eGFR the four-variable MDRD from the entered serum creatinine (a child's on the adult scale) or an [assumed normal](help:models/covariates) one. The exponent is **derived**, not read: its coefficient is in the supplement's Table S1, which was not available, but the paper reports exposure 18.5%, 49.2% and 88.1% higher at an eGFR of 60, 30 and 15, and the one exponent 0.333 reproduces all three within 0.2%. The other covariates (BMI, total protein, age, sex, smoking, race) change exposure by less than 25% and are not applied.

### What is not plotted

Concentration only, in ng/mL. The fasting glucose and HbA1c models and urinary glucose excretion are endpoints of their own and are not drawn. See [Antidiabetic models and their limits](help:models/antidiabetics).
