### The model

Topotecan comes from Mould and colleagues (*Clin Pharmacol Ther* 2002;71:334-348): 245 adults with solid tumours from nine trials, given 30-minute infusions daily for five days. Two compartments describe total topotecan (the active lactone and the open-ring carboxylate together):

- clearance (5.77 + 12.8 × renal function) × (weight / 70)^0.75 L/h, renal function being the Cockcroft-Gault creatinine clearance, capped at 150 mL/min, standardised to 70 kg and divided by 70 mL/min;
- central volume 19.3 L and peripheral volume 45.7 L, in proportion to weight, exchanging at 90.4 L/h × (weight / 70)^0.75.

For a 70-kg patient with a creatinine clearance of 70 mL/min, clearance is 18.6 L/h, two-thirds of it renal, and the half-lives are 6 minutes and 2.7 hours.

### What is plotted

Total plasma topotecan in ng/mL. Doses are entered in mg as labelled (topotecan base); a dose prescribed per m² must be multiplied by the body surface area first. There is no effect site and no band.

### Covariates

- **Weight**, on every parameter. With [the scaling switch](help:models/fat-free-mass) on it is the pharmacokinetic weight.
- **Serum creatinine**, from the Patient Profile, in Cockcroft-Gault. Because the paper standardises the creatinine clearance to 70 kg, weight does not enter the renal term. Left blank, the library's assumed normal value is used.
- **Performance status is held at ECOG 1**, the study's middle; ECOG 0 raises clearance by 13% and ECOG 2 lowers it by 11%.

### Where to be careful

- **Renal impairment** matters most: the dose is reduced for it, and the curve shows why.
- **The paper's worked examples** (its Table VI) come out as if the raw creatinine clearance, not the standardised one, had been used; the printed equation is the one implemented.
- **Oral topotecan** was not in the analysis. Not a monitoring tool.

Every parameter was read in the paper's PDF.
