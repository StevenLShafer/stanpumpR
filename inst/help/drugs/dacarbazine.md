### The model

Breithaupt and colleagues (*Cancer Chemother Pharmacol* 1982;9:103-109) gave dacarbazine (DTIC) by intravenous injection on 5 consecutive days to adults with melanoma or sarcoma. For six patients aged 42 to 57 they printed each patient's two half-lives, clearance and steady-state volume. Those four numbers fix a two-compartment model, which stanpumpR solves for each patient and averages: central volume 0.178 L/kg, peripheral volume 0.454 L/kg, clearance 15.4 mL/min/kg and intercompartmental clearance 21.9 mL/min/kg, per kilogram of body weight.

Two numbers the derivation did not use check it: each patient's central volume lies near the paper's own initial volume, and the fast phase carries 30% of the area, against the paper's "about 28%".

### What is plotted

Plasma dacarbazine, in mcg/mL, with a terminal half-life of about 41 minutes. Dacarbazine is a prodrug: the liver turns it into MTIC, the methylating species, which was not measured and is not modelled, nor is AIC, the inactive end product. There is no therapeutic range, so none is drawn.

### Where to be careful

- **Six patients, 1982.** The model is a mean of six people with normal kidneys.
- **Kidney function.** About half of the clearance is renal, but with six patients no renal term could be estimated; the curve is a normal kidney's.
- **Pregnancy:** one woman studied in pregnancy and again after delivery had a 27% larger area in pregnancy (Kantrowitz-Gordon 2018). One woman is too few to apply, so nothing is changed.
