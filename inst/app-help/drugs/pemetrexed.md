### The model

Pemetrexed comes from Latz and colleagues (*Cancer Chemother Pharmacol* 2006;57:401-411): 287 adults from ten single-agent phase II trials, aged 26 to 79, given 500 or 600 mg/m² over 10 minutes every three weeks. Two compartments describe the disposition:

- clearance 43.0 + 47.2 × (creatinine clearance / 92.6) mL/min, the creatinine clearance by Cockcroft-Gault in mL/min;
- central volume 6.13 × (body surface area)^1.32 L;
- one peripheral compartment, 3.38 L, exchanging at 14.5 mL/min.

For the cohort's median patient (creatinine clearance 92.6 mL/min, body surface area 1.74 m²) clearance is 90.2 mL/min, the terminal half-life 3.5 hours and the steady-state volume 16 L, as the paper and the Alimta label report.

### What is plotted

Plasma pemetrexed in mcg/mL (mg/L). Doses are entered in mg or as an infusion rate, as pemetrexed (Alimta is labelled as the acid); a dose prescribed per m² must be multiplied by the body surface area first. There is no effect site and no band.

### Covariates

- **Serum creatinine**, from the Patient Profile, in Cockcroft-Gault. Left blank, the library's assumed normal value is used. Neither the weight in Cockcroft-Gault nor the surface-area formula is named in the paper: total weight and Mosteller's formula are used, and with [the scaling switch](help:models/fat-free-mass) on, the pharmacokinetic weight.
- **Body surface area**, on the central volume.

### Where to be careful

- **Poor renal function.** The clearance never falls below 43 mL/min, however poor the kidneys. The study's lowest creatinine clearance was 36 mL/min, and the authors themselves doubt the straight line below about 40. Pemetrexed is not given below 45 mL/min.
- **Vitamin supplementation.** The patients received no folic acid or vitamin B12, which every patient now does. That changes the toxicity far more than the pharmacokinetics, which the curve describes.
- **Not a monitoring tool:** the curve is a population prediction, not an estimate from measured levels.

Every parameter was read in the paper's PDF. Table 3 gives Q as 14.5 mL/min, the text as 14.4; the table's value is used.
