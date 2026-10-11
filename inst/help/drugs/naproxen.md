### The model

Naproxen kinetics are from Välitalo and colleagues (*J Clin Pharmacol* 2012;52:1516-1526). They gave 53 healthy children, aged 3 months to 12 years, a single 10 mg/kg dose of naproxen suspension before surgery under spinal anaesthesia. Plasma was sampled for up to 51 hours, and the model has two compartments with first-order absorption (1.1/h, no lag). There is no intravenous naproxen, so every parameter is apparent, divided by the unmeasured bioavailability. Naproxen is absorbed completely by mouth (Davies and Anderson 1997), and is offered by mouth only. Doses are milligrams of naproxen: a 550 mg tablet of naproxen sodium is 500 mg of naproxen, and a 220 mg over-the-counter tablet is 200 mg.

### Re-centred for adults

The source scaled clearance linearly with weight. For a 70 kg adult that gives 0.62 L/h, more than adults are measured to clear: 0.42 L/h in young men after 375 mg (Upton and colleagues, *Br J Clin Pharmacol* 1984;18:207-214). The model is therefore re-centred on the source's median child, 20 kg, who receives the published values exactly, and scaled from there allometrically. Clearances scale by weight to the 0.75 power and volumes in proportion to weight. The authors' own allometric alternative fitted the data as well.

For the 70 kg reference man this gives the following:

| Parameter | Value |
|---|---|
| Clearance | 0.45 L/h |
| Central volume | 8.2 L |
| Intercompartmental clearance | 0.36 L/h |
| Peripheral volume | 4.3 L |
| Half-lives | 4.6 h and 23 h |

With the [fat-free-mass switch](help:models/fat-free-mass) on, the 70 kg values are scaled to fat-free mass; with it off, allometrically on total weight about the same 20 kg child. The source's linear scaling of clearance is not reproduced in either position. The source found no weight effect on the intercompartmental clearance, so scaling it is theory. Left unscaled, it would give an adult a terminal half-life of 32 hours.

Against adult data, a single 500 mg dose gives an area under the curve of 1103 mg·h/L. Sixty-six healthy adults given 500 mg of enteric-coated naproxen had 1206 mg·h/L to 72 hours (Choi and colleagues, *Drug Des Devel Ther* 2015;9:4127-4135). The model's peak, 48 mcg/mL at 2.5 hours, is about 20% below the 62 mcg/mL those adults reached. The terminal half-life, 23 hours, is longer than the label's 12 to 17 hours and close to the 24.7 hours Vree and colleagues measured in 10 adults (*Br J Clin Pharmacol* 1993;35:467-472).

### Protein binding

Naproxen is more than 99% bound to albumin, and the binding saturates. Above about 500 mg a day the unbound fraction rises, so total clearance rises and total concentrations increase less than in proportion to the dose. The unbound clearance does not change (Davies and Anderson, *Clin Pharmacokinet* 1997;32:268-293). The model is linear in total drug, so it overstates total concentrations at the usual doses. At steady state, 375 mg twice a day averages 69 mcg/mL in the model against 58 in young men, and 500 mg twice a day 92 against 75 (van den Ouweland and colleagues, *Br J Clin Pharmacol* 1987;23:189-193). The elderly and patients with low albumin have a higher unbound fraction still.

### Effect and threshold

The model has no effect site: the plotted line is the plasma. Björnsson and Simonsson (*Br J Clin Pharmacol* 2011;71:899-906) modelled pain intensity in 242 adults after wisdom-tooth removal, given naproxen 500 mg, naproxcinod or placebo. They found the effect followed the **unbound** plasma concentration with no delay: a sigmoid Emax model with a maximum effect of 1, an EC50 of 0.135 µmol/L unbound and a shape factor of 1.61. Through their own binding model the EC50 corresponds to 29 mcg/mL of total naproxen. That is the recovery threshold, timed on the plasma, so *time until threshold* answers how long the concentration stays above the level that halves the pain naproxen can relieve. No shaded band is drawn. Their own kinetic model is not used here, because it was fitted to only 8 hours of data and gives naproxen a half-life of 6 to 8 hours. Naproxen is not an opioid and is not on the MEAC panel.

### Where to be careful

The kinetics were fitted in children and are re-centred for adults, so they are an extrapolation. In adults the peak runs about 20% low, and at the usual doses total concentrations run high (above). The model does not represent the saturable binding, age or low albumin, renal or hepatic impairment, CYP2C9 or CYP1A2 genotype, food, or formulations. Enteric-coated and delayed-release tablets peak hours later than the suspension the source used. Clearance is still maturing in infants under 3 months, who were not studied.
