### The model

Acetaminophen (paracetamol) kinetics are from Morse and colleagues (*Eur J Drug Metab Pharmacokinet* 2022;47:497-507). They pooled intravenous, tablet, sachet and suspension data from 116 healthy adults aged 18 to 49 and weighing 49 to 116 kg, and fitted a two-compartment model. For their standard man (70 kg, 176 cm), clearance is 24.0 L/h, central volume 43.7 L, intercompartmental clearance 43.5 L/h and peripheral volume 29.7 L. The terminal half-time at the library's reference man is about 2.3 hours.

Every value is from the paper's final-model table (Table 2). The abstract's "central volume of distribution ... 43.5" is an error in the abstract: in the table, 43.5 L/h is the intercompartmental clearance and the central volume is 43.7 L.

### Oral route

**mg PO** is a fasted immediate-release tablet, from the same study: bioavailability 0.859, absorption half-time 11.5 minutes and a 5.3 minute lag. All three are used as published, and a 1 g tablet peaks at 11.0 mcg/mL at about 34 minutes in the reference man. Like gabapentin and pregabalin, acetaminophen keeps its published absorption lag; for those 5.3 minutes the dose is not yet counted in *time until threshold*. Food multiplies the tablet's absorption half-time by 1.87 and its lag by 4.6. That is not modelled, so the oral curve is the fasted curve.

### Covariates

Clearance carries its own covariate, **normal fat mass**: fat-free mass plus 0.816 of the fat mass, scaled allometrically to the 0.75 power. It does so in either position of the [fat-free-mass switch](help:models/fat-free-mass). Morse computed fat-free mass with Janmahasatian's equations; stanpumpR uses its own, Al-Sallami's, which are the same in adult men. They give a clearance less than 0.3% higher in adult women, and in children about 1.5% lower in boys and under 1% higher in girls. The volumes and the intercompartmental clearance were published on total body weight. With the switch on they see the pharmacokinetic weight, and with it off they see total weight, as published.

### Effect site

ke0 is supplied directly from Anderson and colleagues (*Eur J Clin Pharmacol* 2001;57:559-569). They fitted an analgesic equilibration half-time of 53 minutes in children after tonsillectomy, with a maximum effect of 5.17 pain units (VAS 0-10) and an EC50 of 9.98 mg/L. With this model the effect site peaks about 90 minutes after an intravenous dose and about two hours after a tablet: analgesia lags the plasma peak substantially. Applying a paediatric, oral-derived half-time to adults and to intravenous doses is an assumption.

### Typical concentrations

The shaded band (3 to 15 mcg/mL) covers the effect-site concentrations adult dosing reaches: 1 g intravenously peaks near 7 mcg/mL, and 1 g intravenously every 6 hours averages about 7 (about 6 by mouth). The typical line, 7 mcg/mL, is that steady-state average. For comparison, Anderson's paediatric target effect-site concentration is 10 mg/L, which is expected to reduce pain by about 2.6 units on a 10-point scale. The recovery threshold is 5 mcg/mL in the effect site, so *time until threshold* counts down after an ordinary adult dose; at 10 mcg/mL a 1 g dose never reaches it. Acetaminophen is not an opioid and is not on the MEAC panel.

### Where to be careful

The source studied healthy adults only, and the model has no maturation term. It therefore overpredicts clearance, and underpredicts concentrations, in neonates and in infants under about a year, whose glucuronidation is still maturing. In older children it is allometric extrapolation. The model says nothing about hepatotoxicity, the toxic metabolite NAPQI, hepatic impairment, fed-state absorption or rectal administration.
