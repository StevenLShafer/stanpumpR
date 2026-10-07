### The model

Acetaminophen (paracetamol) kinetics are from Morse and colleagues (*Eur J Drug Metab Pharmacokinet* 2022;47:497-507). They pooled intravenous, tablet, sachet and suspension data from 116 healthy adults aged 18 to 49 and weighing 49 to 116 kg, and fitted a two-compartment model. For their standard man (70 kg, 176 cm), clearance is 24.0 L/h, central volume 43.7 L, intercompartmental clearance 43.5 L/h and peripheral volume 29.7 L. The terminal half-time at the library's reference man is about 2.3 hours.

The clearance, the fat factor and the covariate structure were checked against the paper's text. The central volume, intercompartmental clearance and peripheral volume were not, because the paper's parameter table could not be retrieved, and the abstract gives a central volume of 43.5 (with a unit typo) where this model uses 43.7. These three values are flagged in the code as needing a check against the paper.

### Oral route

**mg PO** is a fasted immediate-release tablet, from the same study: bioavailability 0.86, absorption half-time 11.5 minutes and a 5.3 minute lag. The library keeps its drugs lag-free, so the lag is folded into a single absorption constant with the same mean input time (21.9 minutes, ka 0.046/min). A 1 g tablet then peaks near 10 mcg/mL at about 35 minutes in the reference man. Food roughly doubles the absorption half-time and lengthens the lag up to 4.6-fold. That is not modelled, so the oral curve is the fasted curve.

### Covariates

Clearance carries its own covariate, **normal fat mass**: fat-free mass (Janmahasatian) plus 0.816 of the fat mass, scaled allometrically to the 0.75 power. It does so in either position of the [fat-free-mass switch](help:models/fat-free-mass). The volumes and the intercompartmental clearance were published on total body weight. With the switch on they see the pharmacokinetic weight, and with it off they see total weight, as published.

### Effect site

ke0 is supplied directly from Anderson and colleagues (*Eur J Clin Pharmacol* 2001;57:559-569). They fitted an analgesic equilibration half-time of 53 minutes in children after tonsillectomy, with a maximum effect of 5.17 pain units (VAS 0-10) and an EC50 of 9.98 mg/L. With this model the effect site peaks about 90 minutes after an intravenous dose and about two hours after a tablet: analgesia lags the plasma peak substantially. Applying a paediatric, oral-derived half-time to adults and to intravenous doses is an assumption.

### Typical concentrations

The shaded band (5 to 20 mcg/mL) is centred on Anderson's 10 mg/L target effect-site concentration, which is expected to reduce pain by about 2.6 units on a 10-point scale. The recovery threshold is 10 mcg/mL in the effect site. Acetaminophen is not an opioid and is not on the MEAC panel.

### Where to be careful

The source studied healthy adults only, and the model has no maturation term. It therefore overpredicts clearance, and underpredicts concentrations, in neonates and in infants under about a year, whose glucuronidation is still maturing. In older children it is allometric extrapolation. The model says nothing about hepatotoxicity, the toxic metabolite NAPQI, hepatic impairment, fed-state absorption or rectal administration.
