### The model

Ondansetron's parameters are from Chiang and colleagues (*Br J Clin Pharmacol* 2021;87:516-526), who gave 16 mg of intravenous ondansetron over 15 minutes to 15 adults aged 45 to 70 and sampled plasma for 3 hours (and cerebrospinal fluid once); one patient's samples could not be quantified, so the model was fitted to 14. Their two-compartment model has, at the median age of 58, clearance 24.6 L/h, central volume 63.3 L, intercompartmental clearance 211 L/h and peripheral volume 107 L. Half-times about 0.1 and 5.2 hours.

### Age

Chiang's one covariate is age on the central volume, in a power form about the median age: central volume = 63.3 × (age/58)^-4.91 L. The exponent is steep. Within the study it takes the central volume from 220 L at 45 to 25 L at 70, a ninefold range, so the early peak after a dose changes a great deal with age while the later curve, set by clearance and the peripheral volume, does not. Outside the ages studied the equation gives absurd volumes (1,600 L at 30), so **the age term is used only between 45 and 70**: a younger patient gets the 45-year-old's central volume and an older one the 70-year-old's. That holds the model inside its data, but it is not evidence of what the central volume is at 30 or at 85.

### What is left out

Between-patient variability (clearance 50 per cent, central volume 42 per cent; their correlation was estimated but not reported) is not shown. Beyond 3 hours the curve extrapolates the sampled window.

Oral ondansetron is not offered; no absorption model was assembled.

### Covariates

Age, on the central volume only (above). The parameters also take the default [fat-free-mass scaling](help:models/fat-free-mass) and are used as published with the switch off; the age term applies either way.

### Children

The source has no children. **A child here is the adult model, with the 45-year-old's age term, scaled to fat-free mass: an extrapolation.** Mondick and colleagues fitted intravenous ondansetron at 1 to 48 months (*Eur J Clin Pharmacol* 2010;66:77-86), but their full parameter table was not available to verify, so it is not used.

### Effect site

None. There is no published equilibration delay or concentration-response model for preventing nausea and vomiting after surgery, and only the plasma concentration is plotted. The shaded band (5 to 40 ng/mL) covers what 4 to 8 mg produce over the first hours. For orientation only: in an adult ipecac challenge, Cox and colleagues (*J Pharmacokinet Biopharm* 1999;27:625-644) estimated that the hazard of vomiting halves with each 1.4 ng/mL of ondansetron. That is neither a 5-HT3 receptor occupancy nor a probability of preventing PONV or chemotherapy-induced emesis.

### Where to be careful

Fourteen middle-aged and older adults, three hours of sampling, and an age term estimated from that small group (relative standard error 19 per cent) that moves the central volume ninefold across it. Hepatic impairment, CYP induction and QT effects are not represented.
