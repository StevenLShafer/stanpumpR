### The model

Ondansetron's parameters are from Chiang and colleagues (*Br J Clin Pharmacol* 2021;87:516-526), who gave intravenous ondansetron to 14 adults aged 45 to 70 and sampled plasma and cerebrospinal fluid for 3 hours. Their two-compartment model has, for the reference patient, clearance 24.6 L/h, central volume 63.3 L, intercompartmental clearance 211 L/h and peripheral volume 107 L. Half-times about 0.1 and 5.2 hours.

### What is left out

Chiang found the central volume falling with age (a power exponent of -4.91), but the exact form of that term, and the correlation between clearance and central volume, could not be verified from the published table, so **the age effect is not applied**: every patient gets the reference adult's model, scaled for size. Between-patient variability (clearance 50 per cent, central volume 42 per cent) is not shown. Beyond 3 hours the curve extrapolates the sampled window.

Oral ondansetron is not offered; no absorption model was assembled.

### Covariates

None applied. The parameters take the default [fat-free-mass scaling](help:models/fat-free-mass) and are used as published with the switch off.

### Children

The source has no children. **A child here is the adult model scaled to fat-free mass, an extrapolation.** Mondick and colleagues fitted intravenous ondansetron at 1 to 48 months (*Eur J Clin Pharmacol* 2010;66:77-86), but their full parameter table was not available to verify, so it is not used.

### Effect site

None. There is no published equilibration delay or concentration-response model for preventing nausea and vomiting after surgery, and only the plasma concentration is plotted. The shaded band (5 to 40 ng/mL) covers what 4 to 8 mg produce over the first hours. For orientation only: in an adult ipecac challenge, Cox and colleagues (*J Pharmacokinet Biopharm* 1999;27:625-644) estimated that the hazard of vomiting halves with each 1.4 ng/mL of ondansetron. That is neither a 5-HT3 receptor occupancy nor a probability of preventing PONV or chemotherapy-induced emesis.

### Where to be careful

Fourteen middle-aged and older adults, three hours of sampling, and no age term. Hepatic impairment, CYP induction and QT effects are not represented.
