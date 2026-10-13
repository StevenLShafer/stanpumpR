### The model

Ibuprofen kinetics are from Morse and colleagues (*Eur J Drug Metab Pharmacokinet* 2022;47:497-507), the same study as the library's acetaminophen. They pooled intravenous, tablet, sachet and suspension data from 116 healthy adults aged 18 to 49 and weighing 49 to 116 kg, and fitted a two-compartment model to 6046 ibuprofen concentrations. For their standard man (70 kg, 176 cm), clearance is 3.79 L/h, central volume 6.05 L, intercompartmental clearance 10.5 L/h and peripheral volume 4.37 L. The terminal half-time at the library's reference man is about 2.0 hours.

Every value is from the paper's final-model table (Table 3). The abstract's "central volume of distribution ... 10.5" repeats the error it makes for acetaminophen: in the table, 10.5 L/h is the intercompartmental clearance and the central volume is 6.05 L. The concentrations are total racemic ibuprofen, as the assay measured; the enantiomers are not modelled separately.

### Routes

**mg PO** is a fasted immediate-release tablet: bioavailability 0.941, absorption half-time 26.7 minutes and a 6.7 minute lag, all as published. A 400 mg tablet peaks at 23.6 mcg/mL at about an hour in the reference man. Like acetaminophen, ibuprofen keeps its published absorption lag; for those 6.7 minutes the dose is not yet counted in *time until threshold*. Food multiplies the tablet's absorption half-time by 1.59 and its lag by 3.65. That is not modelled, so the oral curve is the fasted curve. The faster suspension and sachet are not offered either.

**mg** is intravenous ibuprofen, which the study gave as 300 mg over 15 minutes and 400 mg over 30 minutes. It is usually infused over 30 minutes: enter it as **mg/hr** for the length of the infusion (800 mg/hr at 0 and 0 mg/hr at 30 minutes gives 400 mg), since **mg** is a bolus.

Morse's table of simulated peaks gives a 300 mg tablet a median of 24.1 mcg/mL at 0.94 hours. The two-compartment curve here peaks lower, at 17.7 mcg/mL, at about the same time. The table appears to have been calculated with the central volume alone: a one-compartment calculation on 6.05 L gives 25.3 mcg/mL at 0.98 hours.

### Covariates

Clearance and both volumes carry their own covariate, **normal fat mass**: fat-free mass plus a fraction of the fat mass. The fraction is 0.863 for clearance, scaled allometrically to the 0.75 power, and 0.718 for the volumes, scaled linearly. They do so in either position of the [fat-free-mass switch](help:models/fat-free-mass). The intercompartmental clearance is on total body weight to the 0.75 power. With the switch on it sees the pharmacokinetic weight, and with it off total weight, as published. Morse computed fat-free mass with Janmahasatian's equations; stanpumpR uses Al-Sallami's, which are the same in adult men.

### Effect site

ke0 is supplied directly from Hannam and colleagues (*Paediatr Anaesth* 2018;28:841-851). They modelled oral acetaminophen and ibuprofen together in children after tonsillectomy and found an equilibration half-time of 1.04 hours for ibuprofen (95% confidence interval 0.75 to 1.77). Its EC50 was 3.95 mg/L. With this model the effect site peaks about 2 hours 45 minutes after a tablet, at 16.1 mcg/mL for 400 mg, and about 2 hours after 400 mg intravenously over 30 minutes. As for acetaminophen, applying a paediatric, oral-derived half-time to adults and to intravenous doses is an assumption. Two adult estimates of ibuprofen's analgesic potency could not be used for the delay, because their equilibration rates could not be read: Li and colleagues' (*J Clin Pharmacol* 2012;52:89-101, effect-site EC50 10.2 mg/L after third-molar extraction) and Hannam and Anderson's (*Paediatr Anaesth* 2011;21:1234-1240, EC50 5.07 mg/L in adult dental pain).

### Typical concentrations

The shaded band (5 to 35 mcg/mL) covers the effect-site concentrations adult doses reach. Its lower edge is the adult EC50 for dental pain, about where 400 mg every 8 hours sits at its trough at steady state. Its upper edge is the effect-site peak of the largest single dose, 800 mg. The typical line, 17 mcg/mL, is the steady-state average of 400 mg by mouth every 6 hours. The recovery threshold is 6.3 mcg/mL in the effect site. Anderson and Hannam (*Paediatr Anaesth* 2019;29:1107-1113) give this as the effect-site concentration for a target effect of 4 units on a 10-point pain scale. A 400 mg tablet holds the effect site above it from about 48 minutes to 7.3 hours after the dose. Ibuprofen is not an opioid and is not on the MEAC panel.

### Where to be careful

The source studied healthy adults given 150 to 400 mg. Ibuprofen's binding to albumin depends on its concentration: above about 600 mg the unbound fraction rises and the total concentration falls below proportion to the dose (Davies, *Clin Pharmacokinet* 1998;34:101-154). The model is linear, so it overpredicts total concentrations somewhat after 800 mg. Clearance matures quickly, to 90% of the adult value a month after term birth and 98% by three months (Anderson and Hannam 2019), so in children over about three months the allometric extrapolation is reasonable; in neonates and young infants it overpredicts clearance. The model says nothing about the enantiomers, CYP2C9 genotype, renal or hepatic impairment, gastrointestinal or renal toxicity, or fed-state absorption.
