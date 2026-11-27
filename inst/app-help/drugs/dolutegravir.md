### The model

Dolutegravir's parameters come from the population model of Zhang and colleagues (*Br J Clin Pharmacol* 2015;80:502-514, doi 10.1111/bcp.12639). It was fitted to 3,357 plasma concentrations from 563 adults with HIV-1 who had never been treated, in three studies: a 10-day monotherapy study (19 patients) and the SPRING-1 (141) and SPRING-2 (403) trials, at 10, 25 or 50 mg once daily. The patients were 18 to 68 years old and weighed 39 to 135 kg; 85% were men and 42% smoked. The model has one compartment and a lagged first-order absorption, with apparent (oral) parameters:

- **Clearance:** CL/F 0.901 L/h × (weight / 70)<sup>0.438</sup> × (age / 40)<sup>0.193</sup>.
- **Volume:** V/F 17.4 L × (weight / 70)<sup>0.768</sup>.
- **Absorption:** ka 2.24 /h after a lag of 0.263 h.
- **Sex:** women's bioavailability was 1.21 times men's. Because the parameters are apparent, stanpumpR divides a woman's CL/F and V/F by 1.21.

The reference patient is a 40-year-old man of 70 kg who does not smoke, with a total bilirubin of 9 µmol/L. The curve is total dolutegravir in plasma, of which at least 98.9% is bound to protein.

### What is held at the reference

The source found four more effects that stanpumpR cannot apply or chose not to:

- **Smoking** raised clearance by 16%. The Patient Profile has no smoking field, so the curve is a non-smoker's; a smoker's exposure is about 14% lower.
- **Bilirubin** competes with dolutegravir for UGT1A1. Over the studied 3 to 38 µmol/L, clearance ran from 26% higher to 26% lower. The curve assumes 9 µmol/L, the cohort's median.
- **The monotherapy study** had a clearance 35% higher than the two trials, which the authors could not explain. It is not applied.
- **The 10 mg dose** had a bioavailability 24% higher than 25 or 50 mg, which the authors put down to the smaller tablet dispersing better. It is not applied, so a 10 mg dose is drawn about a fifth low against this source. The adult dose is one 50 mg tablet.

### The age term

Clearance rises slightly with age in this model, which the authors did not expect and say is poorly supported: only 17 of the 563 patients were over 55. The effect is small (0.86 at 18 years, 1.08 at 60). Below 18 years the age is held at 18, the youngest the source studied.

### Checks

In the 403 patients of SPRING-2 on 50 mg once daily, the paper reports geometric means at steady state of AUC 53.6 mcg·h/mL, peak 3.69 mcg/mL at 2.0 hours and trough 1.10 mcg/mL. The reference man in this model gives 55.5 mcg·h/mL, 3.72 mcg/mL at 1.8 hours and 1.21 mcg/mL. He does not smoke, as 42% of the trial's patients did.

The Tivicay label gives the same exposure values, an apparent volume of 17.4 L, an apparent clearance of 1.0 L/h and a terminal half-life of "approximately 14 hours" (13.4 hours here). These are not an independent check: the label's values come from this analysis and those that followed it.

### Children

The source studied no one under 18, and the model reads **high** in children. The label reports that children of 20 kg or more given 50 mg once daily had an AUC of 55.0 mcg·h/mL, the same as adults. This model gives a 30 kg child about 94 mcg·h/mL. Treat a curve for a child as an overestimate. The 5 mg tablets for oral suspension given to small children (Tivicay PD) have about 1.6 times the bioavailability of the film-coated tablets and are not the product this model describes.

### Food

Food raises the AUC by 33% to 66% and the peak by 46% to 67%, more with a fattier meal, and delays the peak from 2 hours to between 3 and 5 hours (label). Tivicay may be taken with or without food. The source fitted no food term, and its patients took the drug as outpatients, so the curve describes the trials as they were run.

### Dose

"Each tablet contains 50 mg of dolutegravir (as dolutegravir sodium)". The source dosed in mg of dolutegravir. Doses are therefore entered as labelled, in mg of dolutegravir. The usual adult dose is 50 mg once daily, or 50 mg twice daily with certain enzyme inducers or when integrase-inhibitor resistance is suspected.

### Body size

Weight acts on clearance and volume by the source's own estimated powers. With [the scaling switch](help:models/fat-free-mass) on, it is the pharmacokinetic weight; with it off, total body weight.

### Not modelled

- **Interactions.** Inducers such as rifampin and efavirenz lower dolutegravir concentrations, which is why the label doubles the dose with them. The label attributes the less than proportional exposure at 50 mg twice daily to inducers in those patients' regimens. Antacids and supplements containing calcium, iron, magnesium or aluminium reduce absorption.
- **UGT1A1 genotype.** Poor metabolisers had a clearance 32% lower in a meta-analysis of healthy volunteers (label). There is no field for it.
- **Unbound dolutegravir.**

### No band

No therapeutic range is established, and the source found no relation between exposure and virological response at 10 to 50 mg once daily. Studies often compare troughs with 0.064 mcg/mL, the protein-adjusted concentration that inhibits 90% of viral replication in cell culture. That is a floor from laboratory work, far below the usual trough, and is not drawn.
