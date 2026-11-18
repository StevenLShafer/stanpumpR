### Two models, chosen by age

Ondansetron uses two published models: **Mondick and colleagues' pediatric model below 18 years**, and **Chiang and colleagues' adult model from 18**. Neither study covers the years between them, so part of the age range is an extrapolation either way; see *The gap between the models* below.

### Children and adolescents (below 18): Mondick 2010

Mondick and colleagues (*Eur J Clin Pharmacol* 2010;66:77-86) pooled 745 samples from 124 surgical and oncology patients aged 1 to 48 months (median 16 months, 3.3 to 20 kg). Their two-compartment model scales every parameter by weight, normalized to a 10.4 kg child, with clearance also maturing with age:

- clearance = 1.53 × weight^0.75 × (1 − 0.760 × e^(−(age − 1) × ln 2 / 3.82)) L/h, age in months
- central volume = 0.930 L/kg; peripheral volume = 2.52 L/kg
- intercompartmental clearance = 15.0 × weight^0.75 L/h

Clearance matures with a half-life of 3.8 months: it is 76, 53 and 31 per cent below mature at 1, 3 and 6 months, and essentially mature by 19 months. Ages under 1 month are treated as 1 month, as in the paper, whose youngest patient was 1 month old; a neonate is therefore an extrapolation. Between-patient variability (clearance 57 per cent, central volume 110 per cent) is not shown.

### Adults (18 and over): Chiang 2021

Chiang and colleagues (*Br J Clin Pharmacol* 2021;87:516-526) gave 16 mg of intravenous ondansetron over 15 minutes to 15 adults aged 45 to 70 and sampled plasma for 3 hours (and cerebrospinal fluid once); one patient's samples could not be quantified, so the model was fitted to 14. Their two-compartment model has, at the median age of 58, clearance 24.6 L/h, central volume 63.3 L, intercompartmental clearance 211 L/h and peripheral volume 107 L. Half-times about 0.1 and 5.2 hours.

Chiang's one covariate is age on the central volume, in a power form about the median age: central volume = 63.3 × (age/58)^-4.91 L. The exponent is steep. Within the study it takes the central volume from 220 L at 45 to 25 L at 70, a ninefold range, so the early peak after a dose changes a great deal with age while the later curve, set by clearance and the peripheral volume, does not. Outside the ages studied the equation gives absurd volumes (1,600 L at 30), so **the age term is used only between 45 and 70**: an adult under 45 gets the 45-year-old's central volume and one over 70 the 70-year-old's. That holds the model inside its data, but it is not evidence of what the central volume is at 30 or at 85.

### The gap between the models

Mondick's data stop at 4 years and Chiang's start at 45. **No ondansetron model here is fitted to anyone aged 4 to 45.** The gap is filled from both ends:

- **4 to 18 years: Mondick, extrapolated.** Above 4 years the pediatric model is applied by weight alone: clearance is fully mature, so a 10-year-old is the 10.4 kg reference child scaled up allometrically. The authors support this: their model extrapolated to 70 kg gives a clearance of 0.53 L/h/kg, consistent with the 0.4 to 0.5 L/h/kg measured in adults and in surgical patients aged 3 to 12.
- **18 to 45 years: Chiang, held at 45.** Young adults get the 45-year-old's age term, whose central volume (220 L at the reference size) is about three times the central volume at Chiang's median age.

Other switch points were considered. Switching at 4 years would give children Chiang's 45-year central volume, about 3.5 times Mondick's. Switching at 45 would carry the pediatric model across all of adulthood. 18 was chosen as the age above which the adult model is the better guide, and the two models are not blended, because there are no data in the gap to say how they should join.

**The switch at 18 is abrupt.** For a 70 kg patient the day before and the day of their 18th birthday, before size scaling:

| | Mondick (17) | Chiang (18) |
|---|---|---|
| Clearance | 37.0 L/h | 24.6 L/h |
| Central volume | 65 L | 220 L |
| Intercompartmental clearance | 363 L/h | 211 L/h |
| Peripheral volume | 176 L | 107 L |

The same dose therefore gives a much lower early peak and a slower late decline at 18 than at 17. Neither is better supported than the other at that age; treat predictions anywhere from 4 to 45 years as uncertain.

### Covariates

Below 18: weight (all four parameters) and age (clearance maturation). Mondick's model carries its own weight covariate, so it is evaluated at the pharmacokinetic weight with the [fat-free-mass switch](help:models/fat-free-mass) on and at total weight with it off. From 18: age on the central volume, with the default fat-free-mass scaling, and the published values used unscaled with the switch off. The age terms apply with the switch either way.

### What is left out

Between-patient variability is not shown for either model, nor is Chiang's correlation between clearance and central volume (estimated but not reported). Beyond 3 hours the adult curve extrapolates Chiang's sampled window. Oral ondansetron is not offered; no absorption model was assembled.

### Effect site

None. There is no published equilibration delay or concentration-response model for preventing nausea and vomiting after surgery, and only the plasma concentration is plotted. The shaded band (5 to 40 ng/mL) covers what 4 to 8 mg produce in adults over the first hours. For orientation only: in an adult ipecac challenge, Cox and colleagues (*J Pharmacokinet Biopharm* 1999;27:625-644) estimated that the hazard of vomiting halves with each 1.4 ng/mL of ondansetron. That is neither a 5-HT3 receptor occupancy nor a probability of preventing PONV or chemotherapy-induced emesis.

### Where to be careful

The ages from 4 to 45 years, above all the switch at 18 (see *The gap between the models*). Neonates under 1 month. In adults, fourteen middle-aged and older patients, three hours of sampling, and an age term estimated from that small group (relative standard error 19 per cent) that moves the central volume ninefold across it. Hepatic impairment, CYP induction and QT effects are not represented.
