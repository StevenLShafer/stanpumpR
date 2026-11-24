### The model

Tafenoquine's parameters come from the population model of Thakkar and colleagues (*Antimicrob Agents Chemother* 2018;62:e00711-18, doi 10.1128/AAC.00711-18). It was fitted to 5,286 plasma concentrations from 675 people in five GSK studies: 193 healthy volunteers and 482 patients with *Plasmodium vivax* malaria, aged 15 to 79 years and weighing 37 to 138 kg, given single doses of 300 to 1,200 mg as tablets or capsules. A sixth study of 166 patients, held out of the fit, was predicted well by the model. The model has two compartments and a lagged first-order absorption, with apparent (oral) parameters:

- **Clearance:** CL/F 2.96 L/h × (weight / 70)<sup>0.75</sup>.
- **Volumes:** V2/F 915 L and V3/F 664 L, each × weight / 70.
- **Intercompartmental clearance:** Q/F 5.09 L/h × (weight / 70)<sup>0.75</sup>.
- **Absorption:** ka 0.252 /h after a lag of 0.908 h (tablets).

The weight exponents were fixed, not estimated. The paper does not print the weight they are normalised to; stanpumpR takes 70 kg, the usual reference (the cohort's median was 69.3 kg). Age, sex and race were tested and had no relevant effect.

### Patients, not healthy volunteers

The healthy volunteers had a central volume 1.35 times, and a peripheral volume 0.347 times, the patients'. The authors suggest two reasons: the patients also took chloroquine, which raises the tafenoquine peak, and acute malaria brings dehydration and leaky capillaries. stanpumpR draws the **patients'** curve, the population given Krintafel (with chloroquine). A traveller taking Arakoda for prophylaxis is a healthy volunteer: for 200 mg, the healthy volunteers' volumes give a peak about a quarter lower (148 against 194 ng/mL). Which state a prophylaxis curve should show is a decision still to be made.

### Checks

The labels were not used in the fit. The Krintafel label gives an apparent clearance of "approximately 3 L/h" (this model 2.96 L/h at 70 kg) and an apparent volume of "~1,600 L" (this model 1,579 L). It says the peak comes "12 to 15 hours" after a dose; this model peaks at 15.4 hours after 300 mg, at 291 ng/mL. Its terminal half-life, "approximately 15 days", compares with 17.2 days here; the Arakoda label gives 16.5 days (10.8 to 27.3) in healthy volunteers.

The Arakoda label measured 200 mg in 65 healthy volunteers: a peak of 147 ng/mL at 14 hours and an AUC of 70 h·mcg/mL. This model gives an AUC of 67.6 h·mcg/mL. With the healthy volunteers' volumes its peak is 148 ng/mL at 16.4 hours, but the curve uses the patients' volumes and peaks at 194 ng/mL.

### Food

Both labels say to take tafenoquine with food, which raised the AUC by 41% and the peak by 31% for a capsule. The source fitted no food term, so the curve describes its trials as they were run.

### Dose

"Each KRINTAFEL tablet contains 150 mg of tafenoquine (equivalent to 188.2 mg tafenoquine succinate)", and Arakoda's 100 mg tablets are labelled the same way. The source dosed in mg of tafenoquine. Doses are therefore entered as labelled, in mg of tafenoquine. Krintafel is a single 300 mg dose; Arakoda is 200 mg daily for 3 days, then 200 mg weekly, then one 200 mg dose in the week after leaving.

### Body size and age

Weight acts on every volume and clearance by the source's own fixed allometric powers. With [the scaling switch](help:models/fat-free-mass) on, it is the pharmacokinetic weight; with it off, total body weight. Krintafel is approved from 16 years and Arakoda from 18. Younger patients are an extrapolation of the weight terms.

### Not modelled

Whole blood concentrations, which the labels say average 67% higher than plasma, are not shown. Neither is the haemolysis tafenoquine causes in G6PD deficiency, its defining hazard: patients must be tested for G6PD before it is given.

### No band

No therapeutic range is established. The doses are fixed.
