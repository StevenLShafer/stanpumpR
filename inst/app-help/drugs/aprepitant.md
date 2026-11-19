### What is plotted

The plasma concentration of aprepitant, in ng/mL, after aprepitant given **intravenously** (the injectable emulsions), dosed in mg of aprepitant. For the prodrug, see [fosaprepitant](help:drugs/fosaprepitant).

### The model

The disposition is from Nijstad and colleagues (*J Oncol Pharm Pract* 2023;29:899-904), a population model of aprepitant in children aged 0.7 to 17.9 years (8.4 to 66.3 kg): one compartment, clearance 5.83 × (weight/70)^0.75 L/h and volume 86.8 × weight/70 L. Only the intravenous branch is used; the oral absorption is not.

### A formulation extrapolation

Nijstad's intravenous data came from **fosaprepitant**, not from aprepitant itself, and in only five children (4.4 to 13.8 years, 19.5 to 52.4 kg). No population model fitted to intravenous aprepitant was available, so this drug borrows the disposition inferred after the prodrug. Adults, infants and older adolescents are extrapolations of the weight equations, and the model should not be used below 6 months of age, where CYP3A4 is immature.

### Covariates

The model carries its own weight covariate, so it is evaluated at the pharmacokinetic weight with the [fat-free-mass switch](help:models/fat-free-mass) on and at total weight with it off. No age maturation was fitted.

### Effect site

None; only the plasma concentration is plotted. The shaded band runs from 116 ng/mL, where the adult PET relation of Iihara and colleagues (*Support Care Cancer* 2026;34:933), occupancy = 98.1 × Cp / (10.4 + Cp), gives 90 per cent striatal NK1 occupancy, to 1500 ng/mL, about the peak after 150 mg of fosaprepitant in an adult. That relation comes from troughs after repeated oral dosing in adults; applying it to an acute intravenous dose, or to a child, is unvalidated, and occupancy is not a probability of preventing nausea or vomiting.

### Where to be careful

One compartment cannot show the early distribution phase, so the peak at the end of a short infusion is uncertain. Between-patient (clearance 25 per cent) and between-occasion (30 per cent) variability are not shown. CYP3A4 inhibitors and inducers, and aprepitant's own inhibition of CYP3A4 (which raises dexamethasone and midazolam concentrations), are not represented.
