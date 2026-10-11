### What is plotted

The plasma concentration of **aprepitant**, in ng/mL, after intravenous fosaprepitant entered in mg of fosaprepitant (the free acid, as vials are labelled, not the dimeglumine salt). The prodrug itself is not plotted or modelled.

### Dose basis

Fosaprepitant is converted to aprepitant within minutes. The conversion is taken as instantaneous and complete, so each mg of fosaprepitant yields the molar equivalent, 534.44 / 614.40 = 0.870 mg of aprepitant: 130 mg from a 150 mg vial. That conversion is applied once, by dividing the model's volume and clearance by 0.870, which leaves the half-life unchanged and multiplies every concentration by 0.870. The parameter table therefore shows a volume and clearance 15 per cent larger than [aprepitant](help:drugs/aprepitant)'s.

### The model

The disposition is Nijstad and colleagues' (*J Oncol Pharm Pract* 2023;29:899-904): one compartment, clearance 5.83 × (weight/70)^0.75 L/h and volume 86.8 × weight/70 L, from children aged 0.7 to 17.9 years. Only five of them received intravenous fosaprepitant (4.4 to 13.8 years, 19.5 to 52.4 kg). Outside that group, including adults, the model is an **extrapolation** of the weight equations, and it should not be used below 6 months of age. The FDA's pediatric review of fosaprepitant (2018) fitted a larger two-compartment model, which was read for context; its parameters are not mixed with Nijstad's.

### Covariates

The model carries its own weight covariate, so it is evaluated at the pharmacokinetic weight with the [fat-free-mass switch](help:models/fat-free-mass) on and at total weight with it off. No age maturation was fitted.

### Effect site

None; only the plasma aprepitant is plotted. The band (116 to 1500 ng/mL) is explained on the [aprepitant](help:drugs/aprepitant) page: its lower edge is where an adult PET relation gives 90 per cent NK1 occupancy, an unvalidated reading for an acute intravenous dose or a child.

### Where to be careful

As for aprepitant: one compartment, so an uncertain early peak; variability not shown; no CYP3A4 interactions.
