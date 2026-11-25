### The model

Mouksassi and colleagues' population analysis (*Clin Pharmacol Drug Dev* 2015) of recombinant human soluble thrombomodulin (ART-123) in 392 subjects: 24 healthy volunteers given 0.02 or 0.06 mg/kg, sampled for six days, and 368 adults with sepsis and suspected disseminated intravascular coagulation in the phase IIb trial, given 0.06 mg/kg (at most 6 mg) daily for six days as a bolus or a 15-minute infusion. Ages 18 to 93, weights 30 to 150 kg, creatinine clearances 5 to 150 mL/min. One compartment:

- clearance 0.14 L/h × (weight/68)^0.56 × (creatinine clearance/72.6)^0.27;
- volume 5.47 L × (weight/68)^0.50;
- a half-life of 27 hours.

The creatinine clearance is Cockcroft-Gault, as the paper used it. Half to three quarters of a dose is recovered in the urine.

### Covariates

- **Weight**, on clearance and volume, as fitted. With the fat-free-mass switch on, the weight in these terms is the pharmacokinetic weight; the Cockcroft-Gault equation keeps total weight.
- **Serum creatinine**, from the Patient Profile. Blank means an assumed normal creatinine for age and sex (0.9 mg/dL in men and 0.8 in women from 18 years). The creatinine clearance is held within the 5 to 150 mL/min the study included, since the power of it would otherwise give no clearance at all without kidney function.
- **Pregnancy**: with the creatinine blank, the creatinine clearance is raised 1.5-fold, the rise in filtration of pregnancy. No concentrations have been measured in pregnancy.

### No band

The paper estimates a "therapeutic range of the product" of 300 to 5,400 ng/mL: the lower edge the effective concentration estimated from non-clinical and clinical data, the upper the highest concentration without bleeding in non-clinical toxicology. No study has titrated a dose to it, and stanpumpR draws no clinical threshold on an anticoagulant, so it is not drawn.

### Product and doses

Thrombomodulin alfa is not approved in the United States (it is approved in Japan as Recomodulin, dosed in units). The doses here are in mg and mg/kg, as the trials gave it: 0.06 mg/kg daily, at most 6 mg.

### What is not plotted

Concentration only, in ng/mL. Thrombomodulin alfa binds thrombin and activates protein C; no coagulation marker or DIC score is drawn.
