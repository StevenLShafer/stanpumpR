### A children's model, deliberately kept separate

This entry is fenoldopam in **children**, from Hammer and colleagues (*BMC Anesthesiol* 2008;8:6): 77 children enrolled and 76 analysed, from 3 weeks to 12.75 years of age, Tanner stage 1 or 2, having surgery in which deliberate hypotension was induced, all under general anaesthesia. It is named `fenoldopamPediatric`, as the paediatric methylphenidate entry is, so that it cannot be picked up as an adult model. Weight-adjusted clearance in these children was about 1.8 times the adult value the same paper quotes (3.06 against 1.66 L/h/kg), and the effective infusion rate was correspondingly higher, 0.8 to 1.2 mcg/kg/min against the adult label's 0.05 to 0.3. An adult simulated on this model would be given too much clearance.

There is **no adult fenoldopam entry**, and that is a statement about the evidence. The adult study (Frederickson and colleagues, 1988) reports a half-life of 9.8 minutes with a clearance of 30.3 mL/min/kg and a volume of 582 mL/kg; in one compartment those three imply a half-life of 13.3 minutes instead, so the reported volume is a multi-compartment summary and the compartment vector cannot be recovered from it.

### The model

A single compartment described the plasma data, fitted in NONMEM, with an elimination rate constant of 13.2 /h and a clearance of 3.06 L/h/kg, both with approximate standard errors (1.23 and 0.62). The paper reports **no volume of distribution**; the volume used here is the derived ratio of clearance to the rate constant, 0.232 L/kg. That derivation is consistent with the paper's own arithmetic, which prints a half-life of 3.15 minutes, and with its non-compartmental analysis, which gives 0.244 L/kg. The non-compartmental estimates (12.4 /h and 3.02 L/h/kg) are a separate analysis and are not mixed in.

### Covariates

Clearance is published per kilogram and there is no allometric exponent anywhere in the paper, so this model's own size term is linear in weight: it is applied to the fat-free-mass weight when [the scaling switch](help:models/fat-free-mass) is on and to total body weight when it is off. The children averaged 14 to 19 kg by group, so a per-kilogram extrapolation to a neonate or an adolescent follows the paper's convention rather than an estimated relationship.

### Blood pressure is not plotted

The paper also fitted a maximum-effect model of mean arterial pressure against plasma concentration, with a maximum effect of -27 mmHg and an EC50 of 9.1 ng/mL (approximate standard errors 5.4 and 4.23), and found baseline mean arterial pressure to be a significant covariate on the maximum effect. Those coefficients are recorded in the antihypertensive parameter audit and can be evaluated against a concentration you supply, but no pressure is drawn on the plot. Two reasons: the paper publishes no functional form or coefficient for the baseline-pressure covariate, so the model as fitted cannot be reproduced; and the population was anaesthetised children undergoing deliberate hypotension, whose baseline pressure is itself an anaesthetic state rather than a resting one. There is no effect site either, since no equilibration rate constant is published, and MEAC is zero because it is an opioid concept.

### Where to be careful

The shaded band is **derived from this paper's dosing conclusion, not from a measured therapeutic range**: 15.7 to 23.5 ng/mL is the steady-state concentration of the 0.8 to 1.2 mcg/kg/min the authors found effective in these children, with 19.6 the midpoint, obtained by dividing the rate by the clearance. Above 1.2 mcg/kg/min the paper found heart rate rose with no further fall in pressure, so the top of the band is where the benefit stopped, not a ceiling on what is safe. The whole model is a fit in anaesthetised children having deliberate hypotension induced; it is not a model of fenoldopam in a hypertensive child on a ward, and it says nothing about renal dose-response, which is a separate endpoint.
