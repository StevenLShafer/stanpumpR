### The model

Etomidate's parameters are from Arden and colleagues (*Anesthesiology* 1986;65:19-27), who studied etomidate across age and found that the elderly's increased sensitivity was explained by a smaller initial volume of distribution rather than by altered brain response. The model is a central volume of 0.09 L/kg with fixed rate constants.

### Covariates

Size only. By default, with *Adjust weight to fat-free mass* ticked, the volumes scale with the patient's fat-free mass relative to the 70 kg, 170 cm reference man and the clearances with that ratio to the 0.75 power, so height, age and sex enter through fat-free mass ([fat-free-mass scaling](help:models/fat-free-mass)). Arden's model is per kilogram of total body weight, with fixed rate constants: unticking the box restores that, scaling the volumes and clearances linearly with weight. Arden's age effect on V1 is not represented.

### Effect site

Arden and colleagues fitted the EEG median frequency with an effect compartment whose mean plasma-to-effect-site equilibration half-life was 1.6 minutes. The drug file supplies that ke0 (0.43/min) and the time to peak effect after a bolus is solved from it, about 1.9 minutes with these kinetics. Until October 2026 the 1.6 minutes was read as the time to peak effect itself, which implied a faster equilibration than Arden measured.

### Typical concentrations

The shaded band, 0.4 to 0.8 mcg/mL, is a hypnotic range taken in the code from a 2011 study in the *Journal of Clinical Pharmacology* (51:482-491). The default recovery threshold is 0.4 mcg/mL.

### Where to be careful

Etomidate's adrenocortical suppression, the reason it is not infused for maintenance, is pharmacodynamic and outside the model, which will happily simulate an infusion. The infusion units are offered for teaching, not as an endorsement.
