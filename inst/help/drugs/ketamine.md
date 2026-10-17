### The model

Ketamine's parameters are from Domino and colleagues (*Clin Pharmacol Ther* 1984;36:645-653), who gave 2.2 mg/kg of racemic ketamine intravenously to eight healthy men, with and without diazepam. The paper reports means of individually fitted quantities: a central volume of 0.063 L/kg, a clearance of 0.848 L/kg/h (0.99 L/min at 70 kg), a deep volume of about 1.51 L/kg and three half-lives of 24 seconds, 4.7 minutes and 2.2 hours. The library's rate constants are the one set that reproduces all of these at once; they imply a steady-state volume of 1.7 L/kg. Until October 2026 the library multiplied the paper's mean central volume by its mean elimination rate constant, which gave a clearance of 1.9 L/min at 70 kg, twice the paper's, and a deep volume of 2.5 L/kg; means of individually fitted volumes and rate constants do not multiply into the mean clearance. The placebo arm is modelled; diazepam premedication slowed redistribution.

### Covariates

Size only. By default, with *Adjust weight to fat-free mass* ticked, the volumes scale with the patient's fat-free mass relative to the 70 kg, 170 cm reference man and the clearances with that ratio to the 0.75 power, so height, age and sex enter through fat-free mass ([fat-free-mass scaling](help:models/fat-free-mass)). Domino's model is per kilogram of total body weight, with fixed rate constants: unticking the box restores that, scaling the volumes and clearances linearly with weight.

### Effect site

The time to peak effect is 3 minutes. It is an assumption: Domino measured no effect, and no separately sourced equilibration has been adopted.

### Typical concentrations

The shaded band, 0.1 to 0.16 mcg/mL, is an **analgesic** range for sub-anesthetic ketamine, from a 2013 review cited in the code. Anesthetic concentrations are an order of magnitude higher, around 1 to 2 mcg/mL. The default recovery threshold is 0.1 mcg/mL. See [the ketamine infusion scenario](scenario:ketamine-infusion).

### Where to be careful

Norketamine, an active metabolite with about a third of ketamine's potency that accumulates during infusions, is not modelled. The S-enantiomer, used alone in many countries, has different kinetics. Domino's volunteers were young and healthy.
