### The model

Dexamethasone's parameters are from Hong and colleagues (*Pharm Res* 2007;24:1088-1097), a two-compartment population model of intravenous dexamethasone phosphate in five healthy men: clearance 18.1 L/h, central volume 41.6 L, k12 1.09/h and k21 1.02/h, which give an intercompartmental clearance of 45.3 L/h and a peripheral volume of 44.5 L. Half-times 0.29 and 3.7 hours.

### Dose basis

Dexamethasone is labelled three ways: 4 mg of the phosphate is 3.3 mg of base and 4.4 mg of the sodium phosphate. The model is used on the convention common vial labels follow, dexamethasone **phosphate** milligrams, with no further correction. A dose already stated as base is about 20 per cent more potent per labelled milligram than the model assumes.

### Oral and intramuscular routes

Oral bioavailability, 0.81, is from Spoorenberg and colleagues' parallel-group comparison in pneumonia (*Br J Clin Pharmacol* 2014;78:78-83; 95% CI 0.54 to 1.21). The oral (0.936/h) and intramuscular (0.460/h) absorption constants are from Krzyzanski and colleagues' population model of oral and intramuscular dexamethasone phosphate in healthy women (*J Pharmacokinet Pharmacodyn* 2021;48:261-272), which also found oral availability 1.04 times intramuscular, so intramuscular bioavailability is 0.78. Pairing these with Hong's disposition is a cross-study assembly, not a joint fit.

### Covariates

None in the source. The parameters take the default [fat-free-mass scaling](help:models/fat-free-mass) and are used as published with the switch off.

### Effect site

None; the glucocorticoid effect is genomic and takes hours. Only the plasma concentration is plotted. The shaded band (20 to 100 ng/mL) covers what 4 to 8 mg produce over the first hours.

### Where to be careful

Five healthy men. No dependence on age, sex, weight beyond fat-free mass, or CYP3A4 induction is represented, and the antiemetic and analgesic-sparing effects for which anaesthetists give dexamethasone have no concentration-effect model here.
