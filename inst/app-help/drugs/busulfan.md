### The model

Busulfan comes from McCune and colleagues (*Clin Cancer Res* 2014;20:754-763), a population analysis of 12,380 plasma concentrations from 1,610 recipients of hematopoietic cell transplants at 51 institutions, aged 0.1 to 66 years (466 under 2), all given intravenous busulfan. It is the one busulfan model that spans infancy to adulthood. Two compartments describe the disposition:

- clearance 11.4 L/h, central volume 13.9 L, intercompartmental clearance 135.2 L/h and peripheral volume 29.9 L, for an adult whose normal fat mass corresponds to a total weight of 70 kg;
- size by **normal fat mass**, the fat-free mass plus a fraction of the fat: 51% of the fat for the clearances and 20% for the volumes, with the clearances scaled to the ¾ power and the volumes in proportion. Fat-free mass is Janmahasatian's, from weight, height and sex;
- **maturation** of clearance by postmenstrual age, half the adult value at 45.7 weeks (about 6 weeks after a term birth) and 95% by about 2½ years. The Patient Profile's gestational age at birth enters here; left blank it is 40 weeks, as the paper assumed for every infant;
- volumes 7% larger in women;
- clearance that **falls during the course**: from 6 hours after the first dose it is 0.932 of the first 6 hours' value, and from 36 hours 0.919. The model switches at 6 and 36 hours after the drug's first dose.

### What is plotted

Plasma busulfan in ng/mL. Doses are entered in mg or mg/kg, or as an infusion rate. A dose prescribed per m² must be converted first. The targets are exposures chosen by the treating team: an average steady-state concentration (the paper's example is 770 ng/mL with a window of 592 to 963), or a daily or cumulative AUC. They are not drawn as a band. There is no effect site.

### Covariates

Weight, height, sex and age all enter, through normal fat mass, maturation and the volumes' sex term. Disease (malignant or not) made no difference. Renal function is not a covariate: busulfan is cleared by conjugation with glutathione. The model carries its own fat-free-mass covariate, so [the scaling switch](help:models/fat-free-mass) leaves it unchanged.

### Where to be careful

- **The paper's worked example disagrees with its own equation.** Its supplement computes a fat-free mass of 49.7 kg for a 70-kg, 176-cm man where the equation the Methods state gives 56.1. The model uses the stated equation. With it, the example's 204 mg daily dose gives an average concentration of 752 ng/mL, within 3% of the example's 770.
- **The fall in clearance is counted from the drug's first dose in the plot.** A second course plotted later is not reset. Langenhorst 2020 links the fall to glutathione depletion and makes it larger after 40 years of age; that model needs an engine feature the library does not have yet.
- **Not a monitoring tool.** The curve is a population prediction for the covariates entered. Busulfan dosing is normally adjusted to measured concentrations, which this curve does not use.

The parameters are from the article's Table 2, its Methods and its supplement. See [chemotherapy](help:models/chemotherapy) for the other models considered.
