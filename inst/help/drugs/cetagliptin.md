### The model

Zhou and colleagues' study of cetagliptin, an investigational DPP-4 inhibitor (*Front Endocrinol* 2024). The data were 20 Chinese adults with type 2 diabetes, about 71 kg, given 50 or 100 mg once daily, fasting, for 14 days. The parameter values are in the paper's Supplementary Table 1.

- **Disposition:** two compartments, a central volume of 8.67 L, a peripheral volume of 558 L, and an intercompartmental clearance of 9.67 L/h.
- **Elimination:** saturable, Vmax 9373 µg/h with Km 171.5 ng/mL. Below Km the clearance is 54.7 L/h, and it falls as the concentration rises.
- **Absorption:** first order, 0.0652/h, with no bioavailability term: the parameters are apparent ones, from oral data only.

The paper's text calls this "the two-compartment model" without naming the elimination. Its supplement gives a Km and a Vmax and no clearance, so the elimination is Michaelis-Menten. The data agree: the first day's exposure is 2.55 times higher at 100 mg than at 50 mg. The curve comes from numerical integration over the whole dose table, the engine that runs [phenytoin](help:drugs/phenytoin).

Elimination below Km is fast compared with absorption, so the concentration follows the absorption and peaks within about two hours. The long terminal phase, about 40 hours, is the drug returning from the large peripheral volume. Steady state takes about a week.

Against the paper's observed means, the model's first-day AUC is 751 ng·h/mL at 50 mg (observed 717) and 1819 at 100 mg (observed 1830). At steady state the peak at 100 mg is 299 ng/mL (observed 300).

### Covariates

None applied. Total bilirubin is in the final model as a covariate on the peripheral volume ("V2 increases with the increase of TBIL"). The paper prints neither the form of the term nor its reference bilirubin, and the Patient Profile has no bilirubin field, so the curve uses the typical peripheral volume. Sex, weight, liver enzymes, lipids, glucose, urea and creatinine were tested and not retained. The volumes, Vmax and the intercompartmental clearance are scaled by the library's fat-free-mass factors; with **Adjust weight to fat-free mass** unticked they are the published values.

### Doses

`mg PO` and `mg PO qd`, as the paper gives them; it does not name a salt. Only 50 and 100 mg once daily were studied. With saturable elimination, a curve at any other dose is an extrapolation of the model, not a measurement.

### What is not plotted

Concentration only, in ng/mL. The DPP-4 inhibition the paper fitted to the same data is not drawn: a sigmoid Emax model with maximum inhibition 91.8% and a half-maximal concentration of 5.12, whose unit the supplement does not state. See [Antidiabetic models and their limits](help:models/antidiabetics).
