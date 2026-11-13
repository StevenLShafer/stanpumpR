### The model

Vincristine in children comes from Nijstad and colleagues (*Pharm Res* 2022;39:2487-2495): 206 patients from Dutch and British cohorts, aged 2 weeks to 34 years (median 8), 25 of them under a year, weighing 2.9 to 126 kg, given about 1.4 mg/m² (capped at 2 mg) by bolus or short infusion. The adult model is [vincristine](help:drugs/vincristine) (Igarashi 2021).

Two compartments describe the disposition, scaled by weight to a 70-kg reference:

- clearance 30.6 L/h × (weight / 70)^0.75;
- central volume 5.39 L × weight / 70;
- a peripheral compartment of 400 L × weight / 70, exchanging at 63.2 L/h × (weight / 70)^0.75.

To these the paper adds a **saturable binding pool**, β-tubulin in the tissues, filled from the central compartment and emptied back into it. Its capacity is 0.525 mg × (weight / 70) × (age / 18)^−0.199, so an infant's is, per kilogram, larger than an adult's (3.4 times at 2 weeks). Binding is very fast (1300 per hour) and release slower (11.5 per hour), so at low concentrations most of the drug in the central compartment is bound, while a large dose saturates the pool.

The binding is so fast that it is held at its equilibrium with the unbound drug rather than integrated as a process. Against a fine-step integration of the full binding equations this is within about 1% at 5 minutes and better from 30 minutes on.

### What is plotted

The **unbound** plasma vincristine in ng/mL, the concentration the paper fitted; the bound drug is in the tissues, not in the sample. Doses are entered in mg as labelled (vincristine sulfate); a dose prescribed per m² must be multiplied by the body surface area first, and is usually capped at 2 mg, with infant reductions. There is no effect site and no band.

Because the binding saturates, doses do not add independently: four times the dose gives about sixteen times the unbound concentration at 5 minutes and about twice at 1 hour. The binding also lengthens the terminal phase (an apparent half-life of about 19 hours in an adult, against 13.5 hours from the two compartments alone). Every microgram given is still cleared at the same clearance, so the area under the curve is the dose over the clearance.

### Covariates

- **Weight**, on every parameter but the binding and release rates (taken as not scaled; the paper does not say).
- **Age**, on the binding capacity only. The term is held at 0.04 years (two weeks), the youngest age studied, for younger patients.
- Sex, height and renal function are not covariates; platelets were tested and not retained.

### Where to be careful

- **Liver disease and CYP3A4 drugs** (azoles) were not covariates and raise exposure well above this curve.
- **Sampling times are not given** in the paper (four to eight samples per patient), so the early minutes rest on the model's structure.
- **Intravenous only: never intrathecal.**
- **Not a monitoring tool:** the curve is a population prediction, not an estimate from measured levels.

Every parameter was read in the paper's PDF and supplement. See [chemotherapy](help:models/chemotherapy) for the other models considered.
