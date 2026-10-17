### The model

Retlich and colleagues' population analysis of 462 patients with type 2 diabetes in four studies, given 0.5 to 10 mg once daily (*Clin Pharmacokinet* 2015). Linagliptin binds its target, the enzyme DPP-4, tightly (dissociation constant 0.0652 nmol/L) and at equilibrium, in plasma and in the tissues. The model has two compartments, and in each the drug is either free or bound to DPP-4:

- a central compartment of 715 L, with 4.97 nmol/L of binding sites in men and 5.43 in women, rising 0.56% a year from age 60;
- a peripheral compartment of 1650 L, with 1650 nmol of binding sites;
- an intercompartmental clearance of 412 L/h, and a clearance of 258 L/h that acts on the **unbound** drug only.

All the volumes and clearances are apparent (divided by the bioavailability). At the concentrations of the 5 mg dose the binding sites in plasma are nearly full. Below that most of the drug is bound and little is cleared; above it, more and more of the drug is free. Exposure therefore rises less than in proportion to the dose, and the terminal phase is very long. The curve comes from numerical integration over the whole dose table (the engine that runs [phenytoin](help:drugs/phenytoin)), and the plotted concentration is the total, bound and free, as the assay measured it.

Absorption is first order, 0.441/h, the rate of the phase 2b tablet. Bioavailability relative to the reference falls 0.958% for each kilogram above 88 kg.

### Covariates

- **Sex and age** act on the binding sites in plasma; age is held within the studied 30 to 78 years.
- **Weight** acts on bioavailability; it is held within the studied 57 to 132 kg.
- **Not applied:** baseline DPP-4 activity and GGT, which have no Patient Profile field, are at the population's medians.

The paper's dose terms (absorption 6.5% slower and binding sites 3.4% more plentiful per mg above 5 mg) are given their 5 mg values. That is the only marketed strength, and other doses are simulated with the same values.

### Product and doses

Tradjenta, `mg PO` and `mg PO qd`. On 5 mg daily, a 60-year-old 88 kg man reaches a steady-state trough of 5.0 nmol/L (2.37 ng/mL) and a peak of 9.0 nmol/L (4.27 ng/mL). The paper's median trough is 5.29 nmol/L, and its predictive check shows a median peak near 9.

### What is not plotted

Concentration only, in ng/mL (1 nmol/L is 0.4725 ng/mL). Plasma DPP-4 inhibition, which the paper relates directly to the concentration with an Emax model, is not drawn, and neither is glucose. See [Antidiabetic models and their limits](help:models/antidiabetics).
