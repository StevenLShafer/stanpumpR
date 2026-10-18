### The model

The sponsor's population model, published by Vashi and colleagues (*Epilepsy Res* 2023;195:107185) and fitted to 960 healthy volunteers and adults with focal seizures, most of them taking 200 mg a day. The paper's full text was not available, so the numbers are taken from the FDA's clinical pharmacology review of the new drug application (NDA 212839, 2019), which reprints the final parameter table: two compartments, apparent oral, with an absorption lag of 0.24 hours, absorption at 2.74/h, a clearance of 0.533 L/h, central and peripheral volumes of 42.4 and 7.27 L, and an intercompartmental clearance of 1.84 L/h.

The terminal half-life is about 65 hours (the label gives 50 to 60). At 200 mg once daily the steady state averages 15.6 mg/L, close to the phase 3 mean of 15.5 mg/L. Steady state takes about two weeks, which is why the label titration (12.5 mg/day, then 25, 50, 100, 150 and 200 mg/day, each for two weeks) is worth simulating as a whole.

### What is not applied

The review prints the covariate coefficients but not the equations or their reference values, so none of them can be evaluated exactly:

- **Daily dose on clearance.** If the term is a power of the daily dose, which is the likely reading, it changes clearance by less than 4% between 100 and 400 mg/day. That agrees with the multiple-dose studies: at steady state the apparent clearance was 0.40 to 0.52 L/h at every daily dose from 50 to 500 mg, with no trend (Vernillet 2020). After a *single* dose cenobamate is non-linear: the apparent clearance fell from about 1.3 to 1.4 L/h at 5 to 10 mg to 0.40 to 0.48 L/h at 400 to 750 mg. The model does not reproduce that. It overpredicts the total exposure to one small dose (2.2-fold at 5 mg, 1.2-fold at 100 mg) and underpredicts it after one very large dose (0.73 at 750 mg). It does reproduce the first day of daily dosing.
- **Body size.** The source puts body surface area on clearance and weight on the central volume; both are replaced by the library's [fat-free-mass scaling](help:models/fat-free-mass), and with the switch off the published values are used for everyone.
- **Ethnicity and bilirubin.** Clearance was 14.7% lower in African American and 13.3% lower in Hispanic subjects, and fell with bilirubin. The Patient Profile has no fields for these.
- **Other antiseizure drugs.** Clobazam lowered cenobamate clearance by 19% and carbamazepine raised it by 15%; phenytoin lowers cenobamate exposure by 27 to 28% (label). There is no co-medication field.

### How the curve compares with healthy volunteers

Vernillet and colleagues (2020) report the ascending-dose studies in healthy adults. These studies are probably part of the data the model was fitted to, so the comparison checks consistency rather than validating the model independently:

- **First day of daily dosing (50 to 300 mg):** the model's 24-hour exposure is 0.87 to 1.05 times the observed, so absorption and distribution are right.
- **Steady state (50 to 500 mg/day, day 14 or 17):** the model's exposure is about 0.80 times the observed, its peak 0.74 and its trough 0.76, at every dose. The healthy volunteers' steady-state clearance was about 0.43 L/h, against the model's 0.533.

Other studies in healthy volunteers agree: a median trough of 18.4 mg/L at 200 mg/day (Greene 2022) and mean peaks of 23 mg/L at 200 mg/day and 64 mg/L at 500 mg/day (Darpo 2022). The curve is therefore about 20% low for a healthy adult. It is right for a patient with focal epilepsy on adjunctive therapy, whose other antiseizure drugs raise cenobamate clearance, provided the patient is not African American or Hispanic and takes no drug the model names.

A later sponsor analysis (Krishna and colleagues, AES 2025, 1101 subjects) prints only a clearance of 0.282 L/h and volumes of 38.6 and 6.72 L, without the absorption rate, intercompartmental clearance or covariate equations. Read as typical values, these give a half-life of at least 95 hours and twice the observed concentrations, so they must depend on covariate reference values the abstract does not give. They are not used.

Cenobamate itself changes other drugs' kinetics (it inhibits CYP2C19, raising phenytoin, phenobarbital and N-desmethylclobazam, and induces CYP3A4 and CYP2B6); none of these interactions is modelled.

### Band

None. No therapeutic reference range has been established. In an Italian cohort, patients who responded at 100 mg/day or less had plasma concentrations from 0.5 to 17.6 mg/L, median about 5 to 6 (Operto 2025). There is no effect site.
