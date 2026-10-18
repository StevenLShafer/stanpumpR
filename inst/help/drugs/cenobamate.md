### The model

The sponsor's population model, published by Vashi and colleagues (*Epilepsy Res* 2023;195:107185) and fitted to 960 healthy volunteers and adults with focal seizures, most of them taking 200 mg a day. The paper's full text was not available, so the numbers are taken from the FDA's clinical pharmacology review of the new drug application (NDA 212839, 2019), which reprints the final parameter table: two compartments, apparent oral, with an absorption lag of 0.24 hours, absorption at 2.74/h, a clearance of 0.533 L/h, central and peripheral volumes of 42.4 and 7.27 L, and an intercompartmental clearance of 1.84 L/h.

The terminal half-life is about 65 hours (the label gives 50 to 60). At 200 mg once daily the steady state averages 15.6 mg/L, close to the phase 3 mean of 15.5 mg/L. Steady state takes about two weeks, which is why the label titration (12.5 mg/day, then 25, 50, 100, 150 and 200 mg/day, each for two weeks) is worth simulating as a whole.

### What is not applied

The review prints the covariate coefficients but not the equations or their reference values, so none of them can be evaluated exactly:

- **Daily dose on clearance.** If the term is a power of the daily dose, which is the likely reading, it changes clearance by less than 4% between 100 and 400 mg/day. That agrees with the multiple-dose studies, where exposure was proportional to dose from 50 to 500 mg/day (Vernillet 2020). After a *single* dose cenobamate is non-linear: exposure rises faster than dose, and the apparent clearance falls from about 1.4 L/h at low doses to 0.5 L/h. The model, fitted mostly at steady state, does not reproduce that and overpredicts the curve after one small dose.
- **Body size.** The source puts body surface area on clearance and weight on the central volume; both are replaced by the library's [fat-free-mass scaling](help:models/fat-free-mass), and with the switch off the published values are used for everyone.
- **Ethnicity and bilirubin.** Clearance was 14.7% lower in African American and 13.3% lower in Hispanic subjects, and fell with bilirubin. The Patient Profile has no fields for these.
- **Other antiseizure drugs.** Clobazam lowered cenobamate clearance by 19% and carbamazepine raised it by 15%; phenytoin lowers cenobamate exposure by 27 to 28% (label). There is no co-medication field.

Healthy volunteers run higher than the curve: a median trough of 18.4 mg/L at 200 mg/day (Greene 2022) and mean peaks of 23 mg/L at 200 mg/day and 64 mg/L at 500 mg/day (Darpo 2022), 20 to 30% above the model. The curve is closest to a patient with focal epilepsy who is not African American or Hispanic and takes no interacting drug.

Cenobamate itself changes other drugs' kinetics (it inhibits CYP2C19, raising phenytoin, phenobarbital and N-desmethylclobazam, and induces CYP3A4 and CYP2B6); none of these interactions is modelled.

### Band

None. No therapeutic reference range has been established. In an Italian cohort, patients who responded at 100 mg/day or less had plasma concentrations from 0.5 to 17.6 mg/L, median about 5 to 6 (Operto 2025). There is no effect site.
