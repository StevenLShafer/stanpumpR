### The model

The sponsor's population model, published by Vashi and colleagues (*Epilepsy Res* 2023;195:107185) and fitted to 960 healthy volunteers and adults with focal seizures, most of them taking 200 mg a day. The parameters are from the paper's supplement, and the FDA's clinical pharmacology review of the new drug application (NDA 212839, 2019) prints the same table: two compartments, apparent oral, with an absorption lag of 0.24 hours, absorption at 2.74/h, a clearance of 0.533 L/h, central and peripheral volumes of 42.4 and 7.27 L, and an intercompartmental clearance of 1.84 L/h.

The terminal half-life is about 65 hours (the label gives 50 to 60). At 200 mg once daily the steady state averages 15.6 mg/L, close to the phase 3 mean of 15.5 mg/L. Steady state takes about two weeks, which is why the label titration (12.5 mg/day, then 25, 50, 100, 150 and 200 mg/day, each for two weeks) is worth simulating as a whole.

### What is not applied

Each continuous covariate enters linearly about its median in the analysis population. Neither the paper nor the FDA or EMA assessment reports print the medians, so none of these terms can be evaluated exactly:

- **Daily dose on clearance.** If the term is a power of the daily dose, which is the likely reading, it changes clearance by less than 4% between 100 and 400 mg/day. That agrees with the multiple-dose studies: at steady state the apparent clearance was 0.40 to 0.52 L/h at every daily dose from 50 to 500 mg, with no trend (Vernillet 2020). After a *single* dose cenobamate is non-linear: the apparent clearance fell from about 1.3 to 1.4 L/h at 5 to 10 mg to 0.40 to 0.48 L/h at 400 to 750 mg. The model does not reproduce that. It overpredicts the total exposure to one small dose (2.2-fold at 5 mg, 1.2-fold at 100 mg) and underpredicts it after one very large dose (0.73 at 750 mg). It does reproduce the first day of daily dosing.
- **Body size.** The source puts body surface area on clearance and weight on the central volume. Both are replaced by the library's [fat-free-mass scaling](help:models/fat-free-mass), and with the switch off the published values are used for everyone. Centred on the 70 kg reference man, the two agree within 5% for men between 60 and 90 kg. They differ for women, for whom the source had no sex term, and in obesity.
- **Ethnicity and bilirubin.** Clearance was 14.7% lower in African American subjects and fell with bilirubin. Hispanic ethnicity changed a parameter by 13.3%; the paper's tables disagree on whether it was the clearance or the central volume. The Patient Profile has no fields for these.
- **Other antiseizure drugs.** Clobazam lowered cenobamate clearance by 19% and carbamazepine raised it by 15%; phenytoin lowers cenobamate exposure by 27 to 28% (label). There is no co-medication field.

### How the curve compares with healthy volunteers

Vernillet and colleagues (2020) report the ascending-dose studies in healthy adults. These studies are part of the data the model was fitted to (the single-dose study alone supplied two thirds of its concentrations), so the comparison checks consistency rather than validating the model independently:

- **First day of daily dosing (50 to 300 mg):** the model's 24-hour exposure is 0.87 to 1.05 times the observed, so absorption and distribution are right.
- **Steady state (50 to 500 mg/day, day 14 or 17):** the model's exposure is about 0.80 times the observed, its peak 0.74 and its trough 0.76, at every dose. The healthy volunteers' steady-state clearance was about 0.43 L/h, against the model's 0.533.

Other studies in healthy volunteers agree: a median trough of 18.4 mg/L at 200 mg/day (Greene 2022) and mean peaks of 23 mg/L at 200 mg/day and 64 mg/L at 500 mg/day (Darpo 2022). The curve is therefore about 20% low for a healthy adult. It is right for a patient with focal epilepsy on adjunctive therapy, whose other antiseizure drugs raise cenobamate clearance, provided the patient is not African American or Hispanic and takes no drug the model names.

A later sponsor analysis (Krishna and colleagues, AES 2025, 1101 subjects) prints only a clearance of 0.282 L/h and volumes of 38.6 and 6.72 L, without the absorption rate, intercompartmental clearance or covariate equations. Read as typical values, these give a half-life of at least 95 hours and twice the observed concentrations, so they must depend on covariate reference values the abstract does not give. They are not used.

### What the regulators add

The European Medicines Agency's assessment report (Ontozry, 2021) adds four points:

- **Weight.** Exposure is about 25% higher at low weights and 20% lower at high weights than at the mean. The fat-free-mass scaling gives a 50 kg man 16% more exposure than the 70 kg reference and a 110 kg man 16% less, close to the report's figures.
- **Renal impairment.** Exposure was 1.4 to 1.5 times normal in mild and moderate impairment. In severe impairment there was no increase, a result the report calls unexplained. Creatinine clearance was tested in the population model and not kept, so the curve does not read the creatinine field. The label caps renal impairment at 300 mg/day.
- **Hepatic impairment.** Exposure was 1.9 times normal in mild and 2.3 times in moderate hepatic impairment. This is not modelled.
- **Late peaks.** Multiple late peaks suggest enterohepatic recirculation, which is not modelled.

Cenobamate itself changes other drugs' kinetics (it inhibits CYP2C19, raising phenytoin, phenobarbital and N-desmethylclobazam, and induces CYP3A4 and CYP2B6); none of these interactions is modelled.

### Band

None. No therapeutic reference range has been established. In an Italian cohort, patients who responded at 100 mg/day or less had plasma concentrations from 0.5 to 17.6 mg/L, median about 5 to 6 (Operto 2025). There is no effect site.
