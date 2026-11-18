### The model

The sponsor's population model, published by Vashi and colleagues (*Epilepsy Res* 2023;195:107185) and fitted to 960 healthy volunteers and adults with focal seizures, most of them taking 200 mg a day. The parameters are from the paper's supplement, and the FDA's clinical pharmacology review of the new drug application (NDA 212839, 2019) prints the same table: two compartments, apparent oral, with an absorption lag of 0.24 hours, absorption at 2.74/h, a clearance of 0.533 L/h, central and peripheral volumes of 42.4 and 7.27 L, and an intercompartmental clearance of 1.84 L/h.

The terminal half-life is about 65 hours (the label gives 50 to 60). At 200 mg once daily the steady state averages 15.6 mg/L, close to the phase 3 mean of 15.5 mg/L. Steady state takes about two weeks, which is why the label titration (12.5 mg/day, then 25, 50, 100, 150 and 200 mg/day, each for two weeks) is worth simulating as a whole.

### What is not applied

Each continuous covariate enters linearly about its median in the analysis population. Neither the paper nor the FDA or EMA assessment reports print the medians, so none of these terms can be evaluated exactly:

- **Daily dose on clearance.** If the term is a power of the daily dose, which is the likely reading, it changes clearance by less than 4% between 100 and 400 mg/day. That agrees with the multiple-dose studies: at steady state the apparent clearance was 0.40 to 0.52 L/h at every daily dose from 50 to 500 mg, with no trend (Vernillet 2020). After a *single* dose cenobamate is non-linear: the apparent clearance fell from about 1.3 to 1.4 L/h at 5 to 10 mg to 0.40 to 0.48 L/h at 400 to 750 mg. The model does not reproduce that. It overpredicts the total exposure to one small dose (2.2-fold at 5 mg, 1.2-fold at 100 mg) and underpredicts it after one very large dose (0.73 at 750 mg). It does reproduce the first day of daily dosing.
- **Body size.** The source puts body surface area on clearance and weight on the central volume. Both are replaced by the library's [fat-free-mass scaling](help:models/fat-free-mass), and with the switch off the published values are used for everyone. Centred on the 70 kg reference man, the two agree within 5% for men between 60 and 90 kg. They differ for women, for whom the source had no sex term, and in obesity.
- **Ethnicity and bilirubin.** Clearance was 14.7% lower in African American subjects and fell with bilirubin. Hispanic ethnicity changed a parameter by 13.3%; the paper's tables disagree on whether it was the clearance or the central volume. The Patient Profile has no fields for these.
- **Other antiseizure drugs.** Clobazam lowered cenobamate clearance by 19% and carbamazepine raised it by 15%; phenytoin lowers cenobamate exposure by 27 to 28% (label). Of these, only carbamazepine's 15 per cent is applied, and only when the **Include antiseizure drug interactions** box in Graph Options is ticked (see [Antiseizure drug interactions](help:models/antiseizure-interactions)); clobazam is not in the library and phenytoin's effect on cenobamate is not modelled.

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
- **Hepatic impairment.** Exposure was 1.9 times normal in mild and 2.3 times in moderate hepatic impairment. The FDA label (revised August 2025) adds a later study in severe (Child-Pugh C) impairment: 4.2 times normal after a single 100 mg dose. Both agencies cap mild and moderate impairment at 200 mg/day and advise against use in severe impairment. None of this is modelled.
- **Late peaks.** Multiple late peaks suggest enterohepatic recirculation, which is not modelled.

Cenobamate itself changes other drugs' kinetics (it inhibits CYP2C19, raising phenytoin, phenobarbital and N-desmethylclobazam, and induces CYP3A4 and CYP2B6); none of these interactions is modelled.

### Patients in clinics

Two therapeutic drug monitoring series in adults with refractory epilepsy, neither part of the model's data, give the concentration per mg/day of dose. The model's steady-state average is 0.078 mg/L per mg/day at every dose.

- **Kork, Germany** (Steinhoff and colleagues 2024, 101 patients on 100 to 400 mg/day): seizure-free patients had 17.8 mg/L at a mean of 240 mg/day (0.074), and non-responders had 20.9 mg/L at 261 mg/day (0.080). Concentration was linear in dose. Morning samples were within 10% of levels drawn at other times of day.
- **Oslo, Norway** (Johannessen Landmark and colleagues 2026, 258 patients, morning samples before the day's doses): 53.7 µmol/L, or 14.4 mg/L, at a mean of 164 mg/day (0.088). Averaging each patient's own ratio gives 0.091 for patients taking neither a strong enzyme inducer nor an inhibitor, 0.083 with carbamazepine, phenytoin or phenobarbital, and 0.103 with valproate or cannabidiol. A patient's own ratio varied by 18% from sample to sample, and patients differed 34-fold.

The model is within 12% of the Kork figures and Oslo's overall figure. It is 14% below Oslo's patients on neither an inducer nor an inhibitor and 6% below those on an inducer. The 24% gap for valproate or cannabidiol reflects an inhibition the model has no term for.

### Band

10 to 25 mg/L at trough, with 18 as the typical value (its midpoint, not a target): the range newly introduced by the AGNP consensus guidelines' 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5, https://doi.org/10.1055/a-2860-7861; laboratory alert level 50 mg/L; TDM only "potentially useful", level 4), taken from the mean ± SD in 36 patients who were seizure-free at a median 200 mg a day. At 200 mg a day this model averages 15.6 mg/L, inside it. Its troughs at 100 and 400 mg a day, 6.8 and 27.4 mg/L, fall outside it, and in the trials below the central 90% of seizure-free patients on 100 mg a day, 5.1 to 9.7 mg/L, lies entirely below it: the consensus range describes the usual 200 mg dose.

Before the 2026 table, the band was 5 to 35 mg/L, from the sponsor's analysis of steady-state trough concentrations in the two controlled trials (Greene and Kamin, AAN 2021). In 95% of the patients whose seizures fell by half or more, or stopped, troughs were between 5 and 35 mg/L, which the authors propose as "a trough plasma concentration reference range (not a target therapeutic range)". The central 90% of those responders were 4.6 to 9.3 mg/L at 100 mg/day, 11.3 to 18.7 at 200 and 20.8 to 35.2 at 400. The model's steady-state troughs, 6.8, 13.7 and 27.4 mg/L, fall inside each range, near its middle. In the phase 3 safety study the mean trough on 200 mg/day was 15.1 mg/L, 15.2 in the patients whose seizures fell by half and 14.5 in those seizure-free for a year or more (AAN 2022), against the model's 13.7.

Both are ranges in which patients respond rather than therapeutic ranges. In the Kork series, seizure-free patients (3.8 to 33.7 mg/L) and non-responders (3.9 to 54.6 mg/L) did not differ, and adverse effects did not follow the concentration. The authors concluded that no general therapeutic range can be defined and recommended recording each patient's own concentration when they do well. In an Italian cohort, patients who responded at 100 mg/day or less had 0.5 to 17.6 mg/L, median about 5 to 6 (Operto 2025). The Oslo laboratory reports against 15 to 130 µmol/L (4.0 to 34.8 mg/L), taken from the Kork series, and 85% of its measurements fell inside it.

### No effect site

The one study that timed an effect (Kasteleijn-Nolst Trenité and colleagues 2019) measured the EEG response to flashing light in 6 patients with photosensitive epilepsy after single doses of 100 to 400 mg. The response was already reduced 1 hour after 250 and 400 mg, before the plasma peak, and was still reduced more than 24 hours later. No delay between plasma and effect can be seen on that time scale, so no ke0 can be estimated. With a half-life of 65 hours and daily dosing, an equilibration time of minutes would not change the curve anyway. The plot shows plasma only.
