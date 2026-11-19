### The model

Lisinopril's parameters come from Thomson, Kelly and Whiting (*Br J Clin Pharmacol* 1989;27:57–65), a NONMEM analysis of 381 concentrations from 60 hypertensive patients — 40 of them over 65, and 20 with renal disease — pooled from two multicentre trials. Steady-state profiles were drawn after at least two weeks on a constant daily dose of 2.5 to 40 mg. One compartment with first-order absorption: apparent clearance 0.251 L/h per kilogram at the reference patient, apparent volume 36.7 L, absorption rate constant 0.104 per hour.

Every parameter is **apparent**, divided by the oral bioavailability, because the analysis had no intravenous arm; the paper puts lisinopril's bioavailability at about 29% from earlier work. So the dose enters without being multiplied by a bioavailability, and **no intravenous unit is offered**.

### Absorption is slower than elimination, so the tail is the gut

This is the unusual thing about lisinopril, and it changes how the curve should be read. At the reference patient the elimination rate constant is 0.479 per hour and the absorption rate constant is 0.104 per hour. **Absorption is more than four times slower**, which makes the drug *flip-flop*: the terminal slope of the plotted curve is the absorption rate constant, not the elimination one.

The practical consequence is that the apparent half-life of roughly 6.7 hours you could measure off the tail of this curve is an **absorption** half-life. It tells you how fast the drug is leaving the gut, not how fast the kidney is clearing it. Renal impairment still raises the whole curve, because clearance sets the area under it — but it does not change that terminal slope, which is why a half-life read off the plot will look stubbornly unchanged in a patient whose clearance has fallen sixfold.

That slow absorption is not a quirk of one fit. Sandra and colleagues (*Br J Clin Pharmacol* 2023;90:504–515) fitted lisinopril in 13 hypertensive children and obtained 0.075 per hour — a different population, different authors, different decade, same order of magnitude.

### Covariates

**Serum creatinine, age and weight**, all on clearance:

> CL/F = 0.251 × weight × (creatinine ÷ 70)<sup>−0.887</sup> × (age ÷ 65)<sup>−0.451</sup>

with creatinine in micromoles per litre. The [Patient Profile's](help:patient-profile) field is in milligrams per decilitre and is converted, so its reference of 70 µmol/L is 0.79 mg/dL. A blank creatinine means the assumed normal value for the patient's age and sex. The equation was fitted in adults, so a child's creatinine is put on the adult scale before it enters.

The age term is a **negative power**, which is infinite at age zero, so the age entering it is floored at 18 years exactly as it is for every other such term in the library. This is an adult model; its youngest patient was 21.

Volume carries no covariate at all. The authors tested weight and body surface area on it and neither helped, which they attribute to the very high interindividual variability in volume — a coefficient of variation above 100% under every model they tried.

### Cardiac failure: measured, and not applied

The source found that cardiac failure multiplied clearance by **0.645**, a 35% reduction, and it is **not applied here** for a simple reason: the Patient Profile has no cardiac failure field, and inferring one from another covariate would be a guess. The coefficient is recorded in the antihypertensive parameter audit so that it is ready if such a field is ever added.

The authors are careful about what it means. Every patient with cardiac failure in their data was also taking a cardiac glycoside, so the two are confounded; when they tested the glycoside on its own the likelihood difference was smaller than for cardiac failure, which they read as the disease rather than the drug being responsible. They also note that a comparable reduction had been measured independently, in elderly patients with chronic cardiac failure.

### Effect site and blood pressure

Neither is modelled. The source fitted no pharmacodynamic model of any kind, and [no antihypertensive in this library plots a blood pressure](help:models/antihypertensives). No band is drawn: there is no established therapeutic plasma range for lisinopril.

### Where to be careful

The data are **steady-state profiles on chronic dosing**, not single-dose curves, and the absorption rate constant is the least well determined thing in such a design — sampling began at the trough and the earliest point in the profile was at one hour. Treat the shape of the first few hours after a dose with more caution than the area under the curve.

Lisinopril also binds angiotensin-converting enzyme saturably, which in other studies produces a prolonged terminal phase at low concentrations that a one-compartment linear model cannot represent. Thomson's design — steady state, sampling over one dosing interval — does not reach into that phase, so the model is a fair description of what happens across a day of dosing and not of what happens over the week after the last dose ever taken.

Finally, the physiological rise in filtration of pregnancy is deliberately not applied to the creatinine term here, because lisinopril is contraindicated in pregnancy and the rise would draw a faster, healthier-looking curve for a patient who should not be taking the drug.
