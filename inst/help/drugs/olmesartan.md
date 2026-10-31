### The model

Olmesartan's parameters come from Yoshihara, Gao, Shiga, Wada and Hisaoka (*Clin Pharmacokinet* 2005;44:1329–42), a NONMEM analysis of 7911 plasma concentrations from 472 people across twelve trials — 89 healthy volunteers and 383 hypertensive patients, 355 Western and 117 Japanese, given single and multiple oral doses of olmesartan medoxomil from 2.5 to 80 mg. Two compartments with first-order absorption and a 26-minute absorption lag.

Every parameter is **apparent**, divided by the oral bioavailability, because the analysis had no intravenous arm. So no intravenous unit is offered, and the dose enters without being multiplied by a bioavailability.

### The dose is the prodrug, and the paper says which

Olmesartan medoxomil is an ester prodrug, hydrolysed completely during absorption; what is plotted is olmesartan, about 20% lighter. A model fitted to olmesartan concentrations could have been given its doses on either basis, and guessing wrong is a flat 20% error in every curve — so this is worth stating explicitly rather than assuming.

This paper settles it in its own words: the observed data were *"normalised to a dose of olmesartan medoxomil 20mg"*, and the dose range it quotes throughout is of the medoxomil. The apparent parameters therefore already carry the molecular-weight ratio along with the bioavailability, and **a dose is entered exactly as prescribed** — 20 mg means 20 mg of Benicar.

### Covariates scale the curve without changing its shape

This is the unusual feature of the model, and it follows from how the source chose to parameterise it. Yoshihara fitted apparent clearance together with the four rate constants, so the central volume was never a parameter at all — it is clearance divided by the elimination rate constant:

> V<sub>1</sub>/F = (CL/F) ÷ k<sub>e</sub>

and **every covariate sits on clearance**. A patient with half the clearance therefore also has half the central volume and the same elimination rate constant, so the eigenvalues of the disposition are identical for everybody and the whole covariate model acts as a single multiplier on concentration. Nothing about the shape of the curve — not the time to peak, not either half-life — moves with age, weight, sex, renal function or disease. Only the height does, inversely with clearance.

That is a property of the published model rather than a simplification made here, and it is worth knowing when reading two patients' curves side by side: they are the same curve at different scales.

### The covariate equation

> CL/F = 7.32 × (age ÷ 50)<sup>−0.117</sup> × (weight ÷ 70)<sup>0.310</sup> × (creatinine ÷ 80)<sup>−0.344</sup> × e<sup>0.145 if male</sup> × e<sup>−0.239 if hypertensive</sup>

with creatinine in micromoles per litre. The [Patient Profile's](help:patient-profile) field is in milligrams per decilitre and is converted; the reference of 80 µmol/L is 0.9 mg/dL. A blank creatinine means the assumed normal value for the patient's age and sex, and because the equation was fitted in adults a child's creatinine is first put on the adult scale.

**Serum creatinine is much the strongest term.** The source reports clearance 33.8% to 44.8% lower across a creatinine of 265 to 450 µmol/L (roughly 3 to 5 mg/dL). Age is much the weakest: a 75-year-old clears 4.6% less than a 50-year-old. The age term is a negative power, infinite at age zero, so the age entering it is floored at 18 years as it is for every such term in the library. This is an adult model — its participants were 18 to 87.

### Patient status is applied, as hypertensive

Hypertensive patients clear olmesartan **21.3% more slowly** than healthy volunteers, and the source fitted that as a covariate. There is no patient-status field in the Patient Profile and there does not need to be one: a patient receiving an angiotensin receptor blocker is a hypertensive patient. So the term is applied as hypertensive, which is also what makes the clearance here match the paper's own headline typical values of 6.66 L/h in a man and 5.76 L/h in a woman.

### Race is not applied, and that is the paper's own finding

The source also fitted a race term, Japanese against Western, and it is **not applied here**: there is no race field in this library and none will be invented. The cost is bounded and very small. The term was not statistically significant, its coefficient had a 131% coefficient of variation and a bootstrap confidence interval straddling zero, and the difference it describes is 3.0%, Japanese clearance being the lower.

It is worth being clear about why the authors kept a non-significant term in their final model: the paper's declared purpose was to look for a race effect in olmesartan's disposition, and the answer was that there is none. **That absence is the finding**, not the coefficient, and leaving the coefficient out preserves it.

### How the curve compares with the label

The source fitted concentrations, not exposure summaries, so an independent check is worth having. At the reference patient a 20 mg dose gives a peak of 402 ng/mL at 1.9 hours and an area under the curve of 3000 ng·h/mL, with the slow eigenvalue corresponding to a half-life of 12.6 hours. The olmesartan medoxomil label puts the peak at 1 to 2 hours and the terminal half-life at about 13 hours, and the apparent volume of 61 L at steady state, multiplied by the label's bioavailability of roughly 26%, gives about 16 L against the label's 17 L. Those agreements are with numbers the model was not fitted to.

### Effect site and blood pressure

Neither is modelled. The source fitted no pharmacodynamic model of any kind, and [no antihypertensive in this library plots a blood pressure](help:models/antihypertensives). No band is drawn: there is no established therapeutic plasma range for olmesartan.

### Where to be careful

The absorption rate constant had by far the largest interindividual variability in the model — a variance of 1.64, against 0.116 for clearance — so the first two hours after a dose are the least reliable part of any individual curve, even though the typical shape is well determined by 7911 observations.

The physiological rise in filtration of pregnancy is deliberately not applied to the creatinine term here, because olmesartan is contraindicated in pregnancy and the rise would draw a faster, healthier-looking curve for a patient who should not be taking the drug.
