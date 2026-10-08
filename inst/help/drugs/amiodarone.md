### The model

Amiodarone's kinetics are from Pollak, Bouillon and Shafer (*Clin Pharmacol Ther* 2000;67:642-652), a population analysis of long-term oral therapy: 77 patients (50 men, 27 women) followed in a Nova Scotia clinic for a mean of two years and as long as seven, with 605 trough serum samples drawn before the morning dose and assayed for both amiodarone and desethylamiodarone. Two compartments described the data (three did not improve the fit), with these typical values and between-patient variability:

| Parameter | Typical value | Variability (CV) |
|---|---|---|
| V1/F | 882 L | not estimable |
| V2/F | 12,700 L | 58% |
| CL1/F | 229 L/day | 31% |
| CL2/F | 588 L/day | 56% |

The abstract prints CL2/F as 599 L/day, which is a misprint; Table II's 588 is the estimate (the difference is small in any case: terminal half-life 55.4 days against 55.1). The rapid half-life is 17.3 hours and the terminal half-life 55.4 days, the product of a low clearance and an enormous peripheral volume, which reflects amiodarone's lipophilicity. The drug file converts the clearances to litres per minute, the library's unit.

### Oral only, as a constant daily rate

No patient received intravenous amiodarone, so every parameter is **apparent**, divided by the unmeasured bioavailability. Apparent parameters predict oral concentrations correctly, because the unknown bioavailability cancels, but would predict intravenous concentrations wrong by a factor of one over it. The paper adds that the scarcity of early data (11 samples in the first week, 64 in the first month) may account for the large and poorly determined V1, making the model ill-suited to single doses or short intravenous infusions. Acute intravenous amiodarone has its own population kinetics (Korth-Bradley and colleagues, *J Clin Pharmacol* 1996;36:715-719), which cannot be reached from these parameters by any choice of bioavailability. Amiodarone is therefore offered for oral long-term therapy only.

Pollak modelled each day's dose as a **constant-rate input over 24 hours** (400 mg/day as 16.7 mg/h), because no patient was sampled twice within a dosing interval, the half-life is sixty times the dosing interval, and a day's dose is small against what is already in the body. The unit **mg/day PO** is that input: enter the daily dose, and it is spread evenly over each day from the row's time until the next amiodarone row, which replaces it; a row of **0 mg/day PO** stops it. There is no first-order oral unit, because no absorption rate was identified. Enter the dose as labelled (mg of amiodarone hydrochloride, as the patients took it); no salt conversion is applied, since the apparent parameters absorb it.

### Active metabolite

Desethylamiodarone appears to be as potent as amiodarone and as toxic (the paper cites Nattel and Talajic, *Drugs* 1988;36:121-131, for potency). Pollak took the input to the metabolite's model to be all the amiodarone permanently cleared from the serum, so in this program every milligram of amiodarone eliminated becomes a milligram of [desethylamiodarone](help:drugs/desethylamiodarone), formed from the central compartment at amiodarone's own elimination rate constant. The metabolite's curve appears on its own row. See [Active metabolites](help:models/metabolites).

Amiodarone is **not a prodrug**. Neither it nor its metabolite has an effect site in the model, because no human equilibration rate for the antiarrhythmic effect has been published, so both rows plot serum concentrations, which is what the therapeutic window refers to.

### Covariates

None. Age, sex, height, weight and lean body mass were tested against every parameter and none was significant. The parameters therefore take the library's default [fat-free-mass scaling](help:models/fat-free-mass) for fixed published values, and are used exactly as published with the switch off. No re-anchoring was needed: the cohort's fat-free mass, about 54 kg at the mean weight, height and age of each sex, is about 1% below the 54.5 kg reference man's. The patients were adults of 22 to 86 years and 39 to 133 kg, so the child and infant in the table above are extrapolation.

### Typical concentrations and regimens

The shaded band is the 1.0 to 2.5 mg/L therapeutic window of the product monograph, which applies to **amiodarone**, not to the metabolite; the typical value is Pollak's target, 1.5 mg/L. Steady state is the daily dose over 229 L/day, so the maintenance dose for 1.5 mg/L is 343 mg/day, which can be approximated by 400 mg/day on six days of seven.

Pollak's proposed regimen reaches the window within a day and stays in it:

| Days | Dose |
|---|---|
| 0 to 2 | 1600 mg/day |
| 2 to 7 | 1200 mg/day |
| 7 to 14 | 1000 mg/day |
| 14 to 21 | 800 mg/day |
| 21 to 28 | 600 mg/day |
| 28 to 90 | 400 mg/day |
| from 90 | 343 mg/day |

In this model it gives 1.19 mg/L at one day, 1.72 at a week, 1.54 at four weeks and 1.50 at a year, with desethylamiodarone rising slowly from 0.56 mg/L at a week to 1.34 at a year. The label regimens the paper compared it with are a maximum of 1600 mg/day for three weeks, 800 mg/day for a month, then 600 mg/day, and a minimum of 800 mg/day for a week, 600 mg/day for a month, then 400 mg/day. The highest leaves half the population above the window at steady state (the typical patient at 600/229 = 2.6 mg/L), and the lowest needs six to nine months to approach its steady state of 400/229 = 1.75 mg/L.

### Stopping

Because the peripheral compartment is so large, how fast the concentration falls after stopping depends on how long the drug was given. At steady state this model gives a fall of 25% in 2.2 days, 50% in 31 days and 75% in 87 days. The paper's text and its Figure 6 give 3, 36 and 98 days; the parameters in its own Table II give the shorter times, and the difference is unexplained. Either way, a two- or three-day interruption lowers the concentration noticeably, but halving it takes weeks. The recovery threshold is 1.0 mg/L, the bottom of the window, so the *time until threshold* line shows how long the serum amiodarone would take to fall out of the window if dosing stopped. It is timed on the serum concentration; see [Time until threshold](help:models/recovery).

### Where to be careful

- **There is no effect-site model.** Serum is not where amiodarone acts, and during loading it is not yet in equilibrium with the tissues, so a serum concentration early in therapy says little about effect. The QT and antiarrhythmic responses are not modelled.
- **Early distribution is under-resolved.** With so few samples in the first month, the first days of the curve rest on a poorly determined V1; richer early data could add a third compartment, sharpening the early curve without changing the long-term one.
- **Bioavailability varies between patients** (30% to 80%, and better with food, which is how the patients took it), and between-patient variability in the parameters is 30% to 108%. The curve is a typical patient's; the paper predicted that its regimen would keep 90% of patients within the window at steady state.
- **The population was overwhelmingly of Northern European descent**, and may not represent others.
- The metabolite's parameters are apparent and rest on the assumption that all of the amiodarone cleared becomes desethylamiodarone; see its page.
