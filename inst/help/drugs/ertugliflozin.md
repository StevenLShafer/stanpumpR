### The model

Fediuk and colleagues' population model re-estimated on an expanded data set (*Clin Pharmacol Drug Dev* 2021, data set 1): two compartments with first-order absorption at 0.329/h after a 0.226 h lag (fasted); apparent clearance 11.9 L/h times weight over 85 kg to the power 0.75 times MDRD eGFR over 90 to the power 0.458, 8% lower in type 2 diabetes and 4% lower in women; central volume 6.51 L in proportion to weight, times age over 65 to the power −0.192, and 46% higher in women; peripheral volume 107 L in proportion to weight; intercompartmental clearance 7.76 L/h. The eGFR is the four-variable MDRD from the entered serum creatinine (a child's on the adult scale) or an [assumed normal](help:models/covariates) one, capped at 120 as in the source; the weight is the pharmacokinetic weight. The terminal half-life is about 16 hours (the 2020 paper: 16.6 hours in patients with type 2 diabetes and normal renal function).

### What is applied and what is not

The equations are the supplement's: powers of each continuous covariate over its reference (85 kg, eGFR 90, 65 years) and a factor for each category. The app has no diabetes field, and ertugliflozin is given only in type 2 diabetes, so the diabetes effect is applied. East and South-East Asian race (17% higher clearance, a central volume 2.5 times larger) and food (a 27% slower absorption and 7% lower bioavailability) are not: the curve is a fasted patient's. Below 18 years the age term is held at 18, the source having studied adults.

### What is not plotted

Concentration only, in ng/mL. Urinary glucose excretion is an endpoint of its own. See [Antidiabetic models and their limits](help:models/antidiabetics).
