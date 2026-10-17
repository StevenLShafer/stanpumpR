### The model

Fediuk and colleagues' population model re-estimated on an expanded data set (*Clin Pharmacol Drug Dev* 2021, data set 1): two compartments with first-order absorption at 0.329/h after a 0.226 h lag (fasted); apparent clearance 11.9 L/h times weight over 85 kg to the power 0.75 times MDRD eGFR over 90 to the power 0.458, 4% lower in women; central volume 6.51 L (46% higher in women) and peripheral volume 107 L in proportion to weight; intercompartmental clearance 7.76 L/h. The eGFR is the four-variable MDRD from the entered serum creatinine (a child's on the adult scale) or an [assumed normal](help:models/covariates) one, capped at 120 as in the source; the weight is the pharmacokinetic weight. The terminal half-life is about 15 hours (the paper: 15.3 hours in healthy subjects).

### What is applied and what is not

The functional forms are those the table's labels name; the equations are in the supplement. Type 2 diabetes (8% lower clearance), East and South-East Asian race, the age effect on the central volume and food (a 27% slower absorption and 7% lower bioavailability) are not applied: the curve is a healthy, fasted patient's.

### What is not plotted

Concentration only, in ng/mL. Urinary glucose excretion is an endpoint of its own. See [Antidiabetic models and their limits](help:models/antidiabetics).
