### The model

Tocilizumab (Actemra) is a humanised monoclonal antibody against the interleukin-6 receptor. Moes and colleagues in Leiden (*Clin Pharmacokinet* 2022;61:231-247) studied 29 adults in the intensive care unit with COVID-19. Most (83%) were mechanically ventilated, and all were on dexamethasone. Each received one intravenous dose of 8 mg/kg, up to 800 mg, and had plasma sampled to day 20.

The model has one compartment with two elimination pathways working side by side:

- a linear clearance of 0.725 L/day;
- a saturable, target-mediated pathway through the IL-6 receptor, with a capacity (Vmax) of 4.19 mg/day and a half-saturation concentration (Km) of 0.22 mcg/mL.

The volume is 4.34 L. No covariate was kept. Above about 5.6 mcg/mL the linear clearance carries most of the elimination, and the curve falls with a half-life of about 4 days. Below that the receptor pathway takes over, and well below Km the curve falls with a half-life of under 4 hours. So the end of the curve is a cliff rather than a long tail, as the paper's Figure 2 draws it.

The paper's abstract gives Vmax in micrograms a day and Km in micrograms per mL. Its Table 3 and Results give milligrams. The milligrams are right: only they reproduce the paper's own curves.

Elimination saturates, so doses do not simply add. The curve comes from numerical integration over the whole dose table, the engine that runs [phenytoin](help:drugs/phenytoin) and [evolocumab](help:drugs/evolocumab). The test checks it against an independent solution of the paper's equations: the two agree to better than 1 part in 100,000 of the peak.

### Who it describes

The model describes critically ill adults with COVID-19, and stanpumpR uses it for every patient because it is the only complete model in the archive. Two others are waiting:

- **Jamois 2025** (cytokine release syndrome after bispecific antibodies) prints its parameters only in a supplement not yet obtained.
- **Frey 2010** (1,793 adults with rheumatoid arthritis) prints its parameters to only one or two figures.

Either may join as an alternative for its own population. Clearance in rheumatoid arthritis is lower: the label gives 12.5 mL/h, 0.30 L/day, against 0.725 here.

Children are not in the model. The label's paediatric volumes are about half the adults', which the body-size scaling carries only in part.

### Covariates

Moes tested weight, BMI, body surface area, height, sex, age, renal and liver function, albumin and C-reactive protein, and kept none. From this the authors argue for a fixed dose.

stanpumpR scales every parameter by body size all the same, as it does for every model, because failing to reach statistical significance in a small study is not evidence that size does not matter. With [the scaling switch](help:models/fat-free-mass) on, the volume follows fat-free mass, and the linear clearance and the saturable capacity follow fat-free mass to the power 0.75. Both are anchored at Moes's cohort (21 men and 8 women, mean 96.4 kg, 177 cm and 64 years), so a patient like them receives the published values. Km is a concentration and is not scaled. With the switch off, the published values are used whatever the patient.

### Doses

Actemra for COVID-19 is a single 60-minute intravenous infusion of 8 mg/kg (12 mg/kg below 30 kg), and no more than 800 mg per infusion. If the patient does not improve, one more infusion may be given at least 8 hours after the first. Enter it as a 1-hour infusion: 8 `mg/kg/hr` at time 0 and 0 at 60 minutes. A fixed dose, such as the RECOVERY trial's weight-tiered 400, 600 or 800 mg, which the label says gives similar exposure, is entered in `mg/hr` the same way.

Only the intravenous route is offered. Moes gave no subcutaneous doses, and the label does not approve the subcutaneous product for COVID-19.

### How well it agrees

At Moes's mean dose, 781 mg in a patient like the cohort's:

| | Measured or published | stanpumpR |
|---|---|---|
| AUC after the first dose | 938 ± 190 mcg/mL·day (Moes) | 958 mcg/mL·day |
| Peak after 8 mg/kg | median 151 (77.5-319) mcg/mL (label, 380 COVID-19 patients) | 179 mcg/mL |
| Below 1 mcg/mL | about days 13 to 22 (Moes, Figure 2) | day 20 |
| Linear clearance, ventilated patients | 29 mL/h, 0.70 L/day (label) | 0.725 L/day |
| Day 28 after 8 mg/kg | median 0.229 (0.00119-19.4) mcg/mL (label) | below 0.001 mcg/mL |

The label's COVID-19 model, fitted to 380 patients, agrees with this one on clearance, the one parameter that measures a physiological process. It has two compartments, though, and its concentrations fall below quantification only "after 35 days on average". Moes sampled to day 20 and could fit only one compartment. So this curve's tail is shorter than the label's: by day 28 it is essentially zero, where the label's median is 0.23 mcg/mL.

### No band

No therapeutic range is established for tocilizumab, and the label gives no concentration target. Moes used 1 mcg/mL as the concentration that saturates the receptor, a threshold taken from rheumatoid arthritis, and every patient stayed above it for at least 15 days. That is a pharmacological landmark, not a therapeutic range, so it is not drawn.

### What is not plotted

Concentration only, in mcg/mL. Neither C-reactive protein, the soluble IL-6 receptor nor any clinical response is drawn. Time until threshold is blank, as for every drug run by the numerical engine.
