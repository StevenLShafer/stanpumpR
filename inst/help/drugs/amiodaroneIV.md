### For the first one to three days only

**This entry is for acute intravenous amiodarone: the first one to three days.** Its parameters come from patients sampled during short-term treatment, and its terminal half-life, 42 hours, is far shorter than the weeks amiodarone takes to leave the body after longer observation. Do not use it to predict accumulation over longer courses: it reaches a steady state in days that the real drug approaches only over weeks to months. For oral and long-term therapy use [long-term oral amiodarone](help:drugs/amiodarone), a separate entry with its own model.

### The model

The kinetics are from Korth-Bradley and colleagues (*J Clin Pharmacol* 1996;36:715-719), a population analysis of 245 patients given intravenous amiodarone for the short-term treatment of refractory, hemodynamically destabilising ventricular tachycardia or fibrillation. Two compartments described the data, with proportional models for the differences between patients and an additive-proportional residual error. The parameters are per kilogram:

| Parameter | Per kilogram | At 70 kg | Precision of the estimate (CV) |
|---|---|---|---|
| Clearance (CL1) | 0.22 L/h/kg | 15.4 L/h (0.257 L/min) | 13% |
| Central volume (V1) | 0.30 L/kg | 21 L | 11% |
| Peripheral volume (V2) | 10.0 L/kg | 700 L | 9.5% |
| Intercompartmental clearance (CL2) | 0.71 L/h/kg | 49.7 L/h (0.828 L/min) | 16% |

The percentages are the precision of each estimate, not the variability between patients. That is reported separately, as variances of 1.52 for clearance, 0.37 for each volume and 0.44 for the intercompartmental clearance; read as variances of proportional random effects, those are coefficients of variation of roughly 120%, 60%, 60% and 65%, so individual patients differ widely from the typical curve shown. At 70 kg the half-lives are **13.2 minutes** (distribution into the peripheral volume) and **42.0 hours**. The authors concluded that the parameters were similar to those of healthy volunteers, and added that estimates made during short periods of observation may not agree with those from prolonged observation.

### Why a separate entry from long-term amiodarone

One drug in stanpumpR runs one pharmacokinetic model, and [amiodarone](help:drugs/amiodarone) runs Pollak, Bouillon and Shafer's model of long-term oral therapy. Its parameters are apparent, divided by the unmeasured oral bioavailability *F*, and no choice of *F* turns them into these. Korth-Bradley's clearance at 70 kg is 0.22 × 70 = 15.4 L/h, or 369.6 L/day. The oral model's true clearance is 229 L/day × *F*, which is at most 229 L/day for any *F* up to 1. The central volumes disagree as well: 21 L here against 882 L × *F*. The oral paper itself calls its model ill-suited to single doses or short intravenous infusions. So intravenous amiodarone is this separate drug, and its concentrations are not added to the long-term entry's: a patient given both appears on two rows, each with its own model.

### No metabolite

Desethylamiodarone is not formed in this model. Korth-Bradley did not model it, and the long-term entry's metabolite parameters are on the oral model's apparent scale, so they cannot be attached to these. In the window this model covers it matters little: Vadiei and colleagues (*J Clin Pharmacol* 1996;36:720-727), who gave a single 15-minute infusion of 5 mg/kg, found desethylamiodarone formation slow and its concentration low relative to amiodarone, so that it is unlikely to contribute significantly to the effect during intravenous therapy of up to two weeks.

### No effect site, and no band

No human equilibration rate for the antiarrhythmic effect has been published, so the model has no effect site and the plot shows the plasma concentration. The QT and antiarrhythmic responses are not modelled.

No shaded band is drawn. The familiar therapeutic window of 1.0 to 2.5 mg/L, which the long-term entry draws, is a range for **trough concentrations in chronic therapy**, when serum is in equilibrium with the tissues. During intravenous loading it is not, and the window says little about effect. For reference, the product label's first-day regimen (150 mg over 10 minutes, 1 mg/min for 6 hours, then 0.5 mg/min) gives a peak of 5.6 mg/L at the end of the rapid load and then, from the first hour to the 24th, 0.87 to 1.38 mg/L in this model: at the bottom edge of the chronic window, and below 1.0 mg/L from 6.5 to 15.6 hours. There is no default recovery threshold; one set under Settings → Drug Thresholds is timed on the plasma. See [the label's 24-hour loading regimen](scenario:amiodarone-iv-loading).

### Covariates and body size

Weight only, through the per-kilogram parameters. Age, gender, height, serum creatinine, serum alkaline phosphatase, ejection fraction and the response to treatment did not contribute to the variability. The model is therefore scaled like the other per-kilogram models (ketamine, etomidate): by default the 70 kg values above are the reference man's and are [scaled to fat-free mass](help:models/fat-free-mass); with the switch off, every volume and clearance is multiplied by weight / 70, which is the published per-kilogram model exactly. The abstract does not give the patients' weights, heights or ages, so the child and infant in the table above are extrapolation.

### Cross-checks against published observations

These are the model's typical-patient values, computed for the 70 kg reference man (with the fat-free-mass switch off, the same for any weight when the dose is per kilogram), set against what the papers report.

- **Watt and colleagues** (*Br J Clin Pharmacol* 1986;21:525-528) infused 175 mg/h for 2 hours and then 50 mg/h for 46 hours in eight patients, and found that in at least three of them the plasma concentration was below 1.0 mg/L from 3 to 16 hours. This model's typical patient on that regimen is at 1.10 to 1.48 mg/L over those hours, just above 1.0, so a patient with a larger volume or clearance than typical would fall below it.
- **Shiga and colleagues** (*Heart Vessels* 2011;26:274-281) gave healthy Japanese men single 15-minute infusions of 1.25, 2.5 and 5.0 mg/kg. The mean peak concentrations were 2.92 ± 0.61, 7.14 ± 1.48 and 13.66 ± 3.41 mg/L; this model gives 2.90, 5.81 and 11.62 mg/L at the end of the infusion, within the reported spread at each dose. Their areas under the curve to 96 hours were 3.6 ± 0.7, 8.1 ± 1.6 and 16.6 ± 4.3 mg·h/L; the model's are 15 to 33% higher (4.78, 9.56 and 19.12). They also found a serum half-life of more than 14 days, which is the long tail this model does not have.

### Where to be careful

- **Acute use only**, as above. Beyond about three days the curve still rises towards the model's steady state, the infusion rate divided by 15.4 L/h (1.95 mg/L at 0.5 mg/min), and reaches 90% of it within five days; the real drug goes on accumulating for weeks.
- **The curve is a typical patient's.** Between-patient variability in these parameters is large.
- **The patients were critically ill**, with refractory ventricular arrhythmias. The abstract reports parameters similar to those of healthy volunteers.
- **Doses are entered as labelled.** No salt or molecular-weight conversion is applied.
