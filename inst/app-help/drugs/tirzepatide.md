### The model

Schneck and Urva's population analysis of 19 studies and 5802 participants (*CPT Pharmacometrics Syst Pharmacol* 2024), with an intravenous study among them, so the parameters are on the true scale: two compartments, first-order absorption 0.0373/h, clearance 0.0329 L/h and intercompartmental clearance 0.126 L/h per 70 kg, scaled by weight to the power 0.8, and volumes 2.47 and 3.98 L scaled by fat-free mass plus 48% of fat mass, over 70. The model carries its own size covariates, so the fat-free-mass switch does not change it.

### Bioavailability

The paper fixes the absolute bioavailability at 0.8 and estimates a relative change of −0.181 between studies. Its supplement's NONMEM code shows that the change applies to every study except the phase 1 bioavailability study, the type 2 diabetes trials included. So the bioavailability here is 0.8 × (1 − 0.181) = 0.655. With it, a 90 kg patient's apparent clearance is 0.061 L/h, the value the paper reports in type 2 diabetes. The half-life is about five and a half days.

### What is not plotted

Concentration only, in ng/mL. The FDA's linked model of fasting glucose and HbA1c describes changes over months and is not drawn. See [Antidiabetic models and their limits](help:models/antidiabetics).
