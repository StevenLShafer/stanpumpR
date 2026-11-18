### The model

Tang and colleagues, at Jiangsu Hengrui, fitted one population model to their new basal insulin INS068 and to insulin degludec (*Clin Pharmacokinet* 2026;65:417-429), to set INS068's unit against degludec's. Degludec itself was measured by LC-MS/MS in 102 subjects: healthy men after single doses of 3.6 and 7.2 nmol/kg, and adults with type 1 diabetes given 0.4, 0.6 and 0.8 U/kg once daily for 9 days and sampled to 120 hours after the last dose. The model has one compartment, first-order absorption after a lag of 0.4 hours, and linear elimination. Degludec differs from INS068 only in its absorption rate (× 0.837).

The library uses the typical values for **type 1 diabetes**, where most of the degludec data lie: absorption rate 0.0339/h, apparent clearance 1.89 L/h and apparent volume 11.2 L at 70 kg. Clearance scales as (weight/70)^1.04 and volume as (weight/70)^1.54. Weight is held between 54.4 and 93.1 kg, the 5th and 95th percentiles the paper prints. The model is for adults: no child was studied.

Absorption is slower than elimination, so the curve's terminal half-life is the absorption half-life, 20.5 hours, as the label says ("determined primarily by the rate of absorption"). Steady state is reached within 3 to 4 doses.

Concentrations are total degludec (more than 99% protein-bound) in **mcU/mL**. One unit of degludec is 6 nmol, so 1 mcU/mL is 6 pmol/L. A degludec concentration cannot be compared with a human-insulin concentration: degludec binds the insulin receptor weakly and is bound to albumin, so it circulates far above human insulin's concentrations for the same effect.

### What was not applied

- **Type 2 diabetes.** Tang's type 2 diabetes factors were estimated from INS068 troughs alone, with no degludec concentrations. Applied to degludec, they would slow absorption fivefold and delay steady state by weeks. The label and the FDA review contradict that: the review found the pharmacokinetics consistent in healthy subjects and in type 1 and type 2 diabetes.
- **Healthy subjects.** The healthy typical values (absorption 0.0398/h, clearance 1.93 L/h, volume 20.9 L) are used only to check the model below.

### Checks against data the model was not fitted to

- **Label** (TRESIBA, revised 7/2022), type 1 diabetes, 0.4 U/kg once daily for 8 days: peak 4472 pmol/L at a median 9 hours. The model gives 4174 pmol/L (696 mcU/mL) at 8.1 hours for a 70 kg patient, 7% lower. The label's half-life at steady state, about 25 hours (Heise 2015 measured 25.4 hours), is longer than the model's 20.5 hours.
- **FDA clinical pharmacology review** (NDA 203314), healthy subjects, one 0.4 U/kg dose: peak 2083 pmol/L at a median 14 hours, clearance 0.029 L/h/kg, half-life 16 hours. Tang's healthy values give 1833 pmol/L at 16.4 hours, 0.028 L/h/kg and 17.4 hours.
- **Cao 2026**: 30 healthy Chinese men, one dose, sampled for 24 hours. Its clearance, 1.49 L/h, is 15% below Tang's healthy value at the same median weight. Its covariate equations are not printed, and 24 hours of sampling cannot resolve a 17- to 25-hour absorption half-life, so it is not used.
- **Kapitza 2015**: Novo Nordisk's population analysis in type 2 diabetes. Exposure fell with weight less steeply than Tang's exponent predicts: 0.77 at 156.8 kg and 1.38 at 41 kg, against 85.8 kg. The weight hold keeps the model close to that.

### Routes

Subcutaneous injection, `U SC` or `U/kg SC`, or `U SC qd` once daily. TRESIBA U-100 and U-200 are one drug: their exposures are equivalent at the same dose in units. The label forbids intravenous use and insulin pumps, and none is offered.

### What is not plotted

Concentration only. Glucose, a clamp's glucose infusion rate and HbA1c are separate endpoints and are not drawn. No therapeutic range is established, so there is no band: the dose is titrated to glucose. Pregnancy: no pharmacokinetic study of degludec in pregnancy was found, so nothing is applied. See [Antidiabetic models and their limits](help:models/antidiabetics) and [subcutaneous doses](help:models/absorption).
