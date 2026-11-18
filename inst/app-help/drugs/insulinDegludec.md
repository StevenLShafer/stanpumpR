### The model

Tang and colleagues, at Jiangsu Hengrui, fitted one population model to their new basal insulin INS068 and to insulin degludec (*Clin Pharmacokinet* 2026;65:417-429), to set INS068's unit against degludec's. Degludec itself was measured by LC-MS/MS in 102 subjects: healthy men after single doses of 3.6 and 7.2 nmol/kg, and adults with type 1 diabetes given 0.4, 0.6 and 0.8 U/kg once daily for 9 days and sampled to 120 hours after the last dose. The model has one compartment, first-order absorption after a lag of 0.4 hours, and linear elimination. Degludec differs from INS068 only in its absorption rate (× 0.837).

The library uses the typical values for **type 1 diabetes**, where most of the degludec data lie: absorption rate 0.0339/h, apparent clearance 1.89 L/h and apparent volume 11.2 L at 70 kg. Clearance scales as (weight/70)^1.04 and volume as (weight/70)^1.54. Tang prints only the middle 90% of its subjects' weights, 54.4 to 93.1 kg, so the two ends of the weight range were decided separately (Steven L. Shafer, 18 November 2026):

- **Above 93.1 kg** the model holds the weight at 93.1 kg. Tang's exponent is steeper than Novo Nordisk's own measurements in heavy adults: Kapitza 2015 (below) found exposure falls with weight with an exponent of about 0.44, up to 156.8 kg. Holding the weight reproduces that. At 150 kg and 0.4 U/kg once daily the model's average level is 5.91 nmol/L against 5.68 by Kapitza's exponent, where Tang's term would give 3.60, 37% too low.
- **Below 54.4 kg**, including children, Tang's terms carry on. Tang studied no child, but Novo Nordisk's pediatric population model, reviewed and accepted by the FDA (Clinical Pharmacology Review of NDA 203314/S-03, 2016), was fitted in 205 subjects aged 1.5 to 57 years weighing 11.2 to 102 kg. It puts weight on clearance with an exponent of 0.977 (95% CI 0.815 to 1.14), which includes Tang's 1.04, and found no effect of age once weight was in. The reviewer concluded that "when dosed per kg body weight, the exposure becomes independent of body weight", and the label agrees. Holding a child at 54.4 kg had put a 20 kg child's level at a third of that.

Two cautions for children. Below 11.2 kg, the lightest child studied, the weight term is an extrapolation. And Tang's exponent on the volume, 1.54, is just outside the pediatric model's range for the volume (1.01, 95% CI 0.49 to 1.52). That leaves the average level unchanged, but for an 11 kg child the peak is about 10% higher and the trough 12% lower than the pediatric exponent would give.

Absorption is slower than elimination, so the curve's terminal half-life is the absorption half-life, 20.5 hours, as the label says ("determined primarily by the rate of absorption"). Steady state is reached within 3 to 4 doses.

Concentrations are total degludec (more than 99% protein-bound) in **mcU/mL**. One unit of degludec is 6 nmol, so 1 mcU/mL is 6 pmol/L. A degludec concentration cannot be compared with a human-insulin concentration: degludec binds the insulin receptor weakly and is bound to albumin, so it circulates far above human insulin's concentrations for the same effect.

### What was not applied

- **Type 2 diabetes.** Tang's type 2 diabetes factors were estimated from INS068 troughs alone, with no degludec concentrations. Applied to degludec, they would slow absorption fivefold and delay steady state by weeks. The label and the FDA review contradict that: the review found the pharmacokinetics consistent in healthy subjects and in type 1 and type 2 diabetes.
- **Healthy subjects.** The healthy typical values (absorption 0.0398/h, clearance 1.93 L/h, volume 20.9 L) are used only to check the model below.

### Checks against data the model was not fitted to

- **Label** (TRESIBA, revised 7/2022), type 1 diabetes, 0.4 U/kg once daily for 8 days: peak 4472 pmol/L at a median 9 hours. The model gives 4174 pmol/L (696 mcU/mL) at 8.1 hours for a 70 kg patient, 7% lower. The label's half-life at steady state, about 25 hours (Heise 2015 measured 25.4 hours), is longer than the model's 20.5 hours.
- **FDA clinical pharmacology review** (NDA 203314), healthy subjects, one 0.4 U/kg dose: peak 2083 pmol/L at a median 14 hours, clearance 0.029 L/h/kg, half-life 16 hours. Tang's healthy values give 1833 pmol/L at 16.4 hours, 0.028 L/h/kg and 17.4 hours.
- **Cao 2026**: 30 healthy Chinese men, one dose, sampled for 24 hours. Its clearance, 1.49 L/h, is 15% below Tang's healthy value at the same median weight. Its covariate equations are not printed, and 24 hours of sampling cannot resolve a 17- to 25-hour absorption half-life, so it is not used.
- **Kapitza 2015**: Novo Nordisk's population analysis in type 2 diabetes. Exposure fell with weight less steeply than Tang's exponent predicts: 0.77 at 156.8 kg and 1.38 at 41 kg, against 85.8 kg. Holding the weight at 93.1 kg keeps the model close to that in heavy adults.

### Routes

Subcutaneous injection, `U SC` or `U/kg SC`, or `U SC qd` once daily. TRESIBA U-100 and U-200 are one drug: their exposures are equivalent at the same dose in units. The label forbids intravenous use and insulin pumps, and none is offered.

### What is not plotted

Concentration only. Glucose, a clamp's glucose infusion rate and HbA1c are separate endpoints and are not drawn. No therapeutic range is established, so there is no band: the dose is titrated to glucose. Pregnancy: no pharmacokinetic study of degludec in pregnancy was found. With **Pregnant** ticked, the clearance is raised 1.25-fold, the rise Catalano 1998 measured in the clearance of insulin late in pregnancy, which every insulin in the library takes: an insulin's formulation should not materially change its clearance unless specific data say otherwise (Steven L. Shafer, 2026-11-18). See [Antidiabetic models and their limits](help:models/antidiabetics) and [subcutaneous doses](help:models/absorption).
