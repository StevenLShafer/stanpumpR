### The model

Tham and colleagues pooled 16 pharmacokinetic and clamp studies, mostly in healthy volunteers (*J Clin Pharmacol* 2017), into one-compartment models with an absorption rate for each formulation and an apparent volume and clearance for each insulin. Table 2's values are used with the covariate of the supplement's equations: the volume grows steeply with body weight, 43.0 L × (weight/80)^2.48, held within the 52.6 to 86 kg the lispro studies covered (the paper's own check in obese subjects was external). Lispro has no dose term. Concentrations are in **mcU/mL** of the injected insulin, the exogenous insulin alone: the endogenous baseline the paper estimated is not added. Humalog Mix50/50 is modelled as its components: half of each dose as lispro protamine suspension (0.0365/h, no lag) and half as insulin lispro (72.9% of it first-order at 0.989/h after a 0.265 h lag, 27.1% at a constant rate over 1.06 hours), on the lispro volume and clearance. The lispro component's zero-order input enters the circulation directly, as [insulin lispro](help:drugs/insulinLispro)'s does. In pregnancy, enter the pre-pregnancy weight: a term-pregnant weight enlarges this volume by weight alone, with no pregnancy data behind it.

### Routes

Subcutaneous injection, `U SC`, twice daily. Premixes are not pump insulins.

### Effect site

Every insulin shares one effect site, with an equilibration rate constant (ke0) of 0.876 per hour, so that the effect lags the plasma insulin by the time insulin takes to act, and each product's own absorption sets when its effect peaks. The constant reproduces the 23-minute peak of the glucose infusion rate after an intravenous bolus of regular insulin (Howey 1994; Galloway 1992's study, 20 minutes) and puts the effect peaks of subcutaneous lispro, regular insulin and NPH within an hour of the clamp studies' (2.2, 2.5 and 5.0 hours against about 1.7 to 1.9, 2.9 to 3.2 and 5.4 to 5.9: lispro 0.3 to 0.6 hours late, regular insulin 0.3 to 0.7 and NPH 0.4 to 0.9 hours early, differences that follow each product's absorption model), and it agrees with the FDA's analysis of 72 clamps after subcutaneous insulin aspart, 0.996 per hour (95% CI 0.88 to 1.11). How it was derived is on the page [Antidiabetic models and their limits](help:models/antidiabetics).

### What is not plotted

The concentration and its effect site only. Glucose, a clamp's glucose infusion rate and HbA1c are separate endpoints and are not drawn; a unit of one insulin is not converted into another's. See [Antidiabetic models and their limits](help:models/antidiabetics) and [subcutaneous doses](help:models/absorption).
