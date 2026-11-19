### The model

Tham and colleagues pooled 16 pharmacokinetic and clamp studies, mostly in healthy volunteers (*J Clin Pharmacol* 2017), into one-compartment models with an absorption rate for each formulation and an apparent volume and clearance for each insulin. Table 2's values are used with the covariates of the supplement's equations. The volume grows with body weight, 178 L × (weight/80)^1.62. The clearance falls as the injection grows, 127 L/h × (dose/10 U)^-0.282; NPH's absorption rate has no dose term. Each injection is simulated with the parameters for its own size and the injections are added, as the model was fitted to single doses. Weight and dose are held within the studied 53.1 to 124 kg and 3 to 100 U. Concentrations are in **mcU/mL** of the injected insulin, the exogenous insulin alone: the endogenous baseline the paper estimated is not added. Human NPH (isophane) insulin is absorbed first-order at 0.129/h after a lag of 0.378 h, on the human-insulin volume and clearance (178 L, 127 L/h): a 10 U dose peaks at about 7 mcU/mL some three hours after injection. In pregnancy, enter the pre-pregnancy weight: a term-pregnant weight enlarges this volume by weight alone, with no pregnancy data behind it.

### Routes

Subcutaneous injection, `U SC` or `U/kg SC`, once or twice daily.

### Effect site

Every insulin shares one effect site, with an equilibration rate constant (ke0) of 0.876 per hour, so that the effect lags the plasma insulin by the time insulin takes to act, and each product's own absorption sets when its effect peaks. The constant reproduces the 23-minute peak of the glucose infusion rate after an intravenous bolus of regular insulin (Howey 1994; Galloway 1992's study, 20 minutes) and puts the effect peaks of subcutaneous lispro, regular insulin and NPH within about half an hour of the clamp studies' (2.2, 2.5 and 5.0 hours against about 1.8, 3.0 and 5.4 to 5.9), and it agrees with the FDA's analysis of 72 clamps after subcutaneous insulin aspart, 0.996 per hour (95% CI 0.88 to 1.11). How it was derived is on the page [Antidiabetic models and their limits](help:models/antidiabetics).

### What is not plotted

The concentration and its effect site only. Glucose, a clamp's glucose infusion rate and HbA1c are separate endpoints and are not drawn; a unit of one insulin is not converted into another's. See [Antidiabetic models and their limits](help:models/antidiabetics) and [subcutaneous doses](help:models/absorption).
