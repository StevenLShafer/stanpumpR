### The model

Tham and colleagues pooled 16 pharmacokinetic and clamp studies, mostly in healthy volunteers (*J Clin Pharmacol* 2017), into one-compartment models with an absorption rate for each formulation and an apparent volume and clearance for each insulin. Table 2's values are used with the covariate of the supplement's equations: the volume grows steeply with body weight, 43.0 L × (weight/80)^2.48, held within the 52.6 to 86 kg the lispro studies covered (the paper's own check in obese subjects was external). Lispro has no dose term. Concentrations are in **mcU/mL** of the injected insulin, the exogenous insulin alone: the endogenous baseline the paper estimated is not added. Humalog Mix75/25 is modelled as its components: 75% of each dose as lispro protamine suspension (absorbed at 0.0365/h, no lag) and 25% as insulin lispro (72.9% of it first-order at 0.989/h after a 0.265 h lag, 27.1% at a constant rate over 1.06 hours), on the lispro volume and clearance (43.0 L, 30.5 L/h). The lispro component's zero-order input enters the circulation directly, as [insulin lispro](help:drugs/insulinLispro)'s does.

### Routes

Subcutaneous injection, `U SC`, twice daily. Premixes are not pump insulins.

### What is not plotted

Concentration only. Glucose, a clamp's glucose infusion rate and HbA1c are separate endpoints and are not drawn; a unit of one insulin is not converted into another's. See [Antidiabetic models and their limits](help:models/antidiabetics) and [subcutaneous doses](help:models/absorption).
