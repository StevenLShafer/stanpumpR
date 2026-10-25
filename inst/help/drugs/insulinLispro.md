### The model

Tham and colleagues pooled 16 pharmacokinetic and clamp studies, mostly in healthy volunteers (*J Clin Pharmacol* 2017), into one-compartment models with an absorption rate for each formulation and an apparent volume and clearance for each insulin. Table 2's values are used with the covariate of the supplement's equations: the volume grows steeply with body weight, 43.0 L × (weight/80)^2.48, held within the 52.6 to 86 kg the lispro studies covered (the paper's own check in obese subjects was external). Lispro has no dose term. Concentrations are in **mcU/mL** of the injected insulin, the exogenous insulin alone: the endogenous baseline the paper estimated is not added. Insulin lispro U-100 (Humalog) has a volume of 43.0 L and clearance of 30.5 L/h. 72.9% of each dose is absorbed first-order at 0.989/h after a lag of 0.265 h; the other 27.1% enters the circulation at a constant rate over 1.06 hours. The zero-order share enters the circulation directly from the moment of injection, and the lag delays the depot only: the paper's Figure 1 draws the zero-order input from the injection site straight into the central volume, and its supplement defines the lag as that of the first-order absorption.

### Routes

Subcutaneous injection, `U SC` or `U/kg SC` (three times daily with meals), and by **insulin pump**, `U/hr SC`, which Humalog U-100's label permits. A pump rate runs from its row's time until the next `U/hr SC` row; its zero-order share is given as that of each 2-minute portion of the delivery. At a steady rate the concentration settles at the rate over the clearance: 1 U/hr gives 32.8 mcU/mL. Humalog U-200, which must not be used in a pump, is not offered.

### What is not plotted

Concentration only. Glucose, a clamp's glucose infusion rate and HbA1c are separate endpoints and are not drawn; a unit of one insulin is not converted into another's. See [Antidiabetic models and their limits](help:models/antidiabetics) and [subcutaneous doses](help:models/absorption).

### Pregnancy

With **Pregnant** ticked, the apparent clearance is multiplied by 1.19, the rise in the metabolic clearance of insulin measured by clamp in 16 women at 34-36 weeks against before conception, 526 against 442 mL/m²/min (Catalano, *Diabetes Care* 1998); the volume and the absorption are unchanged. The larger insulin requirement of late pregnancy is mostly insulin resistance, which is not plotted. The same factor applies to every insulin in the library. Note that the volume in this model grows steeply with body weight, so a term-pregnant weight already enlarges it, by weight alone and with no pregnancy data behind it; the pre-pregnancy weight avoids that. See [Pregnancy](help:models/pregnancy).
