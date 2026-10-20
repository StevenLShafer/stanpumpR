### The model

Tham and colleagues pooled 16 pharmacokinetic and clamp studies, mostly in healthy volunteers (*J Clin Pharmacol* 2017), into one-compartment models with an absorption rate for each formulation and an apparent volume and clearance for each insulin. Table 2's values are used with the covariates of the supplement's equations. The volume grows with body weight, 178 L × (weight/80)^1.62. The clearance falls as the injection grows, 127 L/h × (dose/10 U)^-0.282, taking the whole 70/30 injection as the dose, and the regular component's absorption slows with its 30% share, 0.67/h × (share/10 U)^-0.275. Each injection is simulated with the parameters for its own size and the injections are added, as the model was fitted to single doses. Weight and dose are held within the studied 53.1 to 124 kg and 3 to 100 U. Concentrations are in **mcU/mL** of the injected insulin, the exogenous insulin alone: the endogenous baseline the paper estimated is not added. Human 70/30 premix is modelled, as Tham modelled it, as its two components: 70% of each dose is absorbed as NPH (0.129/h after a 0.378 h lag) and 30% as regular insulin (0.67/h, no lag), from two depots, on the shared human-insulin volume and clearance.

### Routes

Subcutaneous injection, `U SC`, twice daily. Premixes are not pump insulins.

### What is not plotted

Concentration only. Glucose, a clamp's glucose infusion rate and HbA1c are separate endpoints and are not drawn; a unit of one insulin is not converted into another's. See [Antidiabetic models and their limits](help:models/antidiabetics) and [subcutaneous doses](help:models/absorption).

### Pregnancy

With **Pregnant** ticked, the apparent clearance is multiplied by 1.19, the rise in the metabolic clearance of insulin measured by clamp in 16 women at 34-36 weeks against before conception, 526 against 442 mL/m²/min (Catalano, *Diabetes Care* 1998); the volume and the absorption are unchanged. The larger insulin requirement of late pregnancy is mostly insulin resistance, which is not plotted. The same factor applies to every insulin in the library. Note that the volume in this model grows steeply with body weight, so a term-pregnant weight already enlarges it, by weight alone and with no pregnancy data behind it; the pre-pregnancy weight avoids that. See [Pregnancy](help:models/pregnancy).
