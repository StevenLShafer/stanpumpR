### The model

Kuchimanchi and colleagues' population analysis (*J Pharmacokinet Pharmacodyn* 2018) pooled 3414 adults from eleven phase 1 to 3 studies: healthy volunteers and patients with hypercholesterolemia, aged 18 to 80 years and weighing 41 to 175 kg, half of them women, given 7 to 420 mg of evolocumab intravenously or subcutaneously. The model has one compartment with first-order subcutaneous absorption (ka 0.319/day, bioavailability 72%, both fixed from the densely sampled phase 1a study). Elimination runs by two parallel pathways:

- a linear (nonspecific, proteolytic) clearance of 0.105 L/day;
- a saturable, target-mediated pathway through PCSK9: Vmax 9.85 nM/day (a rate of change of concentration) and km 27.3 nM.

The molecular weight in the label, 144 kDa, puts km at 3.9 mcg/mL and the saturable capacity at 7.35 mg a day in a volume of 5.18 L. Below about 0.4 mcg/mL the target pathway dominates, with a half-life of about 1.9 days. Above about 40 mcg/mL it is saturated and the half-life approaches the linear pathway's 34 days. Clinical doses sit in between, which is why the label gives an "effective half-life of 11 to 17 days".

What is plotted is the **unbound** evolocumab concentration in serum, which is the analyte the source's ELISA measured.

Elimination saturates, so doses do not simply add. The curve comes from numerical integration over the whole dose table, the engine that runs [exenatide](help:drugs/exenatide) and [phenytoin](help:drugs/phenytoin). That engine takes a subcutaneous dose only through exenatide's saturable absorption step. Here that step is set so that it is exactly first order (its saturation amount is 10¹² mg), and the 72% bioavailability is applied to each dose. The test checks this against an independent solution of the paper's equations: the two agree to 1 part in 100,000 of the peak.

### Covariates

The model's own terms are applied:

- weight on all three disposition parameters, about the paper's 84 kg reference patient: linear clearance × (weight/84)^0.276, volume × (weight/84)^1.04 and the saturable capacity × (weight/84)^0.145;
- the volume 11% larger in women.

With the fat-free-mass switch on, the weight in these terms is the pharmacokinetic weight, as for every model with its own weight covariate. The terms are not held to the studied 41 to 175 kg. A volume that scales almost in proportion to weight is a plasma-sized volume, and holding it at 41 kg would give a 20 kg child an adult's volume. The model has not been checked in children beyond the label's troughs in adolescents (see below).

The paper divides each covariate by its population median, which it does not print. The 84 kg used here is the reference patient the paper itself names, the cohort's mean weight.

Three covariates are **not applied**, because the Patient Profile has no field for them, and the curve describes the paper's reference patient, who takes no other lipid-lowering drug:

- a statin raises the saturable capacity 13% (20% with ezetimibe), which lowers exposure. The label reports "approximately 20%" lower Cmax and AUC with a high-intensity statin. Most patients take one.
- higher baseline PCSK9 raises the capacity: × (PCSK9/median)^0.194.

Renal function was tested and is not a covariate. The label reports lower exposure after a single 140 mg dose in severe renal impairment and end-stage disease, and 40 to 50% lower AUC in mild to moderate hepatic impairment. Neither is in the model.

### Doses

Repatha is 140 mg every 2 weeks or 420 mg once a month, subcutaneously (`mg SC`). stanpumpR has no every-2-weeks or monthly schedule, so enter each injection as its own row (140 mg at days 0, 14, 28, and so on). A plot of a few months shows steady state, which the label says is approached by 12 weeks.

### How well it agrees

The model reproduces the two half-lives the paper derives from its parameters (1.9 and 34 days).

It does not reproduce two other statements in the paper's Discussion. The paper says about 77% of a single 140 mg dose, and 51% of a 420 mg dose, is eliminated by the saturable pathway. Its own Table 3 gives 87% and 73%, and no single value of the capacity yields both of the paper's figures. The paper does not say how it computed them.

Against the label's single doses in healthy volunteers, the model peaks lower and later:

| | Label | Model (84 kg man, no statin) |
|---|---|---|
| 140 mg: peak | 18.6 mcg/mL (median 3-4 days) | 10.4 mcg/mL at 5 days |
| 140 mg: AUC | 188 day·mcg/mL | 125 day·mcg/mL |
| 420 mg: peak | 59.0 mcg/mL | 39.5 mcg/mL at 7 days |
| 420 mg: AUC | 924 day·mcg/mL | 789 day·mcg/mL |

The paper itself notes that the model under-predicts some high concentrations, especially after the monthly dose. It also notes that a one-compartment model fitted mostly to subcutaneous data may be biased. Its outcomes of interest, the week 8-12 AUC and the week-12 trough, were well predicted.

At steady state the model's week-12 troughs are 8.5 mcg/mL on 140 mg every 2 weeks and 16.5 mcg/mL on 420 mg monthly, without a statin. The label's phase 3 troughs, from patients most of whom took a statin, are 7.21 and 11.2 mcg/mL. In adolescents (10 to 17 years, 420 mg monthly) the label reports troughs of 22.4 and 25.8 mcg/mL at weeks 12 and 24.

### No band

There is no therapeutic range for evolocumab concentration. The drug is dosed to LDL cholesterol, and the label gives no concentration target. The exposure-response model in the same paper relates the week 8-12 AUC to the LDL cholesterol response. It has an EC50 of 51.5 mcg/mL·day on the every-2-weeks regimen and 118.5 on the monthly one, and both approved regimens reach about 80% of the maximal fall. That is an area, not a concentration, so it is not drawn as a band.

### What is not plotted

Concentration only, in mcg/mL. Neither PCSK9 nor LDL cholesterol is drawn. Time until threshold is blank, as for every drug run by the numerical engine.
