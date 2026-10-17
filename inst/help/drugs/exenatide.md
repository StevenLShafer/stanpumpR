### The model

Cirincione and Mager's population analysis of eight studies (*Br J Clin Pharmacol* 2017), with healthy volunteers and patients with type 2 diabetes whose renal function ran from normal to end-stage, given exenatide subcutaneously, as an intravenous bolus and as an infusion. Two compartments, eliminated in parallel by a linear pathway and a saturable (Michaelis–Menten) one:

- linear clearance 4.58 L/h × (eGFR/80)^0.838, from the MDRD eGFR (5.06 L/h at an eGFR of 90);
- saturable elimination, Vmax 1.55 mcg/h and Km 567 pg/mL;
- central volume 7.03 L × (weight/84.8)^2.67, intercompartmental clearance 3.72 L/h, peripheral volume 7.04 L.

A subcutaneous dose is absorbed in two steps, one after the other. First, 37% of the dose (F × (1 − fr), with fr 0.628 and F 1) enters at a constant rate over 1.35 hours. Then the rest is absorbed by a saturable process: maximum 12.8 mcg/h, half-maximal at 16.9 mcg in the depot. At the approved doses (5 and 10 mcg) the absorption is far from saturation.

Neither the elimination nor the absorption is linear, so doses do not simply add. The curve comes from numerical integration of the paper's equations over the whole dose table (the engine that runs [phenytoin](help:drugs/phenytoin)), not from the closed-form solutions that run most drugs.

### Covariates

Renal function acts on the linear clearance only, through the MDRD eGFR (no race term) calculated from the Patient Profile's serum creatinine. A blank field means an assumed normal creatinine for the patient's age and sex. At 210 pg/mL the total clearance is 5.1, 6.3 and 7.5 L/h at eGFRs of 50, 75 and 100, as the paper reports.

Body weight acts on the central volume only, and steeply. For that term the weight is held within the studied 52.6 to 162 kg, because a 40 kg patient would otherwise get a central volume of 0.9 L.

### Product and doses

Byetta, `mcg SC` and `mcg SC bid`. Each dose has its own 1.35-hour constant-rate phase. The paper writes its equations for one dose, so here the doses share the saturable depot. Twice daily, little is left in the depot when the next dose arrives (under 1% of a 10 mcg dose after 12 hours), and the third day's peak is 5% above the first.

A 10 mcg dose in a 75 kg man with an eGFR of 90 peaks at 263 pg/mL at 1.35 hours, the end of the constant-rate phase. The paper's simulations, which include between-subject variability, give 287 pg/mL for the typical 75 kg subject, and the observed peaks came at about 2 hours. The peak is 22% lower at 100 kg and 38% lower at 125 kg (paper: 18% and 34%).

The extended-release products (Bydureon, Bydureon BCise) are different formulations and are not modelled.

### What is not plotted

Concentration only, in pg/mL. Neither glucose, insulin nor gastric emptying is drawn. The time-until-threshold column is blank, as for every drug run by the numerical engine. See [Antidiabetic models and their limits](help:models/antidiabetics).
