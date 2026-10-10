### The model

None of the published studies of oxycodone fitted a population model, so this one is assembled from five of them. The intravenous disposition pools three Finnish studies: Pöyhiä and colleagues (*Br J Clin Pharmacol* 1991), the control patients of Kirvelä and colleagues (*J Clin Anesth* 1996), and the youngest group of Liukas and colleagues (*Drugs Aging* 2011). Weighted by the number of subjects, the pooled clearance is 13.2 mL/min/kg and the steady-state volume 2.93 L/kg. That is 0.92 L/min and 205 L at 70 kg.

The split into two compartments comes from Kirvelä's per-subject biexponential fits. The central volume is 13% of the steady-state volume (27 L at 70 kg), and the intercompartmental clearance is 2.3 times the clearance (2.15 L/min). The terminal half-life is 3.4 hours. The central volume is the least certain number: the early intravenous concentrations in these studies disagree widely.

### Oral and intravenous

Oxycodone can be given as **mg** (intravenous bolus) or **mg PO**. Oral bioavailability is 0.67: the pooled intravenous clearance divided by the oral clearance that Lalovic and colleagues measured (*Clin Pharmacol Ther* 2006). Absorption has a rate constant of 0.01/min (a half-time of 69 minutes) and no lag. That reproduces the 30 ng/mL peak of Lalovic's mean curve after 15 mg and the concentrations over the following 12 hours. The model's peak comes at 35 minutes, earlier than Lalovic's mean of 65 minutes. See [the oral oxycodone scenario](scenario:oral-oxycodone) and [Oral, intramuscular and intranasal doses](help:models/absorption).

### Covariates

- **Age.** Liukas found clearance 28 to 34 per cent lower in patients over 60. With the renal part removed, clearance falls by 0.53 per cent a year above 30, so it is 0.74 of the young value at 80.
- **Renal function.** Kirvelä's patients in end-stage renal failure had a clearance per kilogram 0.75 of the controls'. Clearance falls linearly with CKD-EPI eGFR below 100 mL/min/1.73 m², to 0.73 at an eGFR of zero. A blank creatinine assumes a normal value for age and sex, so renal impairment counts only when a **creatinine** is entered.
- **Size.** Under the default [fat-free-mass scaling](help:models/fat-free-mass), volumes follow fat-free mass and clearances its 0.75 power. Unticking the box scales both linearly with total weight, as the per-kilogram studies reported them.

In children the model is less certain. Balyan and colleagues' 30 children aged 2 to 17 cleared oral oxycodone about a third faster than this model predicts for their size.

### Effect site

Lalovic measured pupil constriction after oral oxycodone and found that the parent drug alone explained it, with an effect-site equilibration half-time of 11 minutes. Its metabolites contributed nothing. The model's tPeak of 12.35 minutes after an intravenous bolus reproduces that ke0 in a 70 kg adult. So oxycodone's effect follows its plasma concentration closely, and an oral dose's effect peaks about an hour after the dose.

### MEAC and typical concentrations

MEAC is 12 ng/mL, unchanged from the earlier model: a compromise between the lower values suggested by Mandema and the 45 to 50 ng/mL suggested by Kokki in 2012. None of the five studies behind this model measured analgesia. Lalovic's EC50 of 30 ng/mL is for miosis. The shaded band is 10 to 20 ng/mL.

### Active metabolite

Oxycodone forms **oxymorphone** by CYP2D6, so a dose of oxycodone adds an [oxymorphone](help:drugs/oxymorphone) row. Oxymorphone was undetectable or barely detectable after intravenous oxycodone in the Finnish studies. The model sets its AUC at 1 per cent of oxycodone's after an intravenous dose. After an oral dose, more forms on the first pass through the liver. The model puts that AUC at 3.35 per cent, between Balyan's 2.7 per cent in genotyped normal metabolisers and Lalovic's 4 per cent. The **CYP 2D6** field scales both. Balyan found intermediate metabolisers at about 0.63 of normal, and the model uses 0.65. See [Active metabolites](help:models/metabolites).
