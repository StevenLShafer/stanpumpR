### The model

None of the published studies of oxycodone fitted a population model, so this one is assembled from five of them. The intravenous disposition pools three Finnish studies: Pöyhiä and colleagues (*Br J Clin Pharmacol* 1991), the control patients of Kirvelä and colleagues (*J Clin Anesth* 1996), and the youngest group of Liukas and colleagues (*Drugs Aging* 2011). Weighted by the number of subjects, the pooled clearance is 13.2 mL/min/kg and the steady-state volume 2.93 L/kg. That is 0.92 L/min and 205 L at 70 kg.

The split into two compartments comes from Kirvelä's per-subject biexponential fits. The central volume is 13% of the steady-state volume (27 L at 70 kg), and the intercompartmental clearance is 2.3 times the clearance (2.15 L/min). The terminal half-life is 3.4 hours. The central volume is the least certain number: the early intravenous concentrations in these studies disagree widely.

### Oral and intravenous

Oxycodone can be given as **mg** (intravenous bolus), **mg/hr** (intravenous infusion), by [target-controlled infusion](help:tci) (**Plasma target** or **Effect site target**, in ng/mL, on the intravenous model), **mg PO**, or as oral liquid, **mg PO liquid** or, for paediatric dosing, **mg/kg PO liquid**. The liquid is simulated exactly like a tablet: the ROXICODONE label and an FDA bioequivalence review both found oral solution and immediate-release tablets equivalent in peak and exposure, with the solution absorbed no faster. Doses are entered in mg, not mL; the 20 mg/mL concentrate is for opioid-tolerant patients only. Oral bioavailability is 0.67: the pooled intravenous clearance divided by the oral clearance that Lalovic and colleagues measured (*Clin Pharmacol Ther* 2006). Absorption has a rate constant of 0.01/min (a half-time of 69 minutes) and no lag. That reproduces the 30 ng/mL peak of Lalovic's mean curve after 15 mg and the concentrations over the following 12 hours. The model's peak comes at 35 minutes, earlier than Lalovic's mean of 65 minutes. See [the oral oxycodone scenario](scenario:oral-oxycodone) and [Oral, intramuscular, intranasal and regional anesthesia doses](help:models/absorption).

### Covariates

- **Age.** Liukas found clearance 28 to 34 per cent lower in patients over 60. With the renal part removed, clearance falls by 0.53 per cent a year above 30, so it is 0.74 of the young value at 80.
- **Renal function.** Kirvelä's patients in end-stage renal failure had a clearance per kilogram 0.75 of the controls'. Clearance falls linearly with CKD-EPI eGFR below 100 mL/min/1.73 m², to 0.73 at an eGFR of zero. A blank creatinine assumes a normal value for age and sex, so renal impairment counts only when a **creatinine** is entered.
- **Size.** Under the default [fat-free-mass scaling](help:models/fat-free-mass), volumes follow fat-free mass and clearances its 0.75 power. Unticking the box scales both linearly with total weight, as the per-kilogram studies reported them.

In children the model is less certain. Balyan and colleagues' 30 children aged 2 to 17 cleared oral oxycodone about a third faster than this model predicts for their size.

### Effect site

Lalovic measured pupil constriction after oral oxycodone and found that the parent drug alone explained it, with an effect-site equilibration half-time of 11 minutes. Its metabolites contributed nothing. The model's tPeak of 12.35 minutes after an intravenous bolus reproduces that ke0 in a 70 kg adult. So oxycodone's effect follows its plasma concentration closely, and an oral dose's effect peaks about an hour after the dose.

### MEAC and typical concentrations

MEAC is 12 ng/mL, set for consistency with morphine's 8 ng/mL. None of the five studies behind this model measured analgesia, and Lalovic's EC50 of 30 ng/mL is for miosis. At equal intravenous doses this model reaches about 1.4 times morphine's concentration, and intravenous oxycodone takes 1 to 1.3 times as many milligrams as morphine for the same analgesia, which puts a MEAC consistent with morphine's at 11 to 15 ng/mL. Measured directly, the values are higher. Kokki and colleagues (*Basic Clin Pharmacol Toxicol* 2012;110:469-475) titrated intravenous oxycodone after laparoscopic cholecystectomy. Pain returned at a mean of 21 ng/mL (range 16 to 31), and relief came at 45 to 50 ng/mL. Morphine's 8 ng/mL sits at the low end of morphine's own literature in the same way, so the two are kept together. The audit of the MEAC and time to peak effect of every opioid in the library (October 2026) confirmed 12 on this basis. The shaded band is 10 to 20 ng/mL. The AGNP consensus guidelines' 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5) gives 5 to 100 ng/mL, 12 hours after a dose, for chronic pain, a spread that depends on tolerance; it is not drawn.

### Active metabolite

Oxycodone forms **oxymorphone** by CYP2D6, so a dose of oxycodone adds an [oxymorphone](help:drugs/oxymorphone) row. Oxymorphone was undetectable or barely detectable after intravenous oxycodone in the Finnish studies. The model sets its AUC at 1 per cent of oxycodone's after an intravenous dose. After an oral dose, more forms on the first pass through the liver. The model puts that AUC at 3.35 per cent, between Balyan's 2.7 per cent in genotyped normal metabolisers and Lalovic's 4 per cent. The **CYP 2D6** field scales both. Balyan found intermediate metabolisers at about 0.63 of normal, and the model uses 0.65. See [Active metabolites](help:models/metabolites).

### Pregnancy

With **Pregnant** ticked, the elimination clearance is multiplied by 3.8/2.6 = 1.46. The review the library follows (Ansari, Carvalho, Shafer and Flood, *Anesth Analg* 2016;122:786-804) reports a shorter elimination half-time in labouring women, 2.6 hours against 3.8, from the higher glomerular filtration rate and induction of CYP3A4 and CYP2D6; the ratio of half-times is the factor, with the volumes unchanged. The renal factor above is not touched (it is capped at normal renal function), and oxymorphone formation is left as it is, as the review gives no figure for the metabolite. See [Pregnancy](help:models/pregnancy).
