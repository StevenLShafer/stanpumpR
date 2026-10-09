### The model

Naloxone's intravenous parameters are from Dowling and colleagues (*Ther Drug Monit* 2008;30:490-496), a three-compartment population model of intravenous, intramuscular and intranasal naloxone in six healthy men: clearance 91 L/h at a **lean body weight of 70 kg**, scaling with lean body weight to the 0.75 power; central volume 2.87 L at 70 kg total weight; peripheral volumes 1.49 and 33.6 L with intercompartmental clearances 5.66 and 29.8 L/h. Note the clearance is normalised to a lean weight, not an ordinary 70 kg man: the reference man of this library (fat-free mass 54.5 kg) has a clearance of 75.4 L/h.

This model replaced the Papathanasiou 2019 weight-proportional model in October 2026 so that the intravenous and nasal routes rest on one specification.

### Nasal spray

The **mg IN** unit is the 4 mg per 0.1 mL concentrated spray. Dowling's own nasal arm used a dilute solution through an atomiser and does not describe it; Laffont and colleagues (*Front Psychiatry* 2024;15:1399803) modelled the spray in 60 adults during a remifentanil challenge, but on an apparent scale (CL/F 396 L/h) with no absolute bioavailability. The route here is **derived**: bioavailability 0.19 makes the reference man's nasal AUC equal Laffont's fitted dose over CL/F (and agrees with the Narcan label's exposure, which implies a CL/F near 500 L/h), and the absorption constant (1.06/h, no lag) matches the mean input time of Laffont's whole published input. Both are initialisers rather than fitted values. The 0.47 to 0.52 figures on labels are relative to intramuscular, which Dowling found incompletely absorbed (F 0.36).

### Covariates

Clearance carries its own lean-body-weight covariate, in either switch position. Dowling used Janmahasatian's lean body weight; stanpumpR uses the fat-free mass of its own [fat-free-mass scaling](help:models/fat-free-mass), Al-Sallami's, which is the same in adult men, the population Dowling studied. In adult women it is 1 to 3 per cent higher, which raises clearance by 1 to 2 per cent. The central volume sees the pharmacokinetic weight with the switch on and total weight with it off; the peripheral parameters, fixed in the source, take the library factors with the switch on.

### Effect site

ke0 is supplied directly from Yassen and colleagues (*Clin Pharmacokinet* 2007;46:965-980), who fitted an equilibration half-time of 6.5 minutes in the reversal of buprenorphine-induced respiratory depression. Against this disposition that puts the peak effect of a bolus at about 3.4 minutes. No antagonism model is attached: the row shows naloxone alone, not the opioid it reverses.

### Typical concentrations

The shaded band (1 to 10 ng/mL) covers what 0.4 mg intravenously produces after distribution; the concentration actually needed depends entirely on the opioid being opposed. The recovery threshold is 1 ng/mL.

### Why it is here

Naloxone's duration is short, and shorter than that of almost every opioid it reverses: a patient who wakes after naloxone may renarcotise as the naloxone leaves and the opioid remains. [The naloxone scenario](scenario:naloxone-morphine) puts the two curves on one picture.

### Where to be careful

Six men is a small anchor and the model says nothing about how much naloxone is needed, which depends on the opioid's affinity (buprenorphine and the fentanyl analogues need more) and on the degree of respiratory depression. The nasal input understates the first few minutes after a spray, because a single first-order input starts more slowly than the immediate component of the published mixture.
