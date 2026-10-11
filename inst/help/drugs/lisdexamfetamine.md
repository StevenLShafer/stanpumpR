### What is plotted

The plasma concentration of **d-amphetamine**, in ng/mL, after an oral dose of lisdexamfetamine dimesylate (Vyvanse) entered as the capsule strength. Lisdexamfetamine is a prodrug, hydrolysed in red cells; the prodrug itself is not plotted or modelled.

### The model

The parameters are from Tsuda and colleagues (*Drug Metab Pharmacokinet* 2020;35:548-554), who fitted 1365 d-amphetamine concentrations from 194 children and adolescents with ADHD, aged 6 to 17, in Japanese and US studies: one compartment, first-order absorption (0.48/h) after a lag of 0.435 h, and at the 34.1 kg reference an apparent clearance of 8.96 L/h and volume of 133 L, both scaled by weight (exponents 0.600 and 0.776). The absorption term is empirical: it covers absorption of the prodrug and its conversion to d-amphetamine together.

### Dose basis

The capsule strength is the mass of the **dimesylate salt**. Each molecule yields one d-amphetamine, so the d-amphetamine base equivalent is the molar ratio, 0.2968 mg per mg: 8.9, 14.8 and 20.8 mg for the 30, 50 and 70 mg capsules. That conversion is applied once and is shown as the bioavailability; it is a dose basis, not an oral bioavailability.

Tsuda's paper does not print its dose record, so the basis was checked against an independent study. Boellner and colleagues (*Clin Ther* 2010;32:252-264) gave 30, 50 and 70 mg to 18 US children (mean 36 kg) and measured mean peaks of 53.2, 93.3 and 134.0 ng/mL at about 3.5 hours. On the base-equivalent basis this model predicts 44, 74 and 103 ng/mL; on the capsule mass it would predict 2.6 to 2.8 times the observed values. The base-equivalent basis is the only plausible one, but **the model runs 17 to 23 per cent below Boellner's peaks and peaks later (about 4.8 hours)**. Tsuda's between-patient variability does not explain the gap. The parameters are left as published.

### Oral only

Lisdexamfetamine is given by mouth and the parameters are apparent.

### Covariates

The model carries its own weight covariate, so it is evaluated at the pharmacokinetic weight with the [fat-free-mass switch](help:models/fat-free-mass) on and at total weight with it off.

Tsuda estimated clearance 1.26 times higher in the non-Japanese (US) cohort than in the Japanese one. The Patient Profile has no such field; the non-Japanese value is used because it matches the US data above. This is a difference between study cohorts, which may reflect sites, sampling or assays, and is **not** a reason to dose anyone differently by ethnicity.

### Population and variability

Children and adolescents. **Adults are an extrapolation** of the weight equations beyond the fitted range. The variability between people (clearance 18 per cent, absorption rate 56 per cent) is not shown.

### Adderall

Adderall IR and XR are a separate entry, [mixed amphetamine salts](help:drugs/mixedAmphetamineSalts), with a dose basis and a model of their own. They also plot d-amphetamine. The two entries' clearances differ: McGough's Adderall data give about 10.1 L/h at 37.8 kg, against 12.0 L/h from Tsuda's model at the same weight. That difference is about the size of this model's shortfall against Boellner. One possible reason is that not all of a lisdexamfetamine dose becomes d-amphetamine, which would inflate Tsuda's apparent clearance. This is unconfirmed, and the parameters here are left as published.

### Effect site and typical range

None. Tsuda found no clear relationship between d-amphetamine exposure and the fall in ADHD-RS-IV, and no calibrated concentration-effect model exists for a classroom measure or for weekly symptom scores. The 1.5 to 13 hour window of classroom benefit in children (Wigal and colleagues, 2009) is a trial observation, not a concentration threshold. No band is drawn, and nothing on this plot is a dose recommendation.
