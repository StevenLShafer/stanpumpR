### What is plotted

The curve is the triiodothyronine (T3) that the doses add to the serum: total T3 above the patient's own baseline, in ng/mL. T3 is usually reported in ng/dL; multiply by 100 (the reference range of about 90 to 215 ng/dL is 0.9 to 2.15 ng/mL). In a patient with no thyroid function who takes no levothyroxine, the baseline is close to zero and the curve is the serum T3.

In anyone else the curve is not the measured T3. A working thyroid, and any levothyroxine the patient takes, add T3 of their own, and liothyronine lowers TSH and with it the gland's output. Neither is modelled.

### The model

The model comes from Van Tassell and colleagues (*Thyroid* 2019;29:1371-1379, doi 10.1089/thy.2019.0101). They studied 14 patients who had had a near-total thyroidectomy for thyroid cancer. The patients averaged 48 years and 72 kg, and were half men and half women. For at least 30 days the patients took liothyronine alone, three times a day, in place of their levothyroxine. Their free T4 was at or below the assay's limit, so all their T3 came from the tablets. After the last morning dose, T3 was measured for 11 days.

Each patient was fitted with two compartments and a lagged first-order absorption. The model uses the means of the 14 fits (Table 3): a lag of 0.62 hours, an absorption rate of 1.94 /h, a central volume of 8.67 L, and elimination, distribution and return rate constants of 0.12, 0.13 and 0.08 /h. That is a clearance of 1.04 L/h (25 L/day) and half-lives of 2.3 and 21.5 hours. Absorption was taken as complete, so the volumes and clearance are apparent; the Cytomel label says T3 is "almost totally absorbed, 95 percent in 4 hours".

**Why the steady-state fit.** The same paper also gave a first dose to five of the patients while they were profoundly hypothyroid, after their radioiodine treatment. Their volume was about 1.7 times larger and their peak T3 less than half as high (140 against 320 ng/dL). The authors attribute this to binding sites freed by the loss of thyroid hormone. The model describes a patient already taking liothyronine, which is how the drug is used. A first dose given to a profoundly hypothyroid patient will peak well below this curve.

No covariate was fitted: 14 patients were too few to show even a difference between men and women. So the published values belong to the study's typical patient: 72.3 kg, 167.7 cm (from the BMI of 25.7), 48.5 years, half men and half women. With [the scaling switch](help:models/fat-free-mass) on, a patient like that receives the published values, and everyone else is scaled from there by fat-free mass, the volumes in proportion and the clearances to the 0.75 power. The 70 kg reference man gets a central volume of 9.5 L and a clearance of 26.8 L/day. With the switch off, every patient gets the published values.

### Checks

- **The source's own patients**, 18.8 mcg every 8 hours at steady state: the model peaks at 306 ng/dL at 1.7 hours, against the measured 320 ± 60 ng/dL at 1.8 hours.
- **The source's own simulation** of 50 mcg a day: a mean of 206 ng/dL whether given once, twice or three times a day. The model gives 200 ng/dL, with peaks of 515, 327 and 271 ng/dL for once, twice and three times a day.
- **Inada 1975** (*J Clin Invest* 55:1337), intravenous radiolabelled T3 in five normal men: a clearance of 22.2 L/day per 60 kg, 26.8 L/day at the source's 72 kg, against the model's 25.0 L/day.
- **Healthy volunteers** (Celi's group, *Front Endocrinol* 2022), not used: after 52.5 mcg the peak rose 327 ng/dL above baseline at 2 hours. The model gives 449 ng/dL, 37% higher.
- **Healthy volunteers, one dose** (Jonklaas 2015, *Ther Drug Monit* 37:110), not used: after a 50 mcg tablet, T3 rose 229 ng/dL above baseline at 2.5 hours and was back to baseline by 48 hours. The model gives a rise of 428 ng/dL at 1.85 hours, and 1.76 times the measured area over 24 hours. The measured terminal half-life, 22 hours, is the model's 21.5.
- **Healthy volunteers with a placebo arm** (Dumitrescu 2022, *Thyroid* 32:196), not used: after 50 mcg, T3 rose 202 ng/dL above placebo at 3 hours and was back to baseline by 36 hours. The model gives 366 ng/dL at 3 hours.
- **Hypothyroid patients on once-daily liothyronine** (Jonklaas and Burman 2016, *Thyroid* 26:770), not used: on 38 mcg a day on average, T3 rose 197 ng/dL from trough to peak. The model gives 325 ng/dL.

So outside its own source the model's swings are 1.6 to 2 times those measured. None of those groups matches the source's patients. The volunteers had working thyroids, which the dose suppresses, and most of the hypothyroid patients still had some thyroid function. The source itself found the volume 1.7 times larger in the same patients when they were hypothyroid. The model describes patients on liothyronine alone with no thyroid of their own, and it may overstate the peaks in anyone else.

### Dose

Doses are entered as labelled, in micrograms. Cytomel tablets "contain liothyronine sodium equivalent to liothyronine", so a 25 mcg tablet is 25 mcg of T3. The units offered are a single oral dose and doses once, twice or three times a day.

Intravenous liothyronine (for myxedema coma) is not offered yet. It will be, with its disposition taken from patients who are hypothyroid, the only patients who receive it (Steven L. Shafer, 2026-11-19). Its label says no pharmacokinetic study of it has been done in myxedema, and this model was fitted in the opposite state, patients replete with T3. The source will be Nicoloff 1972, which gave labelled T3 intravenously to patients with primary hypothyroidism; its tables have not yet been read.

### Pregnancy

No pharmacokinetic study of liothyronine in pregnancy was found, and levothyroxine, not liothyronine, is the treatment of hypothyroidism in pregnancy. Thyroxine-binding globulin rises in pregnancy, which raises total T3 for the same free T3. Nothing is applied.

### No band

No band is drawn. The total T3 reference range is a normal range for the hormone, not a therapeutic range for the drug. Liothyronine is titrated to TSH.
