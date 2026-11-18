### What is plotted

The curve is the triiodothyronine (T3) that the doses add to the serum: total T3 above the patient's own baseline, in ng/mL. T3 is usually reported in ng/dL; multiply by 100 (the reference range of about 90 to 215 ng/dL is 0.9 to 2.15 ng/mL). In a patient with no thyroid function who takes no levothyroxine, the baseline is close to zero and the curve is the serum T3.

In anyone else the curve is not the measured T3. A working thyroid, and any levothyroxine the patient takes, add T3 of their own, and liothyronine lowers TSH and with it the gland's output. Neither is modelled.

### The model

The model comes from Van Tassell and colleagues (*Thyroid* 2019;29:1371-1379, doi 10.1089/thy.2019.0101). They studied 14 patients who had had a near-total thyroidectomy for thyroid cancer. The patients averaged 48 years and 72 kg, and were half men and half women. For at least 30 days the patients took liothyronine alone, three times a day, in place of their levothyroxine. Their free T4 was at or below the assay's limit, so all their T3 came from the tablets. After the last morning dose, T3 was measured for 11 days.

Each patient was fitted with two compartments and a lagged first-order absorption. The model uses the means of the 14 fits (Table 3): a lag of 0.62 hours, an absorption rate of 1.94 /h, a central volume of 8.67 L, and elimination, distribution and return rate constants of 0.12, 0.13 and 0.08 /h. That is a clearance of 1.04 L/h (25 L/day) and half-lives of 2.3 and 21.5 hours. Absorption was taken as complete, so the volumes and clearance are apparent; the Cytomel label says T3 is "almost totally absorbed, 95 percent in 4 hours".

**Why the steady-state fit.** The same paper also gave a first dose to five of the patients while they were profoundly hypothyroid, after their radioiodine treatment. Their volume was about 1.7 times larger and their peak T3 less than half as high (140 against 320 ng/dL). The authors attribute this to binding sites freed by the loss of thyroid hormone. The model describes a patient already taking liothyronine, which is how the drug is used. A first dose given to a profoundly hypothyroid patient will peak well below this curve.

No covariate was fitted: 14 patients were too few to show even a difference between men and women. With [the scaling switch](help:models/fat-free-mass) on, the volumes and clearances scale with fat-free mass from the 70 kg reference man; with it off they are the published values for every patient.

### Checks

- **The source's own patients**, 18.8 mcg every 8 hours at steady state: the model peaks at 306 ng/dL at 1.7 hours, against the measured 320 ± 60 ng/dL at 1.8 hours.
- **The source's own simulation** of 50 mcg a day: a mean of 206 ng/dL whether given once, twice or three times a day. The model gives 200 ng/dL, with peaks of 515, 327 and 271 ng/dL for once, twice and three times a day.
- **Inada 1975** (*J Clin Invest* 55:1337), intravenous radiolabelled T3 in five normal men: a clearance of 22.2 L/day per 60 kg, 26.8 L/day at the source's 72 kg, against the model's 25.0 L/day.
- **Healthy volunteers** (Celi's group, *Front Endocrinol* 2022), not used: after 52.5 mcg the peak rose 327 ng/dL above baseline at 2 hours. The model gives 449 ng/dL, 37% higher. Volunteers with a working thyroid are not patients on liothyronine, and the source's own two states differ by more than this.

### Dose

Doses are entered as labelled, in micrograms. Cytomel tablets "contain liothyronine sodium equivalent to liothyronine", so a 25 mcg tablet is 25 mcg of T3. The units offered are a single oral dose and doses once, twice or three times a day.

Intravenous liothyronine (for myxedema coma) is not offered. Its label says no pharmacokinetic study of it has been done in myxedema, and this model was fitted in the opposite state, patients replete with T3.

### Pregnancy

No pharmacokinetic study of liothyronine in pregnancy was found, and levothyroxine, not liothyronine, is the treatment of hypothyroidism in pregnancy. Thyroxine-binding globulin rises in pregnancy, which raises total T3 for the same free T3. Nothing is applied.

### No band

No band is drawn. The total T3 reference range is a normal range for the hormone, not a therapeutic range for the drug. Liothyronine is titrated to TSH.
