### The model

Immediate-release quetiapine, from Zheng and colleagues (*Front Psychiatry* 2024;15:1497119): 99 Chinese inpatients with bipolar affective disorder, aged 17 to 69 and weighing 43 to 119 kg, sampled at steady state. One compartment with first-order absorption. At 70 kg the apparent clearance is 76.1 L/h and the apparent volume 530 L; the absorption rate constant, 1.46 per hour, was **fixed** from earlier literature, not estimated. The volume is imprecise (bootstrap 90% interval 210 to 2088 L).

### Oral only, immediate release only

The parameters are **apparent**, divided by an unmeasured oral bioavailability, so quetiapine is offered as **mg PO** only (also as a qd, bid or tid [scheduled dose](help:dose-table)) and bioavailability is carried as 1. Extended-release quetiapine absorbs through a chain of transit compartments, and its published model (Brogren and Nyberg 2010) lacks the transit number, intercompartmental clearance and volumes, so it is **not** offered. Do not use this curve for an XR tablet.

### Covariates

Weight is the source's only covariate: clearance with the 0.75 power of weight over 70 kg, volume in proportion to it. With the [fat-free-mass switch](help:models/fat-free-mass) off those allometric terms are applied exactly; with it on, the library's fat-free-mass factors replace them.

### Effect

There is **no effect site**. Nord and colleagues (*Int J Neuropsychopharmacol* 2011;14:1357-1366) related striatal D2 occupancy directly to **plasma** quetiapine in 11 healthy men: occupancy = 100% × C / (525 ng/mL + C) (1369 nmol/L, Emax fixed at 100%). Half of the receptors are occupied at 525 ng/mL and 80% at 2100 ng/mL. That is a receptor biomarker measured in healthy volunteers, not a therapeutic target, and the pharmacokinetics come from a different population. Norquetiapine, which also binds D2, is not modelled.

The shaded band, **100 to 500 ng/mL** with 300 as the typical value (its midpoint), is the AGNP therapeutic reference range for quetiapine at trough, unchanged from the 2011 consensus through the 2017 update (Hiemke and colleagues, *Pharmacopsychiatry* 2018;51:9-62) to the 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5, https://doi.org/10.1055/a-2860-7861), for samples 24 hours after a dose, with a laboratory alert level of 1000 ng/mL. The range is derived from concentrations in patients with schizophrenia who responded, and does not include norquetiapine. Concentrations above 2000 ng/mL predicted severe acute intoxication. With the extended-release product taken in the evening and blood drawn in the morning, expect about twice the trough.

### Pregnancy

With **Pregnant** ticked, the apparent clearance is multiplied by 4.2; the volume and the absorption are unchanged. Westin and colleagues (*Clin Pharmacol Ther* 2018;103:477-484) followed 35 quetiapine pregnancies through routine drug monitoring and found the dose-adjusted serum concentration 76% lower in the third trimester than the same women's own baseline (95% CI 66 to 83%), the largest fall of the antipsychotics, and 1/0.24 is 4.2. Quetiapine is cleared by CYP3A4, which pregnancy induces. A physiologically based model (Zheng and colleagues, *Clin Pharmacokinet* 2021;60:623-635) predicts about half that change, a 49% lower exposure; the direct, paired measurement is the one applied. See [Pregnancy](help:models/pregnancy).
