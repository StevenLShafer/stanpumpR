### The model

Olanzapine, from Sun and colleagues (*J Clin Pharmacol* 2021;61:1430-1441): 9905 plasma concentrations from 601 healthy subjects and patients with schizophrenia, given olanzapine alone or with samidorphan. Two compartments, first-order absorption after a lag of 0.782 h (47 min, fixed). For the reference subject, a 70 kg, 36-year-old, nonsmoking, non-Black man, fasted: apparent clearance 15.5 L/h, central volume 656 L, peripheral volume 225 L, intercompartmental clearance 6.15 L/h, absorption rate constant 0.861 per hour.

### Oral only

The parameters are **apparent** (divided by an unmeasured bioavailability), so olanzapine is offered by mouth only: **mg PO**, also once or twice daily (**mg PO qd**, **mg PO bid**).

### Covariates

Clearance scales with the 0.75 power of weight and is 14% lower in women (× 0.862). The central volume scales with weight and with age to the power 0.356, relative to 36 years, with the age term held at 18 years for anyone younger (the source studied adults, and the term would otherwise fall to zero at birth); Sun and colleagues wrote every continuous covariate as a power of its ratio to the reference value, and their reference subject is 36 years old. The peripheral volume and intercompartmental clearance carry no covariates. The model carries its own weight terms, so with the [fat-free-mass switch](help:models/fat-free-mass) on it runs at the pharmacokinetic weight.

Sun found five further effects on clearance that are **not applied**, because the Patient Profile has no field for them or, for renal impairment, the model does not read its serum creatinine: smoking (× 1.30), Black race (× 1.10), rifampin (× 1.80), moderate hepatic impairment (× 0.875) and severe renal impairment (× 0.801). The curve is therefore a **nonsmoker's**; a smoker clears olanzapine 30% faster. Food lowers bioavailability slightly (× 0.943), also not applied.

### Effect

There is **no effect site**. A plasma D2 occupancy relation, occupancy = 100% × C / (10.3 ng/mL + C), is attributed to Kapur and colleagues (*Am J Psychiatry* 1998;155:921-928), but their abstract reports occupancy only by dose, and this EC50 has **not yet been checked** against the paper. It is recorded with that caveat and not plotted.

The shaded band, **20 to 40 ng/mL** 12 to 15 hours after a dose, with 30 as the typical value, is the serum therapeutic reference range of the AGNP task force's systematic review (Wesner and colleagues, *J Clin Psychiatry* 2023;84:22r14626, https://doi.org/10.4088/JCP.22r14626), which replaced the AGNP 2017 range of 20 to 80 ng/mL; the AGNP consensus guidelines' 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5) adopts it, with a laboratory alert level of 80 ng/mL and TDM strongly recommended. The review places 65 to 80% D2 occupancy between 17 and 44 ng/mL. Weight gain can occur within the range; higher concentrations are generally tolerated and need not prompt a dose reduction when the patient responds well and tolerates the drug.
