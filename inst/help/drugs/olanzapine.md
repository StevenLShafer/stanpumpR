### The model

Olanzapine, from Sun and colleagues (*J Clin Pharmacol* 2021;61:1430-1441): 9905 plasma concentrations from 601 healthy subjects and patients with schizophrenia, given olanzapine alone or with samidorphan. Two compartments, first-order absorption after a lag of 0.782 h (47 min, fixed). For the reference subject, a 70 kg, 36-year-old, nonsmoking, non-Black man, fasted: apparent clearance 15.5 L/h, central volume 656 L, peripheral volume 225 L, intercompartmental clearance 6.15 L/h, absorption rate constant 0.861 per hour.

### Oral only

The parameters are **apparent** (divided by an unmeasured bioavailability), so olanzapine is offered as **mg PO** only.

### Covariates

Clearance scales with the 0.75 power of weight and is 14% lower in women (× 0.862). The central volume scales with weight and with age to the power 0.356, relative to 36 years; the form of the age term (a power) is taken from the brief and was not confirmed in the paper's printed equations. The peripheral volume and intercompartmental clearance carry no covariates. The model carries its own weight terms, so with the [fat-free-mass switch](help:models/fat-free-mass) on it runs at the pharmacokinetic weight.

Sun found four further effects on clearance that the Patient Profile has no field for, so they are **not applied**: smoking (× 1.30), Black race (× 1.10), rifampin (× 1.80), moderate hepatic impairment (× 0.875) and severe renal impairment (× 0.801). The curve is therefore a **nonsmoker's**; a smoker clears olanzapine 30% faster. Food lowers bioavailability slightly (× 0.943), also not applied.

### Effect

There is **no effect site** and no band. A plasma D2 occupancy relation, occupancy = 100% × C / (10.3 ng/mL + C), is attributed to Kapur and colleagues (*Am J Psychiatry* 1998;155:921-928), but their abstract reports occupancy only by dose, and this EC50 has **not yet been checked** against the paper. It is recorded with that caveat and not plotted.
