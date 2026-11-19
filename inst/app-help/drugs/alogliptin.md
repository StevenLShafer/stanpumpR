### The model

Naik and colleagues' population analysis of alogliptin (*Br J Clin Pharmacol* 2016): 491 subjects in three studies. Healthy volunteers took single and repeated doses of 12.5 to 400 mg; a renal-impairment study gave 50 mg to healthy volunteers and to people with mild, moderate and severe impairment and end-stage renal disease; and 398 adults with type 2 diabetes took 12.5 or 25 mg daily in a phase 3 trial.

- **Disposition:** two compartments. Apparent clearance 17.1 L/h, central volume 194 L, peripheral volume 149 L, intercompartmental clearance 10.2 L/h. The last two have no size covariate in the paper: with **Adjust weight to fat-free mass** ticked they take the library's fat-free-mass scaling, unticked they are the published values.
- **Absorption:** first order, 2.50/h, no lag; a 25 mg dose peaks in about 1 to 2 hours. The parameters are apparent ones (divided by the bioavailability).

### Covariates

- **Renal function:** clearance × (creatinine clearance/72.95)<sup>0.534</sup>. About three quarters of a dose leaves unchanged in the urine. The paper predicts a clearance of 20.2, 16.7, 13.2 and 4.1 L/h at creatinine clearances of 100, 70, 45 and 15 mL/min, which the model reproduces.
- **Severe renal impairment:** below a creatinine clearance of 30 mL/min the paper fitted its own coefficient, 9.61 L/h in place of 17.1, "to account for the potential changes in non renal clearance". The clearance therefore falls by 44% at 30 mL/min. That step is the published model and is kept as published rather than smoothed.
- **Weight:** clearance × (weight/84.9)<sup>0.345</sup> and central volume × (weight/84.9)<sup>1.11</sup>, at the pharmacokinetic weight with **Adjust weight to fat-free mass** ticked and at total weight with it unticked.

Age, sex, race and the medicines taken with it were tested and not kept.

The creatinine clearance is Cockcroft-Gault, from the entered serum creatinine or an [assumed normal](help:models/covariates) one. Naik does not name the equation; the FDA's Clinical Pharmacology reviews of Nesina do. Its renal-impairment study grouped subjects by the Cockcroft-Gault formula on total body weight. Its population analysis of the phase 3 trial, whose median of 72.95 mL/min is the one Naik normalises to, computed creatinine clearance from serum creatinine, weight, age and sex.

### Doses

`mg PO` and `mg PO qd`, in mg of alogliptin, as Nesina's strengths (6.25, 12.5 and 25 mg) are labelled; the tablet carries the benzoate. The label halves the dose below a creatinine clearance of 60 mL/min and quarters it below 30 mL/min.

### What is not plotted

Concentration only, in ng/mL. The inhibition of plasma DPP-4 is not drawn. See [Antidiabetic models and their limits](help:models/antidiabetics).
