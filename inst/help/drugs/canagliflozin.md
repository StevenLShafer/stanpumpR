### The model

Hoeben and colleagues' population analysis of canagliflozin (*Clin Pharmacokinet* 2016): 9061 concentrations from 1616 healthy volunteers and patients with type 2 diabetes in 14 studies, including people with renal impairment, at doses of 25 to 1600 mg. The absorption and distribution were fitted to the 245 richly sampled phase 1 subjects and then fixed, and the elimination was fitted to everyone.

- **Disposition:** two compartments. Apparent central volume 99.3 L in men and 82.6 L in women, elimination rate constant 0.145/h, so an apparent clearance of 14.4 L/h in men and 12.0 L/h in women. Distribution rate constants 0.101/h out of the central compartment and 0.0856/h back.
- **Absorption:** after a lag of 0.147 hours, each tablet is released into the gut at a constant rate over 0.604 hours and absorbed from it first order at 3.68/h (sequential zero- and first-order absorption).

### Covariates

- **Renal function:** elimination rate × (eGFR/87)<sup>0.261</sup>. The eGFR is the MDRD equation, as in the paper, from the entered serum creatinine or an [assumed normal](help:models/covariates) one. The data included severe impairment (eGFR 15 to 29).
- **Sex:** the central volume above, and so the clearance.
- **Weight:** central volume × (weight/75)<sup>0.583</sup>. With **Adjust weight to fat-free mass** ticked, the pharmacokinetic weight is used.
- **Age:** central volume × (age/45)<sup>−0.167</sup>, from 18 years.
- **Body mass index:** absorption rate × (BMI/26)<sup>−1.41</sup> and lag × (BMI/26)<sup>1.41</sup>: a heavier patient absorbs later and more slowly. The paper prints one exponent for both; its text says absorption is faster and the lag shorter at a lower BMI.

The clearance is the central volume times the elimination rate, so weight, age and sex change it as well.

### What had to be assumed

The paper divides each covariate by its median and prints the medians of eGFR (87) and age (45), but not of weight, body mass index or dose. Its supplement, which holds the demographics, was not available. A BMI of 26 lies between the phase 1 tertiles (24.2 and 28.0), and 75 kg is that BMI at the population's height; an error of 5 kg would change the clearance by 4%. The paper's small effect of the daily dose on elimination is not applied: exposure at 300 mg a day is 7% above that at 100 mg (the paper's Table 6).

At 300 mg once daily, the steady-state exposure of a 60-year-old man and woman (90 and 78 kg, eGFR 80) averages 23,200 ng·h/mL over a day; the paper predicts 24,941 for its typical patient with type 2 diabetes. A single 100 mg dose peaks 1.3 hours after it is taken. The terminal half-life of the model, 16 hours, is longer than the label's 10.6 to 13.1 hours, which come from the first days after a dose.

### What is not plotted

Concentration only, in ng/mL: not urinary glucose excretion or HbA1c (de Winter and colleagues built an HbA1c model on this pharmacokinetic model). See [Antidiabetic models and their limits](help:models/antidiabetics).
