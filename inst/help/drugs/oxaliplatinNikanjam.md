### The model

This second oxaliplatin model comes from Nikanjam and colleagues (*Cancer Chemother Pharmacol* 2015;75:495-503): 186 patients aged 0.6 to 86 years, weighing 7 to 165 kg, pooled from three paediatric trials and the National Cancer Institute's studies of oxaliplatin in renal and hepatic impairment. Doses were 60 to 160 mg/m² over 2 hours, and plasma ultrafiltrate platinum was followed for up to 3 weeks. Three compartments describe it, every parameter scaled to weight with no reference weight:

- clearance 0.67 × weight^0.75 × (0.6 / serum creatinine)^0.67 L/h, creatinine in mg/dL;
- intercompartmental clearances 0.22 × weight^0.75 (slow) and 3.17 × weight^0.75 (fast) L/h;
- central volume 0.39 × weight × (age / 21)^0.16 L, peripheral volumes 6.22 × weight and 16.9 × weight L.

The equation printed in the paper omits the 0.67 in the clearance; every number the paper reports needs it. At the cohort's medians (59 kg, creatinine 0.8 mg/dL, 20 years) the model gives a clearance of 11.8 L/h, a steady-state volume of 1387 L, and half-lives of 0.18, 19 and 214 hours, the values Deyme and colleagues tabulate for it in their 2018 review of FOLFIRINOX models. For a 70-kg adult given 130 mg/m² over 2 hours the curve is 1455 ng/mL at the end of the infusion, 12 ng/mL at one week and 3.8 ng/mL at three weeks, as the paper's Figure 1 shows.

### What is plotted

Ultrafilterable plasma platinum, in ng/mL, with doses entered in mg of oxaliplatin. The paper fitted platinum concentrations against oxaliplatin doses with no conversion between them, so its volumes and clearances mix the two (about twice a true platinum volume; platinum is 49% of oxaliplatin's mass). Ultrafilterable platinum is not all intact oxaliplatin, and the very long tail is largely platinum bound to small molecules, so the late curve overstates the active drug. There is no effect site and no therapeutic range: the paper discusses exposure (AUC), not a concentration band.

### Covariates

- **Weight**, on every parameter. With the fat-free-mass switch on, the weight used is the patient's pharmacokinetic weight.
- **Serum creatinine**, the patient's own, on clearance; the source used children's creatinine as measured. A blank field is the library's assumed normal creatinine for the patient's age and sex. Below 0.2 mg/dL, the lowest studied, it is taken as 0.2; above 3.4 mg/dL the model is extrapolated.
- **Age**, on the central volume only, held at 0.6 years or above, the youngest studied.
- Sex, hepatic function and race were tested and had no effect.

### How it compares with Bastian 2003

The library's other oxaliplatin model, [Bastian 2003](help:drugs/oxaliplatin), is fitted in adults over 28 hours with a gallbladder store, and plots in mcg/mL. Nikanjam's model has no enterohepatic term but follows the drug for three weeks and covers children. For an adult the two agree on the peak (about 1.4 to 1.5 mcg/mL after 130 mg/m² over 2 hours) and differ in the tail, where Nikanjam's sampling is far longer.

### Where to be careful

- **Not in pregnancy:** no pregnancy pharmacokinetics are known, and nothing is adjusted.
- **Not a monitoring tool:** the curve is a population prediction, not an estimate from measured levels.

Every parameter was read on the page images of the paper's PDF. See [chemotherapy](help:models/chemotherapy) for the other models considered.
