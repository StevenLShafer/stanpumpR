### The model

Urien and colleagues' population analysis (*BMC Pharmacol Toxicol* 2013) of 87 children and adolescents, aged 8 months to 21 years and weighing 4.3 to 83 kg, with systemic-onset juvenile idiopathic arthritis or an autoinflammatory syndrome, given 2 to 10 mg/kg (at most 100 mg) by subcutaneous injection once a day. One compartment with first-order absorption, all parameters apparent (divided by the unknown bioavailability):

- clearance 0.847 × weight^0.47 L/h (6.24 L/h at 70 kg);
- volume 2.581 × weight^0.76 L (65.2 L at 70 kg);
- absorption rate constant 0.38 /h.

At 70 kg the elimination half-life is 7.2 hours and the concentration peaks 4.8 hours after a single dose.

### Who it describes

This is the only population model of anakinra the library has, and it was fitted in children. The program uses it at every age, so for an adult it is an extrapolation by weight. Against the label's adults with rheumatoid arthritis (peak 3 to 7 hours after a dose, terminal half-life 4 to 6 hours) the peak time agrees and the half-life is longer. Healthy adults in Yang 2003 had an apparent clearance of about 170 mL/min, 1.6 times this model's 104 mL/min at 70 kg, so the curve may run high in an adult. In the label's children with NOMID on a median 3 mg/kg a day, the model's steady-state trough (140 to 220 ng/mL at 20 to 30 kg) agrees with the measured median of 203 ng/mL, while its peak (1,400 to 1,700 ng/mL) is about half the measured median of 3,628 ng/mL, inside its range.

### Covariates

- **Weight**, on clearance and volume, as fitted. With the fat-free-mass switch on, the weight in these terms is the pharmacokinetic weight.
- **Renal function** is not in the model. Anakinra is cleared mainly by the kidney: its apparent clearance is 16%, 50%, 70% and 75% lower in mild, moderate and severe renal impairment and end-stage renal disease (Yang 2003; the label, 12.3), and the label suggests 100 mg every other day below a creatinine clearance of 30 mL/min. The curve is that of a patient with normal renal function, and will run low for a patient with renal impairment.
- **Pregnancy**: no anakinra concentrations have been measured in pregnancy, and nothing is applied.

### Product and doses

Kineret, `mg SC` or `mg/kg SC`, once (`qd`) or twice (`bid`) a day. The label doses 100 mg a day for rheumatoid arthritis and 1 to 2 mg/kg a day, up to 8 mg/kg, for NOMID and DIRA. Anakinra is also given intravenously, at higher doses, in sepsis with macrophage-activation-like features and in hyperinflammation; that route is not offered, because the model was fitted to subcutaneous doses.

### What is not plotted

Concentration only, in ng/mL. The ELISA also reads the patient's own interleukin-1 receptor antagonist, which is not added. No C-reactive protein or clinical endpoint is drawn. No therapeutic range is established; Urien's dosing target, a mean steady-state concentration of 400 ng/mL chosen from simulated C-reactive protein curves in 22 children, is recorded but not drawn.
