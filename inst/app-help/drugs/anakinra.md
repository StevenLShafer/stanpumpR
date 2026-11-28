### The model

Urien and colleagues' population analysis (*BMC Pharmacol Toxicol* 2013) of 87 children and adolescents, aged 8 months to 21 years and weighing 4.3 to 83 kg, with systemic-onset juvenile idiopathic arthritis or an autoinflammatory syndrome, given 2 to 10 mg/kg (at most 100 mg) by subcutaneous injection once a day. One compartment with first-order absorption, all parameters apparent (divided by the unknown bioavailability):

- clearance 0.847 × weight^0.47 L/h (6.24 L/h at 70 kg);
- volume 2.581 × weight^0.76 L (65.2 L at 70 kg);
- absorption rate constant 0.38 /h.

At 70 kg the elimination half-life is 7.2 hours and the concentration peaks 4.8 hours after a single dose.

### Who it describes

For subcutaneous doses this is the only population model of anakinra the library has, and it was fitted in children. The program uses it at every age, so for an adult it is an extrapolation by weight. Against the label's adults with rheumatoid arthritis (peak 3 to 7 hours after a dose, terminal half-life 4 to 6 hours) the peak time agrees and the half-life is longer. Healthy adults in Yang 2003 had an apparent clearance of about 170 mL/min, 1.6 times this model's 104 mL/min at 70 kg, so the curve may run high in an adult. In the label's children with NOMID on a median 3 mg/kg a day, the model's steady-state trough (140 to 220 ng/mL at 20 to 30 kg) agrees with the measured median of 203 ng/mL, while its peak (1,400 to 1,700 ng/mL) is about half the measured median of 3,628 ng/mL, inside its range.

### The intravenous route

Intravenous doses run on a model of their own (Steven L. Shafer, 2026-11-26): Ogungbenro and colleagues' population analysis (*J Pharmacokinet Pharmacodyn* 2016) of 31 adults with **subarachnoid haemorrhage**, not sepsis, given an intravenous bolus of 100 to 500 mg with an infusion of 1 to 10 mg/kg/hour. Two compartments, with no covariate:

| | |
|---|---|
| Clearance | 7.86 L/h |
| Central volume | 5.74 L |
| Intercompartmental clearance | 0.65 L/h |
| Peripheral volume | 3.56 L |

These values belong to the source's own patients. Galea 2011 describes 25 of them: 7 men and 18 women, mean 53 years and 73 kg. A patient like them receives the published values, and other patients are scaled by fat-free mass; with the switch off, everyone receives the published values. For the 70 kg, 35-year-old reference man that gives a clearance of 8.7 L/h, half-lives of 29 minutes and 4.3 hours, and a steady state of about 16,000 ng/mL after 100 mg then 2 mg/kg/hour. Yang 2003's 12 healthy men, given 1 mg/kg intravenously and not part of the fit, had a clearance of 137 mL/min (8.2 L/h), within 6% of this model's.

**In sepsis the concentration may run higher than drawn.** Fisher 1994 gave 99 adults with sepsis syndrome 100 mg, then 17, 67 or 133 mg/hour for 72 hours. Their mean concentrations during the infusion, 3,540, 14,800 and 25,200 ng/mL, give a clearance of 4.5 to 5.3 L/hour, about 0.55 of this model's; a fifth of the treated patients had acute renal dysfunction at entry. Their clearance rose with creatinine clearance, but the paper prints no equation, so no renal term is applied.

The two routes run as separate systems on separate models, and their concentrations add. A dose by one route does not change the other route's model. The two populations differ: Urien's apparent clearance at 70 kg, 6.24 L/h, is CL/F; multiplied by the label's subcutaneous bioavailability of 95%, it gives a systemic clearance of 5.93 L/h, a third below the intravenous model's 8.7 L/h for the same man. Neither model carries renal function, which sepsis often impairs; the curve is that of a patient with normal renal function.

### Covariates

- **Weight**, on clearance and volume, as fitted (subcutaneous). With the fat-free-mass switch on, the weight in these terms is the pharmacokinetic weight. The intravenous model is scaled to fat-free mass (above).
- **Renal function** is not in the model. Anakinra is cleared mainly by the kidney: its apparent clearance is 16%, 50%, 70% and 75% lower in mild, moderate and severe renal impairment and end-stage renal disease (Yang 2003; the label, 12.3), and the label suggests 100 mg every other day below a creatinine clearance of 30 mL/min. The curve is that of a patient with normal renal function, and will run low for a patient with renal impairment.
- **Pregnancy**: no anakinra concentrations have been measured in pregnancy, and nothing is applied.

### Product and doses

Kineret, `mg SC` or `mg/kg SC`, once (`qd`) or twice (`bid`) a day. The label doses 100 mg a day for rheumatoid arthritis and 1 to 2 mg/kg a day, up to 8 mg/kg, for NOMID and DIRA.

Intravenously, `mg` or `mg/kg` as a dose and `mg/hr` or `mg/kg/hr` as an infusion, on the intravenous model below. The label does not give anakinra intravenously; it is given so, at higher doses, in sepsis with macrophage-activation-like features and in hyperinflammation.

### What is not plotted

Concentration only, in ng/mL. The ELISA also reads the patient's own interleukin-1 receptor antagonist, which is not added. No C-reactive protein or clinical endpoint is drawn. No therapeutic range is established; Urien's dosing target, a mean steady-state concentration of 400 ng/mL chosen from simulated C-reactive protein curves in 22 children, is recorded but not drawn.
