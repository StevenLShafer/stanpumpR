### The model

Digoxin's parameters come from Yukawa and colleagues in Fukuoka (*Clin Pharmacokinet* 2001;40:773-781): 246 routine steady-state serum concentrations from 106 Japanese patients with heart failure and atrial fibrillation taking digoxin tablets once a day, fitted in NONMEM with two compartments. Clearance is (0.036 × weight + 0.112 × creatinine clearance) L/h, the central and peripheral volumes 1.83 and 22.6 L/kg, and the intercompartmental clearance 0.629 L/h/kg. The paper's own table of clearances at 70 kg (13.72 L/h at a creatinine clearance of 100 mL/min, 3.08 at 5) and the daily doses that give an average of 1 mcg/L (329 and 74 mcg) are reproduced exactly.

### Oral daily rate only

Every dose in the study was a tablet, so every parameter is **apparent**, divided by an unknown bioavailability, and the study fitted no absorption at all. The model is therefore offered only as a constant oral daily rate, **mg/day PO**, as [amiodarone](help:drugs/amiodarone) is: it draws the average concentration the daily tablet produces and the week or more it takes to reach steady state, but not the rise and fall after each tablet. There is no intravenous unit, because an apparent model gives intravenous concentrations wrong by the unknown bioavailability. An intravenous loading dose needs an intravenous study, which is on the list of papers to obtain.

### Covariates

Weight, linear, on every parameter, and Cockcroft-Gault creatinine clearance on clearance, from the Patient Profile's **Serum creatinine** (an assumed normal creatinine for age and sex when blank). With [the scaling switch](help:models/fat-free-mass) on, both use the fat-free-mass weight. Spironolactone (23% lower clearance) and calcium antagonists (22% lower: diltiazem, nicardipine, nifedipine, verapamil) are not Patient Profile fields and are not applied; a patient taking either has higher concentrations than drawn.

### The band

The shaded band is the paper's therapeutic range, 0.5 to 2.0 mcg/L of serum. Current practice in heart failure aims for the lower half of it.

### Where to be careful

Japanese patients with a mean weight of 50 kg, sampled by immunoassay, which can cross-react with digoxin-like substances and with spironolactone's metabolites. The distribution volumes are poorly determined (the peripheral volume's confidence interval includes zero), so how fast the curve reaches steady state is less certain than where it ends. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
