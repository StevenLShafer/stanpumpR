### The model

Piperacillin's parameters come from Kong and colleagues (*Clin Pharmacokinet* 2025;64:107-126, doi 10.1007/s40262-024-01460-6). They pooled 13 studies of piperacillin-tazobactam in 415 patients, from premature neonates to 94 years old. The adults were all critically ill or febrile and neutropenic. The model has two compartments, and at the reference patient (70 kg, 35 years, a creatinine at the normal for age) its values are:

| Parameter | Value |
|---|---|
| Clearance | 10.6 L/h |
| Central volume | 10.4 L |
| Peripheral volume | 11.6 L |
| Intercompartmental clearance | 15.2 L/h |

- **Weight:** the volumes scale with weight and the clearances with weight to the 0.75 power.
- **Age:** clearance matures through infancy (half-mature at 54 weeks postmenstrual age) and declines in old age (to half at 89 years).
- **Creatinine:** clearance falls exponentially with serum creatinine above the normal for age, by a factor of exp(−0.346) for each mg/dL.

The equations were read in the paper's own NONMEM code.

### Checks

| Source | Measured | This model (25 years, 75 kg, creatinine 0.9) |
|---|---|---|
| Zosyn label, healthy subjects, 4 g piperacillin over 30 min: clearance | 12.6 L/h | 11.2 L/h (11% lower) |
| Same: area | 322 mg·h/L | 356 mg·h/L |
| Same: peak | 298 mg/L | 221 mg/L |
| Same: half-life | 0.84 h | 1.8 h (terminal) |

Clearance agrees with the label. The larger volumes and longer terminal phase are what the critically ill patients and the later sampling of the pooled studies give, and they lower the peak after a short infusion.

### Covariates

The creatinine is the Patient Profile's **Serum creatinine**, as the patient's own value (children's against a normal for their age, as the source fitted it). A blank field means an assumed normal creatinine for age and sex. **Gestational age at birth** (blank is term) sets the postmenstrual age for an infant. With [the scaling switch](help:models/fat-free-mass) on, the weight is the pharmacokinetic weight; with it off, total body weight.

The creatinine term has no non-renal floor: at 5 mg/dL it leaves a quarter of normal clearance. The source's adults had creatinines up to 5.82 mg/dL, so higher values are held at 5.82. Even within that range the model probably predicts too little clearance in severe renal failure. The label says only that the half-life doubles below a creatinine clearance of 20 mL/min, about half of normal clearance where this model gives a quarter.

### The MIC line and the band

The line is free piperacillin at the EUCAST susceptible breakpoint for Enterobacterales, **8 mg/L** (with tazobactam fixed at 4 mg/L). Piperacillin is about 30% bound (label), so the line sits at 11.4 mg/L of total drug.

The band is free drug at the expected MBC of the common pathogens:

- **Lower edge:** the Enterobacterales' MBC50, 2 mg/L. Their MIC50 and MIC90 are 2 and 16 mg/L in 9,809 US isolates (Sader 2023), and their MBC was taken as the MIC because Reimer and colleagues (1980) found MBC-to-MIC ratios below 2.
- **Typical:** the Enterobacterales' MBC90, 16 mg/L.
- **Upper edge:** *Pseudomonas aeruginosa*'s measured MBC90, 32 mg/L (Corrado and colleagues, 1980). Modern isolates are less susceptible than those of 1980.

On the total-drug curve the edges are 2.9, 23 and 46 mg/L. See [Antibiotics](help:models/antibiotics).

There is no consensus therapeutic range. The ESICM/ESCMID position paper (Abdul-Aziz and colleagues, *Intensive Care Med* 2020;46:1127-1153, doi 10.1007/s00134-020-06050-1) recommends monitoring the beta-lactams to a target of 100% of the dosing interval with free drug above the MIC. Its sample is a trough "30 min or just before the next dosing", 24–48 hours into therapy, or a steady-state concentration during a continuous infusion. Its Table 1 gives a penicillin toxicity threshold of a trough above 361 mg/L, from piperacillin data.

### Dose

Zosyn is labelled and ordered by its combined weight: "4.5 g" is piperacillin sodium equivalent to 4 g of piperacillin plus tazobactam sodium equivalent to 0.5 g of tazobactam. A dose entered in **mg pip-tazo** (the default) is therefore taken as 4/4.5 piperacillin. A dose in plain **mg** is piperacillin alone. Tazobactam is not plotted.
