### A component of Avycaz

In stanpumpR ceftazidime is drawn for [Avycaz](help:drugs/Avycaz), ceftazidime with avibactam. It cannot be entered in the dose table on its own: an Avycaz dose reaches this model as 80% of the product's weight. The plotted concentration is total plasma ceftazidime. Plain ceftazidime (Fortaz, Tazicef) is not offered yet.

### The model

The parameters come from the population model of Franzese and colleagues (*Clin Pharmacol Ther* 2022;111:635-645, doi 10.1002/cpt.2460), from the control stream and Table S2 of its supplement. It was fitted to adults from 18 phase I to III studies and 154 children 3 months to 17 years old together: 9,628 ceftazidime concentrations from 2,130 people. The model has two compartments:

- **Clearance:** 7.75 L/h × (weight / 70) × (53.5 + 70) / (53.5 + weight) × the renal term. The weight term rises with weight but levels off: it is 1 at 70 kg and approaches 1.76 in very large patients.
- **Renal term:** 0.0103 × NCrCL below 100 mL/min/1.73 m², and 1.03 + 0.00125 × (NCrCL − 100) above it, with NCrCL held at 150. NCrCL is the creatinine clearance normalised to 1.73 m²: Cockcroft-Gault × 1.73 / body surface area (Mosteller) from 18 years, and the bedside Schwartz equation (0.413 × height / creatinine) from 2 to 18 years.
- **Children 2 years old and younger:** the renal term is replaced by kidney maturation, PMA^3.4 / (47.7^3.4 + PMA^3.4), with the postmenstrual age (PMA) in weeks. PMA is the Patient Profile's **Gestational age at birth** (40 weeks when blank) plus the age since birth.
- **Central volume:** 11.2 L × weight / 70.
- **Intercompartmental clearance:** 5.33 L/h × (weight / 70)^0.67.
- **Peripheral volume:** 6.52 L × weight / 70.

The renal term uses the Patient Profile's **Serum creatinine**. A blank field means an assumed normal creatinine for age and sex. The source changes equation at 2 years and at 18, and the curve steps there. At 18 the step can be large: a 60-kg, 170-cm young man goes from 6.6 to 7.8 L/h, because the Schwartz equation reads a lower filtration than Cockcroft-Gault. The source studied children from 3 months, so a younger infant's curve is an extrapolation.

### The infection

The infection changes the parameters as follows:

| Infection | Clearance | Central volume |
|---|---|---|
| None (healthy), the default | × 1 | × 1 |
| Complicated urinary tract infection | × 1 | × 1.49 |
| Complicated intra-abdominal infection | × 1.33 | × 1.83 |
| Pneumonia, not ventilated | × 1.10 | × 1.83 |
| Pneumonia, mechanically ventilated | × 1.10 | × 2.20 |
| Other infection | × 1 | × 1 |

The source fitted no other infection, so *Other* gives the healthy curve. A patient given Avycaz is almost always infected, so with the default (*None*) the plot overstates the peak an infected patient reaches.

### Not modelled

- **Race.** The source found clearance 14% lower in non-Chinese, non-Japanese Asian patients and 8% lower in Chinese patients, and the central volume 14% smaller in Asian patients. The Patient Profile has no field for race.
- **Dialysis.** The source's renal term goes to zero with the creatinine clearance, and its authors say the model's non-renal clearance is close to zero. The curve does not describe a patient on dialysis.
- **Avibactam**, which is not drawn (see [Avycaz](help:drugs/Avycaz)).

### Checks

The label's numbers were not used in the fit.

| Quantity | Avycaz label or source | This model |
|---|---|---|
| Clearance, healthy men, 2.5 g every 8 h | 6.86 L/h (label) | 8.06 L/h (35-year-old, 70-kg man) |
| Steady-state volume, healthy men | 17 L (label) | 17.7 L |
| Peak at steady state, healthy men | 90.4 mcg/mL (label) | 82.8 mcg/mL (50-year-old, 75-kg man) |
| Peak at steady state, adult phase III cUTI | 80.7 mcg/mL (geometric mean, source) | 71.9 mcg/mL (the same man with cUTI) |

The model's healthy clearance is about 17% above the label's. The source's table of exposures in its first publication had the ceftazidime and avibactam columns swapped; the numbers above are from the published correction (*Clin Pharmacol Ther* 2024;115:373).

### The MIC line and the band

The line is free ceftazidime at 8 mg/L. This is the susceptible breakpoint of ceftazidime-avibactam for Enterobacterales and *Pseudomonas aeruginosa*, set by the FDA, CLSI and EUCAST, and it is the source's own target (free ceftazidime above 8 mg/L for half the dosing interval). Ceftazidime is less than 10% bound to plasma proteins, so the line is drawn at 8.9 mg/L of total ceftazidime.

No shaded band is drawn. stanpumpR's antibiotic band marks the expected bactericidal concentrations (MBC) of the common pathogens, and no MBC distribution for ceftazidime with avibactam has been read.

### Pregnancy

There is no population model or paired study of ceftazidime in pregnancy. One early study found serum levels in mid-pregnancy about half of those expected in non-pregnant people. Ceftazidime is cleared by filtration, which rises about 50% in late pregnancy, so for a pregnant patient with a blank creatinine the normalised creatinine clearance is raised by that amount. Above 100 mL/min/1.73 m² the renal term is nearly flat, so this changes the curve little.
