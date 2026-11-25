### A component of Zerbaxa

Ceftolozane is given in the United States only as [Zerbaxa](help:drugs/Zerbaxa), with tazobactam. It cannot be entered in the dose table on its own: a Zerbaxa dose reaches this model as two thirds of the product's weight. The plotted concentration is total plasma ceftolozane.

### The model

The parameters come from the population model of Zhang and colleagues (*J Clin Pharmacol* 2021;61:254-268, doi 10.1002/jcph.1733). It was fitted to 968 adults from 16 studies: 277 healthy participants (including some with renal impairment and no infection), 176 patients with complicated urinary tract infection, 174 with complicated intra-abdominal infection, 331 with pneumonia, and 10 critically ill patients with other infections. They were 18 to 98 years old (mean 53) and weighed 34 to 173 kg (mean 75). The model has two compartments:

- **Clearance:** 4.84 L/h × (CrCL / 100)^0.701, with the creatinine clearance by Cockcroft-Gault in mL/min. Ceftolozane is cleared by glomerular filtration.
- **Central volume:** 9.23 L × (weight / 70)^0.684.
- **Intercompartmental clearance:** 3.13 L/h.
- **Peripheral volume:** 4.78 L × (weight / 70)^0.484.

The creatinine clearance uses the Patient Profile's **Serum creatinine**. A blank field means an assumed normal creatinine for age and sex. The model was fitted in adults. For a child, the creatinine is put on the adult scale.

### The infection

This is the first model in stanpumpR to read the Patient Profile's **Infection**. The infection changes the parameters as follows:

| Infection | Clearance | Central volume |
|---|---|---|
| None (healthy), the default | × 1 | × 1 |
| Complicated urinary tract infection | × 1.18 | × 1.25 |
| Complicated intra-abdominal infection | × 1.43 | × 1.59 |
| Pneumonia, ventilated or not | × 1 | × 2.00 |
| Other infection | × 1 | × 2.14 |

Every pneumonia patient in the source was mechanically ventilated, so for pneumonia without ventilation the curve is an extrapolation. The "other infection" factor comes from only 10 critically ill patients with augmented renal clearance. A patient given Zerbaxa is almost always infected, so with the default (*None*) the plot overstates the peak an infected patient reaches.

### Not modelled

- **End-stage renal disease and dialysis.** The source found that in end-stage renal disease clearance is a further 68% lower than its creatinine clearance predicts, and the central volume 30% larger. The Patient Profile has no field for dialysis, so these are not applied. The curve does not describe a patient on dialysis.
- **Tazobactam**, which is not drawn (see [Zerbaxa](help:drugs/Zerbaxa)).

### Checks

The label's numbers were not used in the fit.

| Quantity | Zerbaxa label | This model |
|---|---|---|
| Clearance, healthy adults after 1.5 g | 4.10 to 6.73 L/h | 5.29 L/h (35-year-old, 70-kg man) |
| Steady-state volume, healthy men | 13.5 L | 14.0 L |
| Peak at steady state, 1.5 g every 8 h, cIAI and cUTI | 65.7 mcg/mL | 64.5 mcg/mL (50-year-old, 75-kg man with cUTI) |
| AUC over 8 h at steady state, cIAI and cUTI | 186 mcg·h/mL | 174 mcg·h/mL (the same man) |

A patient with complicated intra-abdominal infection has a larger central volume and a faster clearance, so the peak and AUC are lower (52 mcg/mL and 144 mcg·h/mL).

### The MIC line and the band

The line is free ceftolozane at 4 mg/L, the susceptible breakpoint for *Pseudomonas aeruginosa* in both the CLSI and EUCAST guidelines. Ceftolozane is about 20% bound to plasma proteins (the label gives 16% to 21%), so the line is drawn at 5 mg/L of total ceftolozane. In 1,209 *P. aeruginosa* isolates from 35 US hospitals, half were inhibited at 0.5 mg/L and 90% at 2 mg/L.

No shaded band is drawn. stanpumpR's antibiotic band marks the expected bactericidal concentrations (MBC) of the common pathogens. No MBC distribution for ceftolozane against a population of isolates has been found. Time-kill studies of single strains show that ceftolozane kills *P. aeruginosa* at concentrations near its MIC.

### Pregnancy

There are no measured pharmacokinetics of ceftolozane in pregnancy. Ceftolozane is cleared by filtration, which rises about 50% in late pregnancy, so for a pregnant patient with a blank creatinine the creatinine clearance is raised by that amount.
