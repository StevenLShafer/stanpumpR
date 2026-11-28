### The product and the dose

Imipenem is always given with cilastatin, which blocks its breakdown in the kidney. Cilastatin has no antibacterial effect and is not drawn. Both US products state their doses in milligrams of imipenem:

- **PRIMAXIN** (imipenem and cilastatin): "500 mg" is 500 mg of imipenem with 500 mg of cilastatin. Adults with a creatinine clearance of 90 mL/min or more receive 500 mg every 6 hours, 1000 mg every 8 hours, or 1000 mg every 6 hours. Children from 3 months receive 15 to 25 mg/kg every 6 hours. The label reduces the dose below 90 mL/min.
- **RECARBRIO** (imipenem, cilastatin and relebactam): 1.25 g is 500 mg of imipenem, 500 mg of cilastatin and 250 mg of relebactam, every 6 hours over 30 minutes. Choose [RECARBRIO](help:drugs/RECARBRIO) in the dose table and enter it as ordered, in grams of the product (`1.25 g`, or the pediatric `37.5 mg/kg`); stanpumpR draws the imipenem it contains, 0.4 of each dose. Relebactam is a beta-lactamase inhibitor and is not drawn.

To enter a 30-minute infusion of 500 mg, enter `1000 mg/hr` at its start and `0 mg/hr` half an hour later, and repeat every 6 hours.

### The model

The parameters come from the population model of Patel and colleagues (*Clin Transl Sci* 2022;15:396-408, doi 10.1111/cts.13158). It was fitted to 1,197 adults from 11 studies of imipenem with relebactam: 231 healthy participants, 308 patients with complicated intra-abdominal infection, 380 with complicated urinary tract infection, and 278 with hospital-acquired or ventilator-associated pneumonia. They were 18 to 96 years old (median 55) and weighed 27 to 180 kg (median 75). The model has two compartments:

- **Clearance:** 12.68 L/h × (CrCl / 105.5)^0.48 × (weight / 75)^0.29, with the creatinine clearance by Cockcroft-Gault in mL/min.
- **Central volume:** 11.39 L × (weight / 75)^1.03.
- **Peripheral volume:** 7.79 L.
- **Intercompartmental clearance:** 23.07 L/h.

The creatinine clearance uses the Patient Profile's **Serum creatinine**. A blank field means an assumed normal creatinine for age and sex. The model was fitted in adults. For a child, the creatinine is put on the adult scale.

### The infection

The model reads the Patient Profile's **Infection**:

| Infection | Clearance | Central volume |
|---|---|---|
| None (healthy), the default | × 1 | × 1 |
| Complicated urinary tract infection | × 1 | × 1 |
| Complicated intra-abdominal infection | × 1 | × 1 |
| Pneumonia, not ventilated | × 0.62 | × 0.61 |
| Pneumonia, mechanically ventilated | × 0.62 | × 0.75 |
| Other infection | × 1 | × 1 |

The source's reference group was healthy participants and patients with urinary or intra-abdominal infection together, so those three categories give the same curve. It fitted no other infection, so *Other* gives the reference curve too. In pneumonia clearance is lower, so exposure is higher than the default (*None*) shows: the source's healthy participants had 0.62 times the AUC of its pneumonia patients.

### Not modelled

- **Dialysis.** The source had few patients with end-stage renal disease (0.4%). The curve does not describe a patient on dialysis.
- **Cilastatin and relebactam**, which are not drawn.

### Checks

The label's numbers were not used in the fit. The RECARBRIO label's Table 7 gives the source model's own steady-state means in patients with a creatinine clearance of 90 mL/min or more, after 500 mg every 6 hours over 30 minutes. The model below is for a 50-year-old, 75-kg man with the assumed creatinine.

| Quantity | RECARBRIO label | This model |
|---|---|---|
| AUC over 24 h, cUTI and cIAI | 171 mcg·h/mL (570.6 µM·h) | 163 mcg·h/mL |
| Peak, cUTI and cIAI | 34.8 mcg/mL (116.1 µM) | 27.0 mcg/mL |
| AUC over 24 h, pneumonia | 231 mcg·h/mL (771 µM·h) | 262 mcg·h/mL |
| Peak, pneumonia | 36.7 mcg/mL (122.7 µM) | 38.9 mcg/mL (not ventilated), 35.6 mcg/mL (ventilated) |
| Steady-state volume | 24.3 L (subjects) | 18.4 L (35-year-old, 70-kg man) |

### The MIC line and the band

The line is free imipenem at 2 mg/L, the imipenem/relebactam breakpoint at which the source assessed its target (free imipenem above the MIC for 30% of the dosing interval). Imipenem is about 20% bound to plasma proteins, so the line is drawn at 2.5 mg/L of total imipenem.

No shaded band is drawn. stanpumpR's antibiotic band marks the expected bactericidal concentrations (MBC) of the common pathogens, and no imipenem MBC source has been read.

### Pregnancy

One small study (Heikkilä 1992) found imipenem's clearance per kilogram 1.85 times higher at term than in non-pregnant women, and its volume of distribution 1.79 times larger. Only its abstract has been read, so its factors are not applied. Instead, for a pregnant patient with a blank creatinine, the creatinine clearance is raised by the 50% rise in filtration of late pregnancy, which raises clearance about 1.21-fold.
