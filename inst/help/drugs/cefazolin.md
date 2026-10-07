### The model

Cefazolin's parameters are from Komatsu and colleagues (*Antimicrob Agents Chemother* 2024;68:e00267-24), who fitted total and unbound serum concentrations together in 152 adults having prostatectomy or nephrectomy. Their disposition model is written on **unbound** drug: a two-compartment system whose clearance acts on free cefazolin (CLu 29.3 L/h at a creatinine clearance of 70 mL/min, Vcu 36.6 L, Qu 54.4 L/h, Vpu 42.2 L), with total concentration recovered afterwards through saturable albumin binding.

### What is plotted

stanpumpR's engine is linear, so it simulates the linear half of that model and **plots unbound cefazolin**. That is the concentration the pharmacodynamic target is written on: the fraction of the dosing interval that free drug spends above the organism's MIC. A laboratory reports total cefazolin, which at the concentrations a 2 g dose produces is two to four times the unbound value, the ratio falling as the level rises because binding saturates. The shaded band (0.5 to 2 mcg/mL unbound) marks the MIC scenarios of 0.5 and 1 mg/L the source examined and 2 mg/L, the upper end of the MIC distribution of wild-type *Staphylococcus aureus* (its epidemiological cut-off) and the MIC90 of methicillin-susceptible strains. There is no longer a cefazolin breakpoint for staphylococci: susceptibility is inferred from oxacillin or cefoxitin.

### Covariates

Clearance follows Cockcroft-Gault creatinine clearance to the power 0.586. Creatinine clearance comes from the **Serum creatinine** in the Patient Profile; left blank, it is estimated at an **assumed normal creatinine** (1.0 mg/dL in men, 0.8 in women), when the fall of renal function with age and the sex difference are represented but **renal impairment is not**. The volumes and the intercompartmental clearance carry no size term in the source and take the default [fat-free-mass scaling](help:models/fat-free-mass); the renal term sees the pharmacokinetic weight with the switch on and total weight with it off.

### Effect site

None. An antibiotic's effect is its exposure relative to the MIC, and there is nothing for the engine to equilibrate it into; only the plasma concentration is plotted. *Time until threshold* is therefore timed on the plasma curve, which for cefazolin is **unbound** (free) drug, so the threshold is the MIC itself: the line shows how long, with no further dose, until free cefazolin falls below the MIC. The MIC is **2 mg/L**, for methicillin-susceptible *S. aureus*; it is also the CLSI susceptible breakpoint for *E. coli* and the other Enterobacterales. See *Time until threshold: free drug at the MIC* above.

### Where to be careful

A patient with a raised creatinine is simulated as if the creatinine were normal, so the curve declines too fast for them. Albumin, which sets the binding capacity, plays no part in the unbound curve. The model was fitted in surgical prophylaxis with 15-minute infusions; it has not been validated for prolonged treatment courses.
