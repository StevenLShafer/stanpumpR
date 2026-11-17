### The model

Tranexamic acid's parameters come from Lanoiselée and colleagues (*Br J Clin Pharmacol* 2018;84:310-319), a population analysis of 827 plasma concentrations from 166 adults having total hip arthroplasty at Saint-Étienne (age 24 to 94, weight 37 to 125 kg, creatinine clearance 15 to 204 mL/min), each given 1 g as an intravenous bolus after induction, then 1 g over 8 hours or placebo. Two compartments: clearance 5.32 L/h at a creatinine clearance of 85 mL/min, central volume 6.09 L at 70 kg, intercompartmental clearance 23.3 L/h and peripheral volume 9.55 L. The terminal half-life is about 2 hours.

### Covariates

**Renal function**: clearance is 5.32 × (creatinine clearance / 85)^0.548 L/h, the creatinine clearance by Cockcroft-Gault from the Patient Profile's **Serum creatinine** (blank means an assumed normal creatinine for age and sex). Tranexamic acid is excreted unchanged in the urine, so in renal failure the concentration stays above any threshold for much longer; that is why the label reduces the dose by serum creatinine. **Weight** is on the central volume, so a heavier patient has a lower peak after the same dose. With [the scaling switch](help:models/fat-free-mass) on, both use the fat-free-mass weight.

### The threshold

No therapeutic range is established. The time until threshold reads **10 mg/L**, the concentration that inhibits fibrinolysis by about 80% in vitro and that the source calls the therapeutic target, so the plot gives how long the concentration stays above it. A systematic review (Picetti and colleagues, *Blood Coagul Fibrinolysis* 2019;30:1-10) found that 10 to 15 mg/L "resulted in substantial inhibition of fibrinolysis, although concentrations between 5 and 10 mg/l were partly inhibitory". In this study every patient was above 10 mg/L at the end of surgery after 1 g, but keeping the concentration above it for longer (up to 16 hours with the infusion) did not reduce blood loss. No concentration at which seizures begin has been established; they are associated with the high doses used in cardiac surgery.

### Where to be careful

- **Cardiopulmonary bypass** is not modelled: these patients were not on bypass, and haemodilution, hypothermia and the circuit change tranexamic acid's kinetics (Dowd and colleagues, *Anesthesiology* 2002, found its elimination slower during bypass).
- Only the **intravenous** route is offered; no adult oral or intramuscular absorption model could be read in full.
- **Pregnancy**: with the Pregnant box ticked and the creatinine left blank, the creatinine clearance is raised by half, the rise in glomerular filtration of late pregnancy, which raises clearance by 25%. Women at caesarean delivery cleared somewhat more than that (8.6 to 9.4 L/h).
