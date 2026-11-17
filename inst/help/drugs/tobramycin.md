### The model

Tobramycin's parameters come from Conil and colleagues (*Br J Clin Pharmacol* 2011;71:61-71), a population analysis of 49 adults in intensive care given 5 mg/kg over 30 minutes once a day. Two compartments: clearance 3.83 L/h at a Cockcroft-Gault creatinine clearance of 94 mL/min and a height of 172 cm, rising by 0.020 L/h for each mL/min of creatinine clearance and 0.052 L/h for each centimetre of height; central volume 25.5 L, intercompartmental clearance 4.74 L/h and peripheral volume 30.6 L.

### Covariates

The creatinine clearance is Cockcroft-Gault from the Patient Profile's **Serum creatinine** (blank means an assumed normal creatinine for age and sex). Tobramycin leaves unchanged in the urine, so clearance follows renal function closely. Because the covariates add, height is held at 150 cm or more and the creatinine clearance at zero or more, which keeps clearance positive. With [the scaling switch](help:models/fat-free-mass) on, the weight in Cockcroft-Gault and the volumes follow fat-free mass.

### Who it describes

These were critically ill patients, whose volumes are larger than a ward patient's (25.5 L against 16.6 L for [gentamicin](help:drugs/gentamicin)'s ward patients), so the peak after a dose is lower than it would be outside intensive care. The other tobramycin model read in full (Inclan 2005, general inpatients) put a non-renal intercept of 3 L/h on clearance, which an aminoglycoside cleared by filtration does not have, and was not used.

### The MIC line and the band

The line is free tobramycin at the CLSI (2023) and EUCAST susceptible breakpoint, **2 mg/L**; tobramycin is not bound in serum. The band is free drug at the expected MBC of the common pathogens: the Enterobacterales' MBC50 and MBC90 (0.5 and 4 mg/L) and *Pseudomonas aeruginosa*'s MBC90 (8 mg/L), with the MBC taken as the MIC, as Chamberland and colleagues found it to be in bloodstream isolates. See [Antibiotics](help:models/antibiotics). The ESICM/ESCMID position paper's targets are a peak of 8-10 times the MIC, an AUC of 80-120 mg·h/L, and a trough below 0.5 mg/L.

### Dose

Tobramycin sulfate is labelled by its tobramycin content (40 mg/mL), so doses are milligrams of tobramycin.
