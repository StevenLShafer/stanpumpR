### The model

Amikacin's parameters come from Tod and colleagues (*Antimicrob Agents Chemother* 1998;42:849-856), a population analysis of 278 serum samples from 57 febrile, severely neutropenic adults with haematological malignancy, given 7.5 mg/kg twice or 20 mg/kg once a day. Two compartments: clearance = 0.367 × creatinine clearance + 1.40 L/h (creatinine clearance by Cockcroft-Gault, in L/h), central volume 8.92 L, intercompartmental clearance 4.43 L/h and peripheral volume 11.4 L. The authors refined clearance further by sex and age, but that equation is printed as an image that could not be read unambiguously, so the first form they give is used. The kinetics were linear from 7.5 to 20 mg/kg.

### Covariates

The creatinine clearance is Cockcroft-Gault from the Patient Profile's **Serum creatinine** (blank means an assumed normal creatinine for age and sex). With [the scaling switch](help:models/fat-free-mass) on, the weight in it and the volumes follow fat-free mass.

### Who it describes

Neutropenic patients outside intensive care. Critically ill patients have larger volumes: in an ICU cohort (Boidin 2019) the median 1-hour peak after 23.4 mg/kg was 67 mg/L, where this model gives 113 mg/L for a patient of their median size and age.

### The MIC line and the band

The line is free amikacin at the CLSI (2023) susceptible breakpoint for Enterobacterales, **4 mg/L**, which on total drug is 4.9 mg/L (about 18% bound, Brunnemann and Segal 1991). The band is free drug at the expected MBC of the common pathogens: the Enterobacterales' MBC50 and MBC90 (2 and 4 mg/L) and *Pseudomonas aeruginosa*'s MBC90 (16 mg/L), the MBC taken as the MIC. See [Antibiotics](help:models/antibiotics). The ESICM/ESCMID position paper's targets are a peak of 8-10 times the MIC, an AUC of 80-120 mg·h/L, and a trough below 2.5 mg/L.

### Dose

Amikacin sulfate is labelled by its amikacin content (250 mg/mL), so doses are milligrams of amikacin.
