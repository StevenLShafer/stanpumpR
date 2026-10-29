### The model

Cefalexin's parameters are from Haynes and colleagues (*Antimicrob Agents Chemother* 2024;68:e00182-24), who fitted oral cefalexin (capsules and suspension) in **15 children** with musculoskeletal infection: CL/F 13.98 L/h and V/F 26.63 L at 70 kg, one compartment, absorption rate constant 1.79/h with no lag. Half-time at 70 kg is 1.3 hours (the source's 1.10 hours is for its children).

### Oral only

The parameters are **apparent**: clearance and volume divided by an unmeasured bioavailability. They predict oral concentrations correctly because the bioavailability cancels, and would predict intravenous ones wrong by one over that bioavailability, so cefalexin is offered by mouth only (**mg PO** or **mg/kg PO**, single or on a daily schedule) and bioavailability is carried as 1. There is no intravenous cefalexin product in any case.

### A pediatric model used in adults

The 70 kg normalisation is a convention of the fit, not evidence that the model describes adults. Ryder and colleagues (*Pharmacotherapy* 2026;46(7):e70179) used this vector to simulate adults, scaling clearance and volume linearly with weight, supplemented it with oral data from published adult studies, and judged it fit for that purpose. That is precedent for the extrapolation and a check against adult averages, but not a validation in adults.

### Covariates

Weight, as allometry: volumes linear, clearance to the 0.75 power, on the fat-free-mass ratio by default and on total weight with the switch off. No renal covariate was fitted (the children had normal kidneys), so the model does not read the Patient Profile's serum creatinine, and the weight term does not stand in for it.

### Effect site

None; only the plasma concentration is plotted. The shaded band (2.4 to 4.7 mcg/mL total) marks free cefalexin at the expected **MBC** of methicillin-susceptible *Staphylococcus aureus*, from its MBC50 to its MBC90 (the MBC of a cephalosporin against a susceptible staphylococcus is the MIC or one dilution above it), divided by the free fraction of 0.85. Group A streptococci are killed at about the same level or lower. The urinary organisms are not in the band: cefalexin treats cystitis through its urine concentration. See *Shaded band: free drug at the MBC* above. *Time until threshold* is timed on the plasma curve. That curve is **total** cefalexin (bound plus free), but it is free drug that acts on the organism, so the threshold is the total concentration at which the **free** concentration equals the MIC. The line shows how long, with no further dose, until free cefalexin falls below the MIC. The MIC is **4 mg/L**, the MIC90 of methicillin-susceptible *S. aureus* (Haynes and colleagues, *Microbiol Spectr* 2022;10:e01039-22). Cefalexin is only 10 to 15 per cent bound, so with a free fraction of 0.85 the threshold is 4.7 mcg/mL total. See *Time until threshold: free drug at the MIC* above.

### Where to be careful

Renal impairment, food and formulation are not represented. Plasma concentrations do not establish bone or abscess exposure.
