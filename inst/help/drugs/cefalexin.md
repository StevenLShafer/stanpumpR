### The model

Cefalexin's parameters are from Haynes and colleagues (*Antimicrob Agents Chemother* 2024;68:e00182-24), who fitted oral cefalexin (capsules and suspension) in **15 children** with musculoskeletal infection: CL/F 13.98 L/h and V/F 26.63 L at 70 kg, one compartment, absorption rate constant 1.79/h with no lag. Half-time at 70 kg is 1.3 hours.

### Oral only

The parameters are **apparent**: clearance and volume divided by an unmeasured bioavailability. They predict oral concentrations correctly because the bioavailability cancels, and would predict intravenous ones wrong by one over that bioavailability, so cefalexin is offered as **mg PO** only and bioavailability is carried as 1. There is no intravenous cefalexin product in any case.

### A pediatric model used in adults

The 70 kg normalisation is a convention of the fit, not evidence that the model describes adults. Ryder and colleagues (*Pharmacotherapy* 2026) used this vector to simulate adults, which is precedent for the extrapolation rather than validation of it.

### Covariates

Weight, as allometry: volumes linear, clearance to the 0.75 power, on the fat-free-mass ratio by default and on total weight with the switch off. No renal covariate was fitted (the children had normal kidneys), and the weight term does not stand in for one.

### Effect site

None; only the plasma concentration is plotted. The shaded band (1 to 4 mcg/mL) spans the MICs of the staphylococci and streptococci cefalexin is used against.

### Where to be careful

Renal impairment, food and formulation are not represented. Plasma concentrations do not establish bone or abscess exposure.
