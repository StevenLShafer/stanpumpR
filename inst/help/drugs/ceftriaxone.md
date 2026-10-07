### The model

Ceftriaxone's parameters are from Sanz-Codina and colleagues (*J Antimicrob Chemother* 2023;78:380-388), who gave 2 g over 30 minutes to **six healthy men**: clearance 1.21 L/h, central volume 5.76 L, intercompartmental clearance 2.92 L/h, peripheral volume 2.97 L, fitted on **total** plasma concentration. Clearance acts on total drug in this fit, so the model is linear as published and implemented exactly.

### What is plotted

Total ceftriaxone, which is what a laboratory reports. Ceftriaxone is 85 to 95 per cent albumin-bound with saturable binding; the paper's purpose was to compare two ways of measuring the free fraction, and neither binding map is applied here. The shaded band (10 to 50 mcg/mL total) is an illustrative range of total concentration, for orientation only. It is not a free-drug target: at 95 per cent binding, 10 mcg/mL total is 0.5 mcg/mL free, below a 1 to 2 mg/L MIC.

### Covariates

None in the source. The parameters take the default [fat-free-mass scaling](help:models/fat-free-mass) and are used as published with the switch off. Renal function is not a covariate: ceftriaxone has substantial biliary elimination and the healthy cohort offered no renal spread to fit.

### Effect site

None; only the plasma concentration is plotted. *Time until threshold* is therefore timed on the plasma curve. That curve is **total** ceftriaxone (bound plus free), but it is free drug that acts on the organism, so the threshold is the total concentration at which the **free** concentration equals the MIC. The line shows how long, with no further dose, until free ceftriaxone falls below the MIC. The MIC is **1 mg/L**, the Enterobacterales susceptible breakpoint that CLSI and EUCAST share. Binding is saturable, so the threshold comes from Sanz-Codina's own binding fit in the same six men as the curve: free ceftriaxone reaches 1 mg/L when the total is about **15 mcg/mL** (free fraction 0.065). It depends on how binding is measured and on albumin: equilibrium dialysis would put it nearer 19 to 22 mcg/mL, and at the lower albumin usual in surgical patients it is nearer 13 mcg/mL. See *Time until threshold: free drug at the MIC* above.

### Where to be careful

Six healthy men is a small, homogeneous anchor. Critically ill patients have a separately fitted model with larger volumes, and their coefficients have not been mixed into this one.
