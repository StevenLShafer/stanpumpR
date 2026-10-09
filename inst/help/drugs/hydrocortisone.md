### The model

Hydrocortisone's parameters are a **linearised form** of the model of Bindellini and colleagues (*J Pharmacokinet Pharmacodyn* 2024;51:809-824), fitted to oral and intravenous hydrocortisone in 29 healthy men with endogenous cortisol suppressed by dexamethasone. The source model is nonlinear: free cortisol drives distribution and elimination (CLu 106 L/h, Q 89.9 L/h, Vp 61.7 L at 70 kg) while the total central amount, in a volume of 2.15 L, is bound to cortisol-binding globulin (CBG) with a capacity of about 430 nmol/L plus a linear nonspecific term of 4.15 times the free concentration.

### The linearisation, and what is plotted

stanpumpR's engine is linear. At the doses anaesthetists give (50 to 100 mg) total cortisol sits above 1000 nmol/L for hours, CBG is saturated, and the model becomes linear in the **increment of total cortisol above the saturated bound pool**: an ordinary two-compartment model with V1 2.15 L, clearance 20.6 L/h, Q 17.5 L/h and V2 12.0 L (the free-cortisol clearances and peripheral volume divided by 5.15). That increment is what the row plots, in mcg/mL (1 mcg/mL is 100 mcg/dL). Endogenous cortisol and the bound pool at baseline are not plotted and are not inputs.

### Oral route

The absorption rate constant, 1.10/h, matches the mean input time of the source's dose-dependent transit chain and depot (0.91 h at 5 mg). Bioavailability is 0.88, from Johnson and colleagues' paired-route tablet study on calculated unbound cortisol (*J Bioequiv Availab* 2018;10:001-003), rather than the source's estimated bioavailability of 0.344 (its Table 2), which sits inside a model whose dose convention could not be reproduced. This is a cross-study choice and the code says so.

### Covariates

Weight, as allometry: volumes linear, clearances to the 0.75 power, on the fat-free-mass ratio by default and on total weight with the switch off, as the source published.

### Effect site

None; the glucocorticoid effect is genomic and takes hours. Only the plasma concentration is plotted. The shaded band (0.2 to 1 mcg/mL above baseline, 20 to 100 mcg/dL) is the range of a stress response.

### Where to be careful

Below about 300 nmol/L of total cortisol, where replacement doses of 5 to 20 mg spend most of their time, CBG is not saturated. Run as published, the source's model then declines with an apparent half-time of total cortisol of 1.3 to 2 hours, against 0.9 hours here. **The linearisation understates the tail of every curve**, and the error grows with time: after 5 mg the source's model gives ten times this one's concentration at 4 hours. It also leaves out the part of the increment the CBG pool itself carries, at most 0.16 mcg/mL. For stress dosing the model is within about a quarter for the first hour after 50 mg and the first two hours after 100 mg. For replacement dosing this is the wrong tool and the source's nonlinear model should be run instead.
