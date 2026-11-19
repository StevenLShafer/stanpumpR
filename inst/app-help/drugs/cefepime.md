### The model

Cefepime's parameters come from the population model of Barreto and colleagues (*Antimicrob Agents Chemother* 2023;67:e00810-23, doi 10.1128/aac.00810-23). It was fitted to 379 serum samples from 120 critically ill adults at the Mayo Clinic (61 ± 17 years, 87 ± 26 kg, 82% with sepsis, 47% on vasopressors). Patients with acute kidney injury of stage II or higher, on renal replacement or on ECMO were excluded. The model has two compartments:

- **Clearance:** 7.84 L/h × eGFR / 120, with the eGFR in mL/min. Clearance is in proportion to renal function, with no non-renal part.
- **Central volume:** 15.56 L × weight / 70.
- **Intercompartmental clearance and peripheral volume:** fixed at 7.09 L/h and 10.63 L.

### The eGFR: creatinine only

The source's eGFR is the 2012 CKD-EPI equation using **both creatinine and cystatin C**, converted to mL/min with the patient's body surface area. It described cefepime clearance best of the eleven equations the authors compared. The Patient Profile has no cystatin C, and the paper prints no model refitted with a creatinine-only eGFR.

So this model uses the creatinine-only equation the authors found best, the 2012 CKD-EPI creatinine equation in mL/min, in the published model. That was Steven L. Shafer's choice on 2026-11-17. It has a known bias. In the source's patients the creatinine-only eGFR averaged 96 mL/min against 87 for the combined equation, so **this model predicts clearance about 10% higher on average** than the published model would with cystatin C. The difference is larger where creatinine overstates filtration, as it does after the loss of muscle in critical illness.

The eGFR is computed from the Patient Profile's **Serum creatinine**; a blank field means an assumed normal creatinine for age and sex. The equation is used without its race term, and converted to mL/min with the DuBois body surface area (the paper does not name its formula).

### The lowest clearance

The source studied no patient on dialysis, so its proportional clearance would fall toward zero as renal function does. The cefepime label says that clearance "decreased proportionally with creatinine clearance in patients with abnormal renal function", which supports the proportion. It also gives a half-life of 13.5 hours in patients on hemodialysis, with a steady-state volume of 18 L. That is a clearance of 0.92 L/h, which this model keeps as its lowest. The floor is reached at an eGFR of about 14 mL/min and changes nothing above it.

### Checks

The label's healthy men were not used in the fit. This model at a 35-year-old, 70-kg man with a normal creatinine (eGFR 115 mL/min):

| Quantity | Label, healthy men | This model |
|---|---|---|
| Clearance | 120 (± 8) mL/min | 126 mL/min |
| Peak after 1 g over 30 min | 81.7 mg/L | 51.9 mg/L |
| Peak after 2 g over 30 min | 163.9 mg/L | 103.7 mg/L |
| AUC after 1 g | 148.5 h·mg/L | 132.7 h·mg/L |
| Steady-state volume | 18 L | 26.2 L |
| Half-life | 2 h | 0.5 and 2.9 h |

Clearance agrees within 5%. The volumes are larger, as expected of critically ill patients against healthy volunteers, so the peaks are lower and the terminal half-life is longer.

### Covariates and body size

Weight acts on the central volume and, through the body surface area, on the eGFR in mL/min. With [the scaling switch](help:models/fat-free-mass) on, both use the pharmacokinetic weight; with it off, total body weight. The model was fitted in adults only.

### The MIC line

The line is free cefepime at **8 mg/L**. In the EUCAST breakpoint tables (v16.1, 2026), *Pseudomonas* isolates up to 8 mg/L are "susceptible, increased exposure", treated with the high dose of 2 g every 8 hours. For Enterobacterales the susceptible breakpoint is 1 mg/L. Cefepime is about 20% bound, so the line sits at 8 / 0.80 = **10 mg/L** of total drug. Steven L. Shafer chose this line on 18 November 2026 over the Enterobacterales breakpoint, which would put it at 1.25 mg/L of total drug. Against a known isolate the threshold can be edited. See [Antibiotics](help:models/antibiotics).

### No band

Most antibiotics' band marks the expected MBC, the concentration of free drug that kills. Cefepime has none: no MBC source has been read.

There is no consensus therapeutic range either. The ESICM/ESCMID position paper (Abdul-Aziz and colleagues, *Intensive Care Med* 2020;46:1127-1153, doi 10.1007/s00134-020-06050-1) recommends monitoring the beta-lactams to a target of 100% of the dosing interval with free drug above the MIC. It recommends a trough sample "30 min or just before the next dosing", 24–48 hours into therapy. Its Table 1 gives the cephalosporins' toxicity threshold as a trough above 20 mg/L, from cefepime data on neurotoxicity.

### Dose

Cefepime for injection is labelled "equivalent to 1 g [or 2 g] of cefepime" (as the hydrochloride), so doses are milligrams of cefepime, as in the source.
