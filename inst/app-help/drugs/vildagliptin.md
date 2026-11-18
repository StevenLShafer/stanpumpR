### The model

Landersdorfer, He and Jusko's mechanism-based population model (*Br J Clin Pharmacol* 2012). The data were 13 patients with type 2 diabetes given 10, 25 and 100 mg twice daily for 28 days, with an intravenous bioavailability study. Vildagliptin is both an inhibitor and a substrate of DPP-4. It binds the enzyme slowly and tightly, in plasma and in the tissues. Most of the bound drug comes off again unchanged, but some is hydrolysed by the enzyme, which frees the enzyme to bind again.

- **Disposition:** two compartments, a central volume of 22.2 L, a tissue volume of 97.3 L, an intercompartmental clearance of 40.1 L/h, and a linear clearance of 36.4 L/h.
- **Binding:** 5 nmol of DPP-4 in the central compartment and 13 µmol in the tissues. Binding runs at k2 23.4/h × free DPP-4 × C/(Kd + C), with Kd 71.9 nmol/L. The complex dissociates at 0.612/h and is hydrolysed at 0.110/h, a half-life of 6.3 hours.
- **Absorption:** two first-order compartments in sequence (1.26 and 1.05/h), with bioavailability 0.772.

The tissue DPP-4 binds a large share of a small dose. As the dose rises it fills, so clearance falls: 74 L/h for a vanishingly small dose and 42 L/h after a single 100 mg dose, as the paper reports. The terminal phase also lengthens with the dose. The curve comes from numerical integration of the paper's equations over the whole dose table (the engine that runs [phenytoin](help:drugs/phenytoin)), and the plotted concentration is the free drug in plasma, as the paper's model has it.

### Covariates

None; the study had 13 patients. The volumes, clearances and amounts of DPP-4 are scaled by the library's fat-free-mass factors; with **Adjust weight to fat-free mass** unticked they are the published values. The patients weighed 65 to 116 kg.

### Product and doses

Galvus, marketed outside the United States: `mg PO`, `mg PO qd` and `mg PO bid` (the usual dose is 50 mg once or twice daily). At steady state on 10, 25 and 100 mg twice daily the model's peaks are 40, 107 and 461 ng/mL, against medians of about 45, 120 and 500 read from the paper's Figure 2.

### Renal impairment: not in the model

The study's 13 patients gave no covariates. Scheen's review of incretin-based therapies in chronic kidney disease (*Clin Pharmacokinet* 2015) gives the dedicated study: 50 mg once daily for 14 days reached a steady-state AUC of 990 ng·h/mL and a peak of 251 ng/mL with normal renal function, against the model's 844 and 209 for an 80 kg man. Exposure was 1.40, 1.71 and 2.00 times higher in mild, moderate and severe impairment, which the curve does not show. The label halves the daily dose in moderate and severe impairment.

### What is not plotted

Concentration only, in ng/mL. The DPP-4 activity fitted alongside, and the GLP-1, glucose and insulin of the companion paper, are not drawn. See [Antidiabetic models and their limits](help:models/antidiabetics).
