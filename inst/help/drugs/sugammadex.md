### The model

Sugammadex's parameters are from Kleijn and colleagues (*Br J Clin Pharmacol* 2011;72:415-433), who modelled free sugammadex, free rocuronium and their complex in 426 patients and 20 volunteers. Because the complex was given the same disposition as free sugammadex, the sum of the two is an autonomous two-compartment model of **total sugammadex**, which is also what the assay measured and what this row plots. At the source's reference (74.5 kg, creatinine clearance 119 mL/min) clearance is 5.58 L/h, central volume 4.70 L, intercompartmental clearance 13.0 L/h and peripheral volume 6.76 L.

### Covariates

Every parameter carries a weight term and two carry creatinine clearance: clearance rises with weight and falls steeply as creatinine clearance declines, through a saturating term, and the peripheral volume grows as creatinine clearance falls. Creatinine clearance is Cockcroft-Gault at the **Serum creatinine** from the Patient Profile, or at an **assumed normal creatinine** when that is left blank, when renal decline with age is represented but **renal impairment is not**. The label does not recommend sugammadex below 30 mL/min. The weight the equations see is the pharmacokinetic weight under the default [fat-free-mass scaling](help:models/fat-free-mass) and total body weight with the switch off. The source's ethnicity term is left at its reference value.

### Effect site

None. Sugammadex acts in plasma, by encapsulating rocuronium there; the reversal it produces is a property of the rocuronium curve, not of a sugammadex effect site.

### Rocuronium binding is not modelled

This is the main limitation of simulating the two side by side: giving sugammadex does not change the [rocuronium](help:drugs/rocuronium) row. The source's binding and train-of-four model would need coupled, mass-conserving states for free sugammadex, free rocuronium and the complex, which the closed-form engine does not have. The row shows the sugammadex concentration alone. The shaded band (5 to 30 mcg/mL) covers what 2 to 4 mg/kg produce over the first hour.

### Where to be careful

Doses are sugammadex equivalents as the label states them. Nothing here predicts the time to a train-of-four ratio of 0.9, recurarisation, or the dose needed for a given depth of block.
