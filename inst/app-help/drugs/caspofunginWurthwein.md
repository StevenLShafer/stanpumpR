### The model

Würthwein and colleagues (*Antimicrob Agents Chemother* 2013;57:1664-1671, [doi:10.1128/AAC.01912-12](https://doi.org/10.1128/AAC.01912-12)) studied 46 adults with invasive aspergillosis at three German university hospitals: 21 men and 25 women, median age 61 years (18 to 74), median weight 76 kg (43 to 104). They were given 70, 100, 150 or 200 mg of caspofungin once daily as a 2-hour infusion for up to 28 days, and 462 plasma samples were measured. Two compartments describe them (Table 2, at 76 kg):

- clearance 0.411 L/h × [1 + 0.0102 × (weight − 76 kg)]
- central volume 5.85 L × [1 + 0.0102 × (weight − 76 kg)]
- intercompartmental clearance 0.843 L/h
- peripheral volume 6.53 L

One weight factor serves both clearance and the central volume. The abstract prints a central volume of 5.785 L; Table 2's final estimate, 5.85 L, is used. Gender, age and creatinine clearance were tested and not retained. Elimination is linear from 70 to 200 mg.

With **Adjust weight to fat-free mass** ticked, the weight term is read at the pharmacokinetic weight, and the intercompartmental clearance and peripheral volume are scaled by fat-free mass. Those two are anchored at stanpumpR's reference man, not at Würthwein's cohort, because the paper prints no height. Unticked, they are the published values.

### When stanpumpR uses it

This is one of [two caspofungin models](help:drugs/caspofungin). stanpumpR uses it for adults.

### Checks

Table 3's steady-state AUCs, 170, 243, 365 and 487 mg·h/L at 70 to 200 mg daily, are each dose divided by 0.411 L/h, and the model reproduces them.

The CANCIDAS label's adults cleared caspofungin faster: a harmonic-mean clearance of 10.6 mL/min and an AUC of 87.3 mg·h/L at 50 mg. A 70 kg adult in this model clears 6.4 mL/min, an AUC of 130 mg·h/L at 50 mg. Würthwein's patients were ill with invasive aspergillosis, and their clearance was close to that of the earlier CASLAMB patients (0.462 L/h).
