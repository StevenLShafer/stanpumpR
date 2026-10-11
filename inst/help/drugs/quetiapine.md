### The model

Immediate-release quetiapine, from Zheng and colleagues (*Front Psychiatry* 2024;15:1497119): 99 Chinese inpatients with bipolar affective disorder, aged 17 to 69 and weighing 43 to 119 kg, sampled at steady state. One compartment with first-order absorption. At 70 kg the apparent clearance is 76.1 L/h and the apparent volume 530 L; the absorption rate constant, 1.46 per hour, was **fixed** from earlier literature, not estimated. The volume is imprecise (bootstrap 90% interval 210 to 2088 L).

### Oral only, immediate release only

The parameters are **apparent**, divided by an unmeasured oral bioavailability, so quetiapine is offered as **mg PO** only and bioavailability is carried as 1. Extended-release quetiapine absorbs through a chain of transit compartments, and its published model (Brogren and Nyberg 2010) lacks the transit number, intercompartmental clearance and volumes, so it is **not** offered. Do not use this curve for an XR tablet.

### Covariates

Weight is the source's only covariate: clearance with the 0.75 power of weight over 70 kg, volume in proportion to it. With the [fat-free-mass switch](help:models/fat-free-mass) off those allometric terms are applied exactly; with it on, the library's fat-free-mass factors replace them.

### Effect

There is **no effect site** and no band. Nord and colleagues (*Int J Neuropsychopharmacol* 2011;14:1357-1366) related striatal D2 occupancy directly to **plasma** quetiapine in 11 healthy men: occupancy = 100% × C / (525 ng/mL + C) (1369 nmol/L, Emax fixed at 100%). Half of the receptors are occupied at 525 ng/mL and 80% at 2100 ng/mL. That is a receptor biomarker measured in healthy volunteers, not a therapeutic target, and the pharmacokinetics come from a different population. Norquetiapine, which also binds D2, is not modelled.
