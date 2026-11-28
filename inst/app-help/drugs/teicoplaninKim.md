### The model

Kim and colleagues (*Pharmaceutics* 2024;16:499, [doi:10.3390/pharmaceutics16040499](https://doi.org/10.3390/pharmaceutics16040499)) gave 200 mg over 30 minutes to 12 healthy Korean adults, 6 men and 6 women (median 32 years, 164 cm, 67.9 kg). They sampled venous plasma from the end of the infusion to 10 days and measured total teicoplanin by HPLC-MS/MS. Three compartments describe it:

- clearance 0.693 L/h × (CE / 105.27 mL/min)^0.785, where CE is the CKD-EPI 2021 eGFR multiplied by the body surface area / 1.73
- central volume 3.96 L, first peripheral 8.24 L, intercompartmental clearance 4.45 L/h
- deep volume 69.8 L × (weight / 67.85)^1.73, intercompartmental clearance 1.76 L/h

The paper does not name its body surface area formula; stanpumpR uses Du Bois. At the reference man the clearance is 0.77 L/h, the steady-state volume 88 L and the terminal half-life 102 hours. The paper's own non-compartmental half-life, over sampling that ended at 7 to 10 days, is 55 hours.

### When stanpumpR uses it

This is one of [two teicoplanin models](help:drugs/teicoplanin). stanpumpR uses it for every adult.

### Covariates

- **Serum creatinine**, through the de-indexed eGFR, on clearance. A blank field is a normal creatinine for age and sex. A child's creatinine is put on the adult scale before the adult equation is applied.
- **Weight** on the deep volume, as fitted. With [the scaling switch](help:models/fat-free-mass) on, it reads the pharmacokinetic weight, and the central and first peripheral volumes and both intercompartmental clearances are scaled to fat-free mass, anchored at the study's own 12 subjects. The clearance takes no further size factor, because the de-indexed eGFR already carries size.

### Where to be careful

The subjects were healthy and few. Critically ill patients, whose albumin is low and whose renal function changes, are the usual teicoplanin patients, and they were not studied. Teicoplanin's long terminal half-life means a loading regimen is needed to reach a trough of 15-30 mg/L within days.
