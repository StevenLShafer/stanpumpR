### The model

Vuong and colleagues (*Infection* 2025, [doi:10.1007/s15010-025-02663-0](https://doi.org/10.1007/s15010-025-02663-0)) pooled the individual data of eight intensive-care studies from Belgium, the Netherlands, Germany and Australia: 177 critically ill adults given intravenous fluconazole, 1616 concentrations. Two compartments describe them. For a patient not on renal replacement therapy:

- clearance 0.614 L/h × (eGFR / 80)^0.532, the eGFR from the CKD-EPI equation without race, in mL/min/1.73 m²
- central volume 39.4 L × (weight / 80 kg)^0.908
- intercompartmental clearance 12.1 L/h and peripheral volume 8.47 L

It is the largest adult model of fluconazole, and it is used for every adult (Steven L. Shafer's decision of 2026-11-26).

### Critically ill patients, not healthy volunteers

The Diflucan label gives healthy adults a clearance of 0.23 mL/min/kg, about 0.97 L/h at 70 kg, and a half-life of about 30 hours. This model gives about 0.6-0.7 L/h at a normal eGFR, roughly 40% less, and a half-life near two days. Critically ill patients clear fluconazole more slowly than their creatinine suggests. For an outpatient with healthy kidneys, for example a single 150 mg tablet, the curve is likely too high.

Patients on continuous renal replacement therapy cleared 1.56 L/h in the source, more than twice the typical value off it. The Patient Profile has no field for renal replacement, so the off-therapy clearance is always used, and the curve of a patient on it is too high.

### The oral route

Vuong's data were intravenous. Oral doses use the label's bioavailability, "over 90%", taken as 0.9, and the absorption rate of McLachlan and Tett 1996 (*Br J Clin Pharmacol* 41:291), 3.07 per hour, from 113 men with HIV given oral and intravenous fluconazole.

### Covariates

- **Kidney function** from the Serum creatinine field (an assumed normal creatinine for age and sex when it is blank), through the 2021 CKD-EPI equation. The source studied eGFRs from 7 to 213 mL/min/1.73 m².
- **Weight** on the central volume, as fitted; the source studied 34 to 142 kg.
- With [the scaling switch](help:models/fat-free-mass) on, the weight term reads the pharmacokinetic weight, and the clearances and the peripheral volume are scaled to fat-free mass at the 70 kg reference man. The source prints no height for its cohort, so its typical patient could not be used as the anchor.

### Pregnancy

No pharmacokinetic study of fluconazole in pregnancy was found. About 80% of a dose is excreted unchanged in urine, so the 50% rise in filtration of late pregnancy is applied to the eGFR when the serum creatinine is blank, raising clearance by about a quarter.

### Target concentrations

No band is drawn. The antimicrobial drug-monitoring position paper of Abdul-Aziz and colleagues (*Intensive Care Med* 2020;46:1127-1153) gives targets for the ratio of the daily area under the curve to the organism's MIC, not a concentration range, and neither recommends nor discourages monitoring fluconazole. Vuong treats a trough of 80 mg/L as a toxicity threshold.
