### The model

Humate-P is a plasma-derived concentrate of von Willebrand factor and factor VIII, about 2.4 IU of VWF:RCo (ristocetin cofactor activity) for every IU of factor VIII. It is approved in the US for von Willebrand disease and for haemophilia A. Unlike [Vonvendi](help:drugs/vonicogAlfa), it brings factor VIII with it.

Bauer and colleagues (*J Blood Med* 2023) fitted it in 20 adults aged 18 to 60 years (median 30) with type 3 von Willebrand disease, each given 50 IU/kg in a crossover with Vonvendi and sampled to 96 hours. VWF:RCo in two compartments, at 75 kg:

| | |
|---|---|
| Clearance | 4.14 dL/h × (weight/75)^0.75 |
| Central volume | 47.0 dL × (weight/75) × (haematocrit/0.4)^−0.334 |
| Intercompartmental clearance | 4.47 dL/h × (weight/75)^0.75 |
| Peripheral volume | 19.3 dL × (weight/75) |

At 75 kg a dose raises the VWF:RCo by 1.60 IU/dL for each IU/kg, the clearance is 5.5 mL/h/kg, the volume at steady state 88 mL/kg, and the half-lives 1.9 and 12.2 hours. Vonvendi's half-life is 1.74 times as long, as the paper reports. The label's two studies (types 1, 2 and 3, before surgery) give median half-lives of 11 and 10 hours, clearances of 3.1 and 4.8 mL/h/kg, volumes of 53 and 59 mL/kg and recoveries of 2.4 and 1.9 IU/dL per IU/kg.

### Factor VIII

Factor VIII is drawn on the panel [factor VIII (with VWF)](help:drugs/factorVIIIvwf), shared with Vonvendi. It is the patient's own factor VIII, protected by the VWF:RCo, plus the product's: each IU/kg of VWF:RCo carries 1/2.4 IU/kg of factor VIII into a volume of 32.9 dL, and both are cleared by the same VWF-dependent process,

dF/dt = kout × F0 + infused factor VIII/32.9 dL − kout × (1 − Imax × VWF/(IC50 + VWF)) × F,

with kout 15.9 /h and F0 0.5 IU/dL as for Vonvendi, Imax 0.994 and IC50 0.0577 IU/dL. After 50 IU/kg at 75 kg factor VIII rises at once from 4.2 to 52 IU/dL, peaks at 61 IU/dL at 10 hours as the patient's own factor VIII is protected, and falls to 10 IU/dL at 72 hours.

The source supports treating the product's factor VIII like the patient's own with a half-life of 11.5 hours at 50 IU/dL of VWF:RCo; that number comes from Vonvendi's Imax. With Humate-P's own Imax the same arithmetic gives 6.1 hours.

### What is plotted

VWF:RCo in **IU/dL**: the patient's own, the Patient Profile's **VWF:RCo baseline** (shared with Vonvendi; blank, 0.5 IU/dL, type 3 disease), plus the infused. The band is the label's VWF:RCo peak target, 50 to 60 IU/dL for minor surgery and 100 IU/dL for major. After major surgery the label keeps the trough of each factor above 50 (VWF:RCo) and 30 (factor VIII) IU/dL, and neither above 100.

### Covariates

- **Weight**, on the VWF:RCo parameters, as fitted. The factor VIII volume has no size term in the source and is scaled by fat-free mass from the cohort's typical patient (66.3 kg, 30 years, 9 men and 11 women; heights not printed, taken from NCD-RisC for the study's six countries).
- **Haematocrit** is held at 0.4: the Patient Profile has none.
- **Age**: adults only were studied; the label approves Humate-P in children, who are drawn with the adult model scaled by weight.
- **Pregnancy**: nothing applied (see the pregnancy section below).

### Doses

`IU/kg` or `IU` of **VWF:RCo**, as the label doses von Willebrand disease, and `IU/kg/hr` or `IU/hr` as an infusion. The label doses haemophilia A in IU of factor VIII; that use is not offered, because the model's factor VIII production is that of a patient with von Willebrand disease: a patient with haemophilia A has normal von Willebrand factor and makes no factor VIII, which the model cannot represent.

### Not modelled

- Vonvendi and Humate-P given to the same patient: the factor VIII model is not linear in the VWF:RCo, so the factor VIII panel cannot add the two, and the plot says so instead.
- Other plasma-derived VWF/FVIII concentrates (Alphanate, Wilate), whose ratios and kinetics differ.
- The patient's own von Willebrand factor is held constant.
