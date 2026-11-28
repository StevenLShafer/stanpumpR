### The model

Vonvendi is recombinant von Willebrand factor (vonicog alfa), approved in the US for bleeding, surgery and prophylaxis in von Willebrand disease. It carries no factor VIII, but von Willebrand factor protects factor VIII from clearance, so the patient's own factor VIII rises after it, over hours.

Bauer and colleagues' population analysis (*J Blood Med* 2023), with its supplement, pooled four trials: 79 adults aged 18 to 70 years (median 35), weighing 44 to 145 kg (median 74), 57 with type 3 von Willebrand disease, 12 with types 1 and 2, and 10 with severe haemophilia A. Once each patient's own von Willebrand factor was accounted for, the types did not differ. VWF:RCo (ristocetin cofactor activity) in two compartments, at 75 kg:

| | |
|---|---|
| Clearance | 2.10 dL/h × (weight/75)^0.75 |
| Central volume | 43.5 dL × (weight/75) × (haematocrit/0.4)^−0.334 |
| Intercompartmental clearance | 2.29 dL/h × (weight/75)^0.75 |
| Peripheral volume | 15.8 dL × (weight/75) |

At 75 kg a dose raises the VWF:RCo by 1.72 IU/dL for each IU/kg, the clearance is 0.028 dL/kg/h, and the half-lives are 3.3 and 21.1 hours. The label's adults (50 IU/kg): recovery 1.9 ± 0.41, clearance 0.02 ± 0.005 dL/kg/h, half-life 22.6 ± 5.3 hours; its dosing rule assumes a recovery of 2.0.

### Factor VIII

Factor VIII is drawn on a panel of its own, [factor VIII (with VWF)](help:drugs/factorVIIIvwf), by the source's indirect-response model: the VWF:RCo the patient has (their own plus the infused) slows the elimination of factor VIII,

dF/dt = kout × F0 − kout × (1 − Imax × VWF/(IC50 + VWF)) × F,

with kout 15.9 /h, Imax 0.998, IC50 0.0658 IU/dL and F0 0.5 IU/dL, the factor VIII a patient with no von Willebrand factor at all would have. In type 3 disease (0.5 IU/dL of VWF:RCo) that gives a factor VIII of 4.2 IU/dL before any dose; after 50 IU/kg it rises to about 98 IU/dL at 28 hours (the label: a peak "approximately 24 hours post-infusion"). Given every 72 hours, factor VIII stays above 40 IU/dL throughout, as the source's own simulation found.

Because factor VIII depends on the VWF:RCo through a saturable function, the two do not rise in proportion: the factor VIII curve is computed from the VWF:RCo curve, not scaled from it.

### What is plotted

VWF:RCo activity in **IU/dL** (% of normal): the patient's own plus the activity infused. The patient's own is the Patient Profile's **VWF:RCo baseline**, which also sets the factor VIII before the first dose. Left blank, it is 0.5 IU/dL, the source's fixed value in type 3 disease. A patient with type 1 or 2 disease has a higher level, which should be entered.

The band is the label's VWF:RCo peak target around surgery: 50 to 60 IU/dL for minor surgery and 100 IU/dL for major, drawn 50 to 100. After major surgery the label keeps the trough above 50 IU/dL for 72 hours, and factor VIII above 30 (Table 4).

### Covariates

- **Weight**, on every parameter, as fitted; with the fat-free-mass switch on, the weight in these terms is the pharmacokinetic weight.
- **Haematocrit**, on the central volume and on F0, is held at 0.4, the source's reference: the Patient Profile has no haematocrit. A lower haematocrit gives a larger central volume (a smaller rise) and a higher factor VIII.
- **Age**: the source studied adults only. The label approves Vonvendi from 1 year of age; a child is drawn with the adult model scaled by weight, which is an extrapolation.
- **Pregnancy**: no pharmacokinetic data (label 8.1); nothing is applied. Von Willebrand factor and factor VIII rise in late pregnancy, which the baseline field carries.

### Doses

`IU/kg` or `IU` as a dose (VWF:RCo units, as labelled), and `IU/kg/hr` or `IU/hr` as an infusion.

### Not modelled

- **Recombinant factor VIII** given with Vonvendi, which the label advises when factor VIII is below 40 IU/dL or unknown, is not added to the factor VIII panel. The source fitted factor VIII from Vonvendi alone, and infused factor VIII is cleared by the same process von Willebrand factor slows, which an added curve would not carry. [Factor VIII](help:drugs/factorVIII) can be dosed as a drug of its own, but its curve is drawn without the von Willebrand factor.
- The patient's own von Willebrand factor is held constant.
- **Humate-P** and the other plasma-derived von Willebrand factor/factor VIII concentrates, which Bauer also modelled, are not yet in the library.
