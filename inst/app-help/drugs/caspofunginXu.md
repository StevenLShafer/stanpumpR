### The model

Xu and colleagues (*Antimicrob Agents Chemother* 2026;70:e01277-25, [doi:10.1128/aac.01277-25](https://doi.org/10.1128/aac.01277-25)) studied 29 critically ill children in a Shanghai paediatric intensive care unit, 0.33 to 16 years old (median 5.3) and 4.9 to 74 kg (median 16). They were given 70 mg/m² on the first day, then 50 mg/m² daily (at most 70 mg), as 1-hour infusions, and were sampled intensively after the sixth dose. Two compartments describe them, for a child of 0.79 m²:

- clearance 0.196 L/h × (BSA / 0.79)^0.66
- central volume 2.22 L × (BSA / 0.79)
- intercompartmental clearance 1.01 L/h × (BSA / 0.79)^0.66
- peripheral volume 1.63 L × (BSA / 0.79)

BSA is the Mosteller body-surface area, as in the study and the label. The paper fixes the exponents at 0.66 for clearance and 1 for volume. stanpumpR applies them to both clearances and both volumes, the usual reading, since the paper's table does not say so for the second pair. Four children were on ECMO, which raised the central volume. There is no ECMO field, so that effect is not applied.

### When stanpumpR uses it

For every patient, at present: it is the only [caspofungin model](help:drugs/caspofungin). For an adult it is an extrapolation, which that page explains.

### Covariates

- **Body-surface area** on every parameter, from the patient's weight and height. It is always computed from the actual weight, because the model was fitted against the children's actual size. So the [scaling switch](help:models/fat-free-mass) does not change this drug's curve.

### Against the label

At 50 mg/m² a day, the label's children (Cancidas, Table 8) reach an area under the curve of 120 to 131 mg·h/L a day. This model gives a 0.79 m² child 202 mg·h/L (39.5 mg / 0.196 L/h), and Xu's children reached a median of 194. Critically ill children cleared less caspofungin per m² than the label's.

### Where to be careful

With only 29 children, the parameters are uncertain: the central volume's standard error is 41%. The study's sampling began after the sixth dose, so it carries no information about the first-day loading dose.
