### The model

This is the model stanpumpR chooses for children under 8 years given [ambrisentan](help:drugs/ambrisentan). It comes from Miura and colleagues (*Br J Clin Pharmacol* 2026;92:3611-3621, doi 10.1002/bcp.70676), who fitted 177 plasma concentrations from 29 Japanese patients with pulmonary arterial hypertension, sampled at steady state in routine care: 18 children aged 0.4 to 8.8 years and 11 adults. The model has one compartment, with apparent (oral) parameters:

- **Clearance:** CL/F 0.594 L/h × (weight / 13.4)<sup>0.561</sup>.
- **Volume:** V/F 12.4 L, for the cohort's typical patient. With [the scaling switch](help:models/fat-free-mass) on it scales with fat-free mass: the paper did not explore covariates on V/F (its shrinkage exceeded 30%), which is not evidence that size has no effect (stanpumpR's rule that a parameter without a size term in its source still scales with size, Steven L. Shafer, 2026-11-19). The published value is anchored at the cohort's children and adults together, each group at its median age and weight with Japanese heights (the JSPE 2000 growth reference for the children, NCD-RisC for the adults), weighted by their numbers of patients; so a small child receives less than the published volume and an adult more. With the switch off it is the published value for everyone.
- **Absorption:** ka 1.03 /h.

The paper writes the weight reference as the median without printing it; it is taken as the ambrisentan cohort's median in its Table 1, 13.4 kg.

### Checks

The paper reports the median steady-state concentrations of its patients' individual fits: 164 ng/mL in children and 298 ng/mL in adults. This model gives 166 ng/mL at the children's median weight of 10.1 kg on 0.2 mg/kg a day, and 330 ng/mL at the adults' median of 51.4 kg on 10 mg a day. At 70 kg its apparent clearance is 1.50 L/h, against the Letairis label's "19 mL/min" (1.14 L/h) in adults.
