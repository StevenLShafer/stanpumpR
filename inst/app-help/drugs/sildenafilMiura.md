### The model

This is the model stanpumpR chooses for children given [sildenafil](help:drugs/sildenafil). It comes from Miura and colleagues (*Br J Clin Pharmacol* 2026;92:3611-3621, doi 10.1002/bcp.70676), who fitted 297 plasma concentrations from 24 Japanese patients with pulmonary arterial hypertension, sampled at steady state in routine care: 15 children aged 0.4 to 14.8 years and 9 adults. The model has one compartment, with apparent (oral) parameters:

- **Clearance:** CL/F 60.6 L/h × (weight / 32.3)<sup>0.537</sup>.
- **Volume:** V/F 322 L, for the cohort's typical patient. With [the scaling switch](help:models/fat-free-mass) on it scales with fat-free mass: the paper did not explore covariates on V/F (its shrinkage exceeded 30%), which is not evidence that size has no effect (stanpumpR's rule that a parameter without a size term in its source still scales with size, Steven L. Shafer, 2026-11-19). The published value is anchored at the cohort's children and adults together, each group at its median age and weight with Japanese heights (the JSPE 2000 growth reference for the children, NCD-RisC for the adults), weighted by their numbers of patients; so a small child receives less than the published volume and an adult more. With the switch off it is the published value for everyone.
- **Absorption:** ka 4.51 /h, fixed by the authors from an earlier report because their sparse samples could not estimate it.

The paper writes the weight reference as the median without printing it; it is taken as the sildenafil cohort's median in its Table 1, 32.3 kg. Clearance was 1.63 times higher in patients also taking bosentan, which induces sildenafil's metabolism. That interaction is not modelled: the curve is for a child not taking bosentan.

### Intravenous doses

Miura fitted oral doses only. In a child, an intravenous dose of sildenafil is run on the [FDA review's model](help:drugs/sildenafilFDA), scaled to the child's size, and added to the oral curve.

### Checks

The paper reports the median steady-state concentrations of its patients' individual fits: 25.4 ng/mL in children and 34.3 ng/mL in adults. This model gives 29.6 ng/mL at the children's median weight of 16.5 kg on 30 mg a day, and 33.2 ng/mL at the adults' median of 48.5 kg on 60 mg a day.

### Limits

Most of the samples came from the adults (260 of 297), and Miura's V/F is large: anchored at the cohort, a 70 kg man receives about 900 L with the switch on, against about 256 L in the FDA's adult model. The group's adults get the FDA model, so this matters only for its children.
