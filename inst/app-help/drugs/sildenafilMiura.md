### The model

This is the model stanpumpR chooses for children given [sildenafil](help:drugs/sildenafil). It comes from Miura and colleagues (*Br J Clin Pharmacol* 2026;92:3611-3621, doi 10.1002/bcp.70676), who fitted 297 plasma concentrations from 24 Japanese patients with pulmonary arterial hypertension, sampled at steady state in routine care: 15 children aged 0.4 to 14.8 years and 9 adults. The model has one compartment, with apparent (oral) parameters:

- **Clearance:** CL/F 60.6 L/h × (weight / 32.3)<sup>0.537</sup>.
- **Volume:** V/F 322 L, with no weight term.
- **Absorption:** ka 4.51 /h, fixed by the authors from an earlier report because their sparse samples could not estimate it.

The paper writes the weight reference as the median without printing it; it is taken as the sildenafil cohort's median in its Table 1, 32.3 kg. Clearance was 1.63 times higher in patients also taking bosentan, which induces sildenafil's metabolism. That interaction is not modelled: the curve is for a child not taking bosentan.

### Intravenous doses

Miura fitted oral doses only. In a child, an intravenous dose of sildenafil is run on the [FDA review's model](help:drugs/sildenafilFDA), scaled to the child's size, and added to the oral curve.

### Checks

The paper reports the median steady-state concentrations of its patients' individual fits: 25.4 ng/mL in children and 34.3 ng/mL in adults. This model gives 29.6 ng/mL at the children's median weight of 16.5 kg on 30 mg a day, and 33.2 ng/mL at the adults' median of 48.5 kg on 60 mg a day.

### Limits

The volume does not scale with weight, so a small child has an adult's apparent volume: the source's model, fitted to few samples per child. Most of the samples came from the adults (260 of 297).
