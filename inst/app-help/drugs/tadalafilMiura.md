### The model

This is the model stanpumpR chooses for the youngest children given [tadalafil](help:drugs/tadalafil). It comes from Miura and colleagues (*Br J Clin Pharmacol* 2026;92:3611-3621, doi 10.1002/bcp.70676), who fitted 157 plasma concentrations from 31 Japanese patients with pulmonary arterial hypertension, sampled at steady state in routine care: 20 children aged 0.1 to 8.3 years and 11 adults. The model has one compartment, with apparent (oral) parameters:

- **Clearance:** CL/F 1.76 L/h × (weight / 14.2)<sup>0.602</sup>.
- **Volume:** V/F 80.6 L, with no weight term.
- **Absorption:** ka 0.84 /h, fixed by the authors from an earlier report because their sparse samples could not estimate it.

The paper writes the weight reference as the median without printing it; it is taken as the tadalafil cohort's median in its Table 1, 14.2 kg. Clearance was 40% lower in patients taking spironolactone, which the authors read as a marker of worsening right heart failure. That is not modelled: the curve is for a child not taking spironolactone.

### When it is chosen

Ferguson-Sells 2022, the library's other tadalafil model, studied children from 2.5 years in a larger study, so by the selector's rule it is chosen from 2.5 years and this model below that.

### Checks

The paper reports the median steady-state concentrations of its patients' individual fits: 461 ng/mL in children and 768 ng/mL in adults. This model without spironolactone gives 300 ng/mL at the children's median weight of 10.7 kg on 1 mg/kg a day, and 432 ng/mL at the adults' median of 52.3 kg on 40 mg a day; with spironolactone, 501 and 720. The paper's medians mix the two (13 of its 31 patients took spironolactone).
