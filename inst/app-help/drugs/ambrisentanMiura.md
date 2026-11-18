### The model

This is the model stanpumpR chooses for children under 8 years given [ambrisentan](help:drugs/ambrisentan). It comes from Miura and colleagues (*Br J Clin Pharmacol* 2026;92:3611-3621, doi 10.1002/bcp.70676), who fitted 177 plasma concentrations from 29 Japanese patients with pulmonary arterial hypertension, sampled at steady state in routine care: 18 children aged 0.4 to 8.8 years and 11 adults. The model has one compartment, with apparent (oral) parameters:

- **Clearance:** CL/F 0.594 L/h × (weight / 13.4)<sup>0.561</sup>.
- **Volume:** V/F 12.4 L, with no weight term.
- **Absorption:** ka 1.03 /h.

The paper writes the weight reference as the median without printing it; it is taken as the ambrisentan cohort's median in its Table 1, 13.4 kg.

### Checks

The paper reports the median steady-state concentrations of its patients' individual fits: 164 ng/mL in children and 298 ng/mL in adults. This model gives 166 ng/mL at the children's median weight of 10.1 kg on 0.2 mg/kg a day, and 330 ng/mL at the adults' median of 51.4 kg on 10 mg a day. At 70 kg its apparent clearance is 1.50 L/h, against the Letairis label's "19 mL/min" (1.14 L/h) in adults.
