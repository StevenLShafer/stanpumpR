### The model

Bosentan's parameters come from the population model of Miura and colleagues (*Br J Clin Pharmacol* 2026;92:3611-3621, doi 10.1002/bcp.70676). It was fitted to 189 plasma concentrations from 16 Japanese patients with pulmonary arterial hypertension. Seven were children aged 2.2 to 9.2 years and nine were adults weighing 38.5 to 58.7 kg; 170 of the samples came from the adults. Every patient had taken bosentan for at least 4 weeks, so the samples were drawn at steady state. The model has one compartment, with apparent (oral) parameters:

- **Clearance:** CL/F 8.85 L/h × (weight / 42.2)<sup>0.472</sup>.
- **Volume:** V/F 41.0 L, for the cohort's typical patient. With [the scaling switch](help:models/fat-free-mass) on it scales with fat-free mass: the paper did not explore covariates on V/F (its shrinkage exceeded 30%), which is not evidence that size has no effect (stanpumpR's rule that a parameter without a size term in its source still scales with size, Steven L. Shafer, 2026-11-19). The published value is anchored at the cohort's children and adults together, each group at its median age and weight with Japanese heights (the JSPE 2000 growth reference for the children, NCD-RisC for the adults), weighted by their numbers of patients; so a small child receives less than the published volume and an adult more. With the switch off it is the published value for everyone.
- **Absorption:** ka 2.47 /h after a lag of 0.379 h.

The paper also found that a higher ALT lowers clearance: (ALT / 16.5)<sup>−0.203</sup>. The Patient Profile has no ALT field, so ALT is held at the cohort's median of 16.5 IU/L, where the term is 1. Within the studied range of 7 to 50 IU/L the term would change clearance by 1.19 to 0.80 times.

The paper writes the weight and ALT references as the medians without printing them. They are taken as the bosentan cohort's medians in its Table 1, 42.2 kg and 16.5 IU/L.

### Steady state only

Bosentan induces its own metabolism. The Tracleer label says that "upon multiple oral dosing, plasma concentrations in healthy adults decrease gradually to 50–65% of those seen after single dose administration", and that steady state "is reached within 3–5 days". This model has the induced clearance of patients on long-term treatment. **It under-predicts the first days of treatment, by up to about half, and is right from about the fifth day.** The paper gives no parameters for the induction itself, so it is not modelled.

### Checks

The paper reports the median steady-state concentrations of its patients' individual fits: 1221 ng/mL in adults on 250 mg a day, and 339 ng/mL in children on 4 mg/kg a day. This model gives 1102 ng/mL at the adults' median weight of 48.5 kg, and 409 ng/mL at the children's median of 12 kg.

Against the label, which was not used in the fit, this model clears 11.2 L/h at 70 kg. The label gives about 8 L/h before induction (an intravenous clearance of "about 4 L/h in patients with PAH" over a bioavailability of "about 50%" in healthy volunteers), and 12–16 L/h after it. The label's peak "within 3–5 hours" is later than this model's 1.4 hours. The paper says most of its samples were drawn in the elimination phase, which leaves absorption poorly defined.

### Body size and age

Weight acts on clearance by the source's own power, and fat-free mass on the volume (see above). With [the scaling switch](help:models/fat-free-mass) on, it is the pharmacokinetic weight; with it off, total body weight. The studied weights were 6.9 to 58.7 kg, so heavier adults are an extrapolation of that power.

The model is offered at every age. The label says that in children "exposure to bosentan reaches a plateau at lower doses" than in adults, and that "doses higher than 2 mg/kg twice daily do not increase the exposure". This model is linear and does not carry that plateau.

### Interactions

Bosentan induces CYP3A and CYP2C9. In the same paper, sildenafil's clearance was 1.63 times higher in patients also taking bosentan. That interaction is not modelled.

### Dose

"Each TRACLEER 62.5 mg tablet contains 64.54 mg of bosentan monohydrate, equivalent to 62.5 mg of anhydrous bosentan", and the source dosed the same tablets. Doses are therefore milligrams of anhydrous bosentan, as labelled.

### No band

No therapeutic range is established. Doses are fixed by weight.
