### One name, the model chosen for the patient

stanpumpR holds 2 published models of oral temozolomide. You do not choose between them: stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [temozolomideOstermann](help:drugs/temozolomideOstermann) | Ostermann 2004 | 35 adults with malignant glioma, 30-79 years |
| [temozolomidePanetta](help:drugs/temozolomidePanetta) | Panetta 2003 | 39 infants, children and young adults with brain tumours, 0.7-21.9 years |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for temozolomide.** The two age ranges do not overlap. Patients up to 25.9 years get Panetta 2003, and older patients get Ostermann 2004. At an adult's body-surface area the two give similar clearances (about 10.6 against 10.0 to 11.9 L/h at 1.8 m²), so the switch between them is small. Both are one-compartment models with fast absorption; Ostermann's also carries a small cerebrospinal-fluid compartment.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.
