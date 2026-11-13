### One name, the model chosen for the patient

stanpumpR holds 2 published models of vincristine. You do not choose between them: most clinicians have no data to make that choice, so stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [vincristineIgarashi](help:drugs/vincristineIgarashi) | Igarashi 2021 | 30 Japanese adults, 25-82 years |
| [vincristinePediatric](help:drugs/vincristinePediatric) | Nijstad 2022 | 206 children and young adults, 0.04-33.9 years, 2.9-126 kg |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for vincristine.** Nijstad 2022 is chosen up to 33.9 years (and for anyone it covers), being the larger study; Igarashi 2021 for older adults.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.
