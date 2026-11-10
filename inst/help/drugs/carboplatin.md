### One name, the model chosen for the patient

stanpumpR holds 2 published models of carboplatin. You do not choose between them: most clinicians have no data to make that choice, so stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [carboplatinDuffull](help:drugs/carboplatinDuffull) | Duffull 1997 | 12 women with ovarian cancer, 49-68 years, 46-85 kg |
| [carboplatinPediatric](help:drugs/carboplatinPediatric) | Duong 2019 | 51 children with high-risk neuroblastoma, 1.7-8.3 years, 8.6-31 kg |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for carboplatin.** Duong 2019 is chosen for young children and Duffull 1997 for adults; between their ranges the nearer one is used, so the switch comes at about 28-29 years.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.
