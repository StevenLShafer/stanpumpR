### One name, the model chosen for the patient

stanpumpR holds 2 published models of melphalan. You do not choose between them: most clinicians have no data to make that choice, so stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [melphalanNath](help:drugs/melphalanNath) | Nath 2010 | 100 adults with myeloma, 36-73 years, 42-132 kg |
| [melphalanPediatric](help:drugs/melphalanPediatric) | Duong 2019 | 51 children with high-risk neuroblastoma, 1.7-8.3 years, 8.6-31 kg |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for melphalan.** Duong 2019 is chosen for young children and Nath 2010 for adults; between their ranges the nearer one is used, so the switch comes at about 22 years.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.
