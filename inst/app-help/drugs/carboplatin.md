### One name, the model chosen for the patient

stanpumpR holds 3 published models of carboplatin. You do not choose between them: most clinicians have no data to make that choice, so stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [carboplatinDuffull](help:drugs/carboplatinDuffull) | Duffull 1997 | 12 women with ovarian cancer, 49-68 years, 46-85 kg |
| [carboplatinPediatric](help:drugs/carboplatinPediatric) | Duong 2019 | 51 children with high-risk neuroblastoma, 1.7-8.3 years, 8.6-31 kg |
| [carboplatinPeng](help:drugs/carboplatinPeng) | Peng 1995 | 22 children with cancer (tested in 23 aged 1 month to 18.4 years, 5-63 kg) |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for carboplatin.** Duong 2019 is chosen for children 1.7 to 8.3 years who weigh 8.6 to 31 kg; Peng 1995 for infants, for children from 8.3 to 18.4 years, and for a younger child outside Duong's weights; Duffull 1997 for adults. Between Peng's range and Duffull's the nearer one is used, so the switch to Duffull comes at about 34 years. Peng 1995 was added on Steven Shafer's decision of 17 November 2026, so that infants and teenagers are given a model fitted in children of their ages rather than the nearest one.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.
