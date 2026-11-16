### One name, the model chosen for the patient

stanpumpR holds 3 published models of doxorubicinol. You do not choose between them: most clinicians have no data to make that choice, so stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [doxorubicinolJoerger](help:drugs/doxorubicinolJoerger) | Joerger 2007 | formed by Joerger's doxorubicin |
| [doxorubicinolPediatric](help:drugs/doxorubicinolPediatric) | Voller 2015 | formed by Voller's doxorubicin |
| [doxorubicinolPerezBlanco](help:drugs/doxorubicinolPerezBlanco) | Perez-Blanco 2016 | formed by Perez-Blanco's doxorubicin |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for doxorubicinol.** The model always comes from the same source as the doxorubicin that forms it, chosen by the same rule.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.
