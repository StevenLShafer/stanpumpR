### One name, the model chosen for the patient

stanpumpR holds 3 published models of doxorubicin. You do not choose between them: most clinicians have no data to make that choice, so stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [doxorubicinJoerger](help:drugs/doxorubicinJoerger) | Joerger 2007 | 59 women with breast cancer (age range not printed; taken as 18 and over) |
| [doxorubicinPediatric](help:drugs/doxorubicinPediatric) | Voller 2015 | 94 children, 0.2-17.7 years |
| [doxorubicinPerezBlanco](help:drugs/doxorubicinPerezBlanco) | Perez-Blanco 2016 | 44 adults with non-Hodgkin lymphoma, 26-84 years, 43-110 kg |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for doxorubicin.** Voller 2015 is chosen for children and Joerger 2007 for adults (59 patients against Perez-Blanco's 44), so Perez-Blanco 2016 is in the library but not currently chosen for anyone. Each forms its own doxorubicinol, and the doxorubicinol panel always comes from the same source as the doxorubicin.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.
