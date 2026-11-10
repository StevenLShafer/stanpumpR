### One name, the model chosen for the patient

stanpumpR holds 2 published models of oxaliplatin. You do not choose between them: most clinicians have no data to make that choice, so stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [oxaliplatinBastian](help:drugs/oxaliplatinBastian) | Bastian 2003 | 56 adults (age range not printed; taken as 18 and over), 41-107 kg; a gallbladder store |
| [oxaliplatinNikanjam](help:drugs/oxaliplatinNikanjam) | Nikanjam 2015 | 186 children and adults, 0.6-86 years, 7-165 kg |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for oxaliplatin.** Nikanjam 2015 covers almost everyone and is the larger study, so it is chosen from 0.6 to 86 years; Bastian 2003 only beyond 86 years (or beyond Nikanjam's weights). Both plot ultrafilterable platinum, here in ng/mL.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.
