### One name, the model chosen for the patient

stanpumpR holds 2 published models of ambrisentan. You do not choose between them: stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [ambrisentanOkour](help:drugs/ambrisentanOkour) | Okour 2023 | 39 children with pulmonary arterial hypertension, 8-16 years |
| [ambrisentanMiura](help:drugs/ambrisentanMiura) | Miura 2026 | children with pulmonary arterial hypertension, 0.4-8.8 years, 7.1-21.6 kg |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for ambrisentan.** Children under 8 years get Miura 2026, on Steven Shafer's decision of 18 November 2026 ("Use Miura's model for children"); the table gives Miura's model the range of its children only. From 8 years Okour's larger study includes the child, and adults get Okour's model as the nearest, since no adult model has been read.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.
