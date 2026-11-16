### One name, the model chosen for the patient

stanpumpR holds 2 published models of methotrexate. You do not choose between them: most clinicians have no data to make that choice, so stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [methotrexateIbarra](help:drugs/methotrexateIbarra) | Ibarra 2023 | 243 adults, 18-88 years, 44-162 kg |
| [methotrexatePediatric](help:drugs/methotrexatePediatric) | Taylor 2023 | 1,668 children and young adults, 0.1-32.3 years |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for methotrexate.** Taylor 2023 (MTXPK.org) is chosen up to 32.3 years, including young adults, because it is the larger study; Ibarra 2023 above that.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.
