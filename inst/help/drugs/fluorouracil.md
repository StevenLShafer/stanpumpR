### One name, the model chosen for the patient

stanpumpR holds 2 published models of fluorouracil. You do not choose between them: most clinicians have no data to make that choice, so stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [fluorouracilBriki](help:drugs/fluorouracilBriki) | Briki 2026 | 726 adults, 24-90 years, 32-126 kg |
| [fluorouracilTan](help:drugs/fluorouracilTan) | Tan 2026 | 140 adults (age range not printed; taken as 18 and over) |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for fluorouracil.** Briki 2026 is chosen for adults of 24-90 years and 32-126 kg; Tan 2026 only outside those ranges.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.
