### One name, the model chosen for the patient

stanpumpR holds 2 published models of tadalafil. You do not choose between them: stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [tadalafilFergusonSells](help:drugs/tadalafilFergusonSells) | Ferguson-Sells 2022 | 324 patients with pulmonary arterial hypertension, 2.5-90.3 years (with the **Pulmonary hypertension** field at No, the FDA's model in men with erectile dysfunction) |
| [tadalafilMiura](help:drugs/tadalafilMiura) | Miura 2026 | children with pulmonary arterial hypertension, 0.1-8.3 years, 5.4-23.9 kg |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for tadalafil.** Steven Shafer decided on 18 November 2026 that Miura's model is the children's, so the table gives it the range of Miura's children. Ferguson-Sells 2022 also studied children, from 2.5 years, in a much larger study, so by the rule children from 2.5 years keep Ferguson-Sells and Miura's model is chosen below 2.5 years. Where Miura's model is chosen it is used whatever the **Pulmonary hypertension** field says.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.
