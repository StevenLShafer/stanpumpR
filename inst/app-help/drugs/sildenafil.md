### One name, the model chosen for the patient

stanpumpR holds 2 published models of sildenafil. You do not choose between them: stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [sildenafilFDA](help:drugs/sildenafilFDA) | FDA review of NDA 22-473 | 45 healthy volunteers (taken as adults, 18 years and over); with the **Pulmonary hypertension** field, clearance 30% lower |
| [sildenafilMiura](help:drugs/sildenafilMiura) | Miura 2026 | children with pulmonary arterial hypertension, 0.4-14.8 years, 6.5-54.9 kg |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for sildenafil.** Children get Miura 2026, on Steven Shafer's decision of 18 November 2026 ("Use Miura's model for children"); the table gives Miura's model the range of its children only. Adults, and adolescents from about 16.4 years, get the FDA review's model. A child gets Miura's model whatever the **Pulmonary hypertension** field says, sildenafil being given to children for pulmonary arterial hypertension. Miura fitted oral doses only, so in a child an intravenous dose is still run on the FDA review's model; the two are added.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.
