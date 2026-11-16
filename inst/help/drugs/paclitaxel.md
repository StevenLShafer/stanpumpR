### One name, the model chosen for the patient

stanpumpR holds 2 published models of solvent-based paclitaxel. You do not choose between them: stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [paclitaxelCrombag](help:drugs/paclitaxelCrombag) | Crombag 2019 | 684 adults, 18-84 years, a quarter of them 70 or older; 1- or 3-hour infusions |
| [paclitaxelJoerger](help:drugs/paclitaxelJoerger) | Joerger 2006 | 168 adults, 33-86 years; mostly 3-hour infusions |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for paclitaxel.** Every patient up to 84 gets Crombag 2019, the larger study, a refit of Joerger's model with Joerger's patients among its 684. Patients of 85 and over get Joerger 2006, whose oldest patient was 86. The two share one structure: saturable elimination, saturable passage into the tissues and ordinary distribution into a second tissue compartment. They differ in where the covariates sit: Crombag puts sex, body-surface area and age on the maximal elimination rate only, Joerger on several parameters.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.
