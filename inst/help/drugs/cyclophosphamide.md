### One name, the model chosen for the patient

stanpumpR holds 2 published models of cyclophosphamide. You do not choose between them: stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [cyclophosphamideJoerger](help:drugs/cyclophosphamideJoerger) | Joerger 2007 | 59 women with breast cancer (age range not printed; taken as 18 and over) |
| [cyclophosphamideMcCune](help:drugs/cyclophosphamideMcCune) | McCune 2009 | 22 children with neuroblastoma, 1.3-9.4 years, 8.7-52.8 kg; autoinduction |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for cyclophosphamide.** Children up to about 13½ years get McCune 2009, in which the drug induces its own clearance over a course of daily doses; adults, and adolescents nearer 18 than 9.4, get Joerger 2007, one compartment with no induction (fitted to one dose per patient).

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.
