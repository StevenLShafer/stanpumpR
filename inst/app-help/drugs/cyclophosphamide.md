### One name, the model chosen for the patient

stanpumpR holds 2 published models of cyclophosphamide. You do not choose between them: stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [cyclophosphamideJoerger](help:drugs/cyclophosphamideJoerger) | Joerger 2007 | 59 women with breast cancer, 29-81 years |
| [cyclophosphamideMcCune](help:drugs/cyclophosphamideMcCune) | McCune 2009 | 22 children with neuroblastoma, 1.3-9.4 years, 8.7-52.8 kg; autoinduction |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for cyclophosphamide.** Children and adolescents up to about 19 years (the midpoint of McCune's 9.4 and Joerger's 29) get McCune 2009, in which the drug induces its own clearance over a course of daily doses; older patients get Joerger 2007, one compartment with no induction (fitted to one dose per patient).

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.

**Routes not modelled.** Cyclophosphamide is given intravenously and as oral tablets (the tablet label is the one listed on this page). Only the intravenous route is modelled here, because the models were fitted to intravenous doses. If you need a route stanpumpR does not offer, please contact Steven Shafer (steven.shafer@stanford.edu). For a one-time simulation in the meantime, a published pharmacokinetic study of that route can often be uploaded to the **Single Paper Review** tab, which reads its model and simulates it.
