### One name, the model chosen for the patient

stanpumpR holds 3 published models of etoposide. You do not choose between them: most clinicians have no data to make that choice, so stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [etoposideKroghMadsen](help:drugs/etoposideKroghMadsen) | Krogh-Madsen 2012 | 23 adults with AML, 22-68 years, 46-117 kg |
| [etoposidePediatric](help:drugs/etoposidePediatric) | Urien 2011 | 67 children, 14 weeks-16.7 years, 5.3-66 kg, conventional 1-h infusions |
| [etoposideDuong](help:drugs/etoposideDuong) | Duong 2019 | 51 children with high-risk neuroblastoma, 1.7-8.3 years, 8.6-31 kg, high-dose |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for etoposide.** In practice Urien 2011 is chosen for children (it is the larger of the two children's studies and covers Duong's ages), the nearer children's model up to about 19 years, and Krogh-Madsen 2012 for adults. Duong 2019 is never chosen while Urien covers the child.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.
