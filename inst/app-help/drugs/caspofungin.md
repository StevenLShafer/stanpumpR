### One name, the model chosen for the patient

stanpumpR holds 2 published models of caspofungin. You do not choose between them: stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [caspofunginWurthwein](help:drugs/caspofunginWurthwein) | Würthwein 2013 | 46 German adults with invasive aspergillosis, 18-74 years, 43-104 kg, 70-200 mg daily |
| [caspofunginXu](help:drugs/caspofunginXu) | Xu 2026 | 29 critically ill Chinese children, 0.33-16 years, 4.9-74 kg |

**The rule.** Among the models whose studied age and weight ranges include the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for caspofungin.** Adults get Würthwein's model, including adults older than 74 or outside 43-104 kg, who are nearer to it than to the children's. Children up to 16 get Xu's. A 17-year-old is one year from each range; the tie goes to Würthwein's larger study. Steven Shafer chose this pairing on 28 November 2026.

### Doses

Doses are in mg of caspofungin, as CANCIDAS is labelled: a 50 mg vial holds caspofungin acetate equivalent to 50 mg of caspofungin. The label forbids an intravenous bolus, so a dose is entered as a rate and stopped when the infusion ends. Adults are dosed as a total (for example 50 mg/hr from 0 to 60 minutes) and children per m² (50 mg/m²/hr from 0 to 60 minutes).

### Target concentrations

No band is drawn. Both sources report exposure as the daily AUC and the trough, and Xu judges it by the ratio of the AUC to the organism's MIC, not by a concentration range. The label gives no therapeutic range.

### Pregnancy

No pharmacokinetic study of caspofungin in pregnancy was found. Caspofungin is slowly metabolised by hydrolysis and N-acetylation, and about 1.4% of a dose is excreted unchanged in the urine, so the rise in filtration of pregnancy does not reach it. Nothing is applied.
