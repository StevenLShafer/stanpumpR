### One name, the model chosen for the patient

stanpumpR holds 2 published models of teicoplanin. You do not choose between them: stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [teicoplaninKim](help:drugs/teicoplaninKim) | Kim 2024 | 12 healthy Korean adults, 19-55 years, rich sampling to 10 days |
| [teicoplaninZhang](help:drugs/teicoplaninZhang) | Zhang 2020 | 159 hospitalised Chinese children, 0.2-14 years, 2.9-69 kg, mostly troughs |

**The rule.** Among the models whose studied age and weight ranges include the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for teicoplanin.** Every adult gets Kim's model: from 19 to 55 years it contains the patient, and an older adult is nearer to it than to the children's model. Children up to 16.5 years get Zhang's. Shafer chose this pairing on 27 November 2026.

### A model that was not used

Wang and colleagues (*Drug Des Devel Ther* 2023;17:2259-2271) fitted 108 critically ill adults. Their assay measured only the A2-2 component of teicoplanin, about half of the drug, and the paper does not say whether the results were scaled to total teicoplanin. Their clearance is about twice Kim's. stanpumpR does not use the model.

### The MIC line and the band

The MIC line is *Staphylococcus aureus*, including MRSA, at 2 mg/L free drug, the top of the EUCAST distribution Kim used. Teicoplanin is 90% or more bound, so the line is drawn at 20 mg/L on the total curve.

The band is **not** an MBC band, as most antibiotics' are. Teicoplanin's MBCs against *S. aureus* are too often those of tolerant isolates to place one: Biedenbach and colleagues (2007) found an MBC/MIC ratio of 32 or more in a share of staphylococci, and Maple and colleagues (1989) an MBC90 above 32 mg/L at 22 hours. Its band is the trough the ESICM/ESCMID position paper recommends for monitoring (Abdul-Aziz and colleagues, *Intensive Care Med* 2020;46:1127-1153, doi 10.1007/s00134-020-06050-1, Table 4): at least 15-30 mg/L of total teicoplanin, one sample 30 minutes before or just before the next dose. The paper's text gives 10-20 mg/L for uncomplicated infections and 20-30 mg/L for severe staphylococcal infections, and nephrotoxicity becomes more likely above 60 mg/L. It is a trough range, drawn against the whole curve.

### No US label

Teicoplanin is not approved in the United States. It is marketed in Europe and Asia as Targocid, and doses are entered as labelled there.

### Pregnancy

No pharmacokinetic study of teicoplanin in pregnancy was found. Teicoplanin is cleared by the kidneys, so with **Pregnant** ticked and the serum creatinine blank, the adult model's eGFR takes the 50% rise in filtration of late pregnancy.
