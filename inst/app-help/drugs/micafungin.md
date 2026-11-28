### One name, the model chosen for the patient

stanpumpR holds 2 published models of micafungin. You do not choose between them: stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [micafunginJullien](help:drugs/micafunginJullien) | Jullien 2017 | 99 ventilated intensive-care patients with sepsis, 29.9-92.7 years, 48-141 kg |
| [micafunginWasmann](help:drugs/micafunginWasmann) | Wasmann 2019 | 24 adults, 8 of normal weight and 16 morbidly obese, 22-61 years, 61.5-184 kg |

**The rule.** Among the models whose studied age and weight ranges include the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for micafungin.** Most adults get Jullien's intensive-care model, the larger study. Adults younger than 29.9 years, and those heavier than 141 kg up to 61 years, get Wasmann's. A patient over 61 and heavier than 141 kg is inside neither study and gets Jullien's, which is nearer by age. Neither source studied children; a child gets Wasmann's, nearer by age, which is an extrapolation. The label's pediatric data are summaries, not a model.

### Two different populations

At 70 kg the two models' clearances are 1.2 L/h (Jullien) and 0.69 L/h (Wasmann). The intensive-care patients cleared micafungin faster than the healthy volunteers, as their low albumin would predict for a drug more than 99% bound to it. The label's patients, 0.20 to 0.36 mL/min/kg, about 0.8 to 1.5 L/h at 70 kg, lie between and above.

### Entering a dose

The label says micafungin is given "by intravenous infusion only" and infused over one hour; faster infusions bring more histamine-mediated reactions. So stanpumpR offers only rates: 100 mg is entered as 100 mg/hr from 0 to 60 minutes, and each day's dose as its own one-hour infusion. A dose per kilogram is entered the same way in mg/kg/hr.

### Target concentrations

No band is drawn. Both sources judge exposure by the ratio of the daily area under the curve to the organism's MIC (above 5000 for most *Candida*, above 285 for *C. parapsilosis*), not by a concentration. The label gives no therapeutic range.

### Pregnancy

No pharmacokinetic study of micafungin in pregnancy was found. Micafungin is metabolized and excreted mainly in the faeces, so the rise in filtration of pregnancy does not reach it, and nothing is applied.
