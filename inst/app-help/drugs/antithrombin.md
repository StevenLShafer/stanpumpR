### One name, the model chosen for the patient

stanpumpR holds 2 published models of plasma-derived antithrombin III. You do not choose between them: stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use. The US product is Thrombate III; recombinant antithrombin alfa (ATryn), a different molecule with a much shorter half-life, is a drug of its own, [antithrombinAlfa](help:drugs/antithrombinAlfa).

| Model | Source | Studied in |
|---|---|---|
| [antithrombinMoffett](help:drugs/antithrombinMoffett) | Moffett 2017 | 184 children under 19 years given Thrombate III, mostly infants, most on heparin |
| [antithrombinIlias](help:drugs/antithrombinIlias) | Ilias 2000 | 32 adults with severe sepsis, 18-85 years, given Kybernin P |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen.

**What that means for antithrombin** (Steven L. Shafer, 2026-11-26). Patients under 19 years get Moffett 2017, and older patients get Ilias 2000. The two ranges meet from 18 to 19 years, where both studies include the patient and Moffett's is the larger. The two models differ:

- At 70 kg, Moffett's half-life is 51 hours and Ilias's 19 hours; the rise for each IU/kg is 1.03 and 1.56 IU/dL.
- In Moffett's model the baseline also sets the volume; in Ilias's it is only plotted, because each patient's baseline was subtracted before the fit.
- Left blank, the **Antithrombin baseline** is each source's own: 60 IU/dL for Moffett, 63 for Ilias, the median of its septic adults.

**Both describe patients consuming antithrombin.** Moffett's children were critically ill and Ilias's adults septic. In a patient with hereditary deficiency who is not consuming it, the Thrombate III label gives a half-life of 3.8 days (8 patients, functional assay), so the activity will fall more slowly than either model draws.

The tables on this page are computed at six reference patients, so each uses the model chosen for that patient. Each model's own page gives its parameters, its checks against its paper, and its limits.

The band is the label's target, 80 to 120% of normal (IU/dL).
