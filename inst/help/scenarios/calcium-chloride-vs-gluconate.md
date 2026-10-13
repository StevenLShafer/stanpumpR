## What to look for

A typical parturient of the source study (34 years, 80 kg, 162 cm) receives **1 g of calcium chloride** as 100 mg/min for 10 minutes and, in the other panel, **3 g of calcium gluconate** as 300 mg/min for 10 minutes. Each panel is the **change in venous ionized calcium** above her own baseline, in mmol/L, not a concentration of the salt; her baseline (about 1.15 mmol/L in the study) is not part of the model, so both curves start at zero. *Adjust weight to fat-free mass* is unticked, so the published parameters apply unscaled and the plot reproduces the paper's figure 5.

Calcium chloride rises to 0.24 mmol/L at 5 minutes and peaks at **0.39 mmol/L** at the end of the infusion. It then falls fast, with a 3.5-minute distribution half-life, to 0.27 at 15 minutes and 0.19 at 30: the peak has **halved 29 minutes** after the infusion began, the study's "half of the peak in about 30 minutes". By 60 minutes it is 0.16 mmol/L, and the decline has slowed to the 109-minute half-life of the model, which is as far as the data go.

Calcium gluconate gives the same curve, 4% lower: 0.375 mmol/L at 10 minutes, 0.19 at 30, 0.15 at 60. The model's fitted dose ratio is 3.11 g of calcium gluconate per gram of calcium chloride, so 3 g is 96% of the dose. That is the bioequivalence the study was designed to measure, and the reason the two salts can be used interchangeably at a 3:1 ratio. There is no shaded band and no effect-site curve: the ionized calcium is the end point.

## Try next

- **The dosing error that prompted the study.** Change the calcium gluconate rate to **100 mg/min**, 1 g over 10 minutes. The peak falls to 0.13 mmol/L, a third of calcium chloride's: 1 g of calcium gluconate is not 1 g of calcium chloride, and the authors cite deaths from the substitution in hyperkalemic patients.
- **A bolus.** Replace the calcium chloride rows with **1 g** at time 0. The model's peak jumps to 0.62 mmol/L (6.8 mmol into a 10.9 L central volume) and is down to 0.26 by 10 minutes. The study never gave a bolus, so this peak is an extrapolation of a model fitted to 10-minute infusions; the curves coincide with the infused dose from about 30 minutes.
- **Fat-free mass on.** Tick *Adjust weight to fat-free mass*. This 80 kg woman's fat-free mass is below the 70 kg reference man's, so her central volume falls from 10.9 to 9.3 L and the calcium chloride peak rises to 0.45 mmol/L. The source found no effect of weight between 55 and 100 kg; the scaling is the library's assumption so that children and very large patients get a proportionate volume (see [Fat-free mass](help:models/fat-free-mass)).
- **A child.** Set 5 years, 20 kg, 110 cm, male, with the switch on, and give calcium chloride **20 mg/kg** (0.4 g) as a bolus: 0.85 mmol/L. With the switch off the child would have an adult's 10.9 L and the same dose would raise the ionized calcium by only 0.25 mmol/L. No children were in the study; this is what the scaling assumption does, not a measurement.
- **Beyond the first hour.** Set Max time to 240 minutes. Calcium chloride is at 0.11 mmol/L at 2 hours and 0.05 at 4 hours. Sampling stopped at 60 to 90 minutes, and the authors expect a slower terminal phase than this, so the model returns to zero faster than ionized calcium really does.

## Background

[Calcium chloride](help:drugs/calciumChloride) describes the study (Ansari and colleagues, *Anesthesiology* 2025), the parturient population, why the model is offered to every patient, and what it leaves out. [Calcium gluconate](help:drugs/calciumGluconate) explains the dose ratio and the package inserts' maximal rates.
