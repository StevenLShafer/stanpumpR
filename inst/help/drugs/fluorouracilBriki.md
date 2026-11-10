### The model

Fluorouracil (5-FU) comes from Briki and colleagues (*Cancer Chemother Pharmacol* 2026;96:63), a meta-analysis of individual patient data: 2332 plasma concentrations from 726 adults with cancer, 24 to 90 years old, pooled from two published studies with rich sampling (Terret 2000, Mueller 2013) and routine drug monitoring at the Dijon cancer centre. The patients received LV5FU2, FOLFOX, FOLFIRI or FOLFIRINOX: a bolus, a 46-hour infusion, or both. Two compartments describe the disposition, and 5-FU leaves the central compartment by two routes at once: a **saturable** pathway (its maximal rate Vmax 429 mg/h, half-saturated at Km 2.34 mg/L) and a **linear** clearance of 52.6 L/h. The central volume is 9.21 L, the peripheral volume 14.6 L and the intercompartmental clearance 125 L/h.

Because elimination saturates, doubling a dose more than doubles the concentration. The engine integrates the whole dose table numerically, because saturable elimination is not superposable, so a bolus given during an infusion is cleared more slowly than the same bolus given alone.

### Covariates

- **Body surface area** on the linear clearance: (BSA/1.7)^2.61, with BSA by Mosteller's formula. A BSA of 1.5 m² gives a linear clearance 53% lower than 2.0 m².
- **Sex** on Vmax: 18% higher in men. The authors read this as the effect of lean body mass, which their alternative model put in its place.
- **Age** on Vmax: about 18% lower at 80 years than at 40. It is held within the 24 to 90 years studied.
- **Weight** on the peripheral volume, in proportion.

With [the scaling switch](help:models/fat-free-mass) on, the weight and BSA terms use the pharmacokinetic weight (the weight that gives the patient's fat-free mass); with it off they use total body weight, as published. Renal function and the DPD phenotype were tested and not retained.

### What is plotted

Plasma 5-FU in mcg/mL (mg/L). Doses are entered in mg, or mg/hr for the infusion: a dose prescribed in mg/m² must first be multiplied by the patient's body surface area. There is no effect site.

The target for colorectal cancer is an **AUC of 20 to 29 mg·h/L over the 46-hour infusion**, which is the plateau concentration times 46 hours. That is an exposure, not a concentration band, and it is not drawn. A plateau of 0.43 to 0.63 mg/L meets it for that regimen only. For a 70-kg, 175-cm man of 60, 2400 mg/m² over 46 hours gives a plateau of about 0.38 mg/L, an AUC of about 18 mg·h/L, below the target, as the paper found for most patients dosed by surface area.

### Where to be careful

- **DPD deficiency is not represented.** A patient with a DPYD variant or low DPD activity clears 5-FU far more slowly than this curve shows, and can be fatally overexposed. The CPIC and DPWG guidelines recommend dose reductions for them; those are not clearance estimates, and none is applied here.
- **The slow rise during the infusion.** 5-FU inhibits its own metabolism, and in patients the plateau takes 18 to 26 hours to settle. The model has no time-dependent term, so its plateau comes within hours. Circadian variation is not modelled either.
- **Adults only.**
- **Not a monitoring tool.** The curve is a population prediction for the covariates entered. It is not an estimate from a measured level, and it does not replace monitoring or the published dose-adjustment tables.

Every parameter and equation was confirmed against the paper's PDF (its Table 3 and printed equations). See [chemotherapy](help:models/chemotherapy) for the other models considered.
