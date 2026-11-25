### The model

Fluorouracil (5-FU) comes from Briki and colleagues (*Cancer Chemother Pharmacol* 2026;96:63), a meta-analysis of individual patient data: 2332 plasma concentrations from 726 adults with cancer, 24 to 90 years old, pooled from two published studies with rich sampling (Terret 2000, Mueller 2013) and routine drug monitoring at the Dijon cancer centre. The patients received LV5FU2, FOLFOX, FOLFIRI or FOLFIRINOX: a bolus, a 46-hour infusion, or both. Two compartments describe the disposition, and 5-FU leaves the central compartment by two routes at once: a **saturable** pathway (its maximal rate Vmax 411 mg/h, half-saturated at Km 1.89 mg/L) and a **linear** clearance of 59.2 L/h. The central volume is 9.58 L, the peripheral volume 13.6 L and the intercompartmental clearance 112 L/h.

These are the values of the authors' **alternative model**, in the paper's supplement (Table S3), which puts lean body mass on Vmax where their main model (Table 3) put sex. Steven Shafer chose it (25 November 2026): a sex term on a clearance with no weight term stands in for body size, and gives no rational way to scale a patient unlike the study's. Lean mass explains the sex difference completely. The study's men carried about 56 kg of lean mass and its women about 39.5 kg, and the model's lean-mass exponent turns that into the 18% sex difference of the main model. Once lean mass was in the model, sex added nothing.

Because elimination saturates, doubling a dose more than doubles the concentration. The engine integrates the whole dose table numerically, because saturable elimination is not superposable, so a bolus given during an infusion is cleared more slowly than the same bolus given alone.

### Covariates

- **Lean body mass** on Vmax: (LBM/55 kg)^0.484. Men's Vmax is higher than women's because they carry more lean mass, not because of a separate term for sex. With [the scaling switch](help:models/fat-free-mass) on, the lean mass is the patient's fat-free mass; with it off, Janmahasatian's lean body weight, as published.
- **Age** on Vmax: 1 − 0.292 × (age − 60)/60, about 18% lower at 80 years than at 40 for the same lean mass. It is held within the 24 to 90 years studied.
- **Body surface area** on the linear clearance: (BSA/1.7)^1.62, with BSA by Mosteller's formula.
- **Weight** on the peripheral volume, in proportion.

With the switch on, the BSA and weight terms use the pharmacokinetic weight (the weight that gives the patient's fat-free mass); with it off they use total body weight, as published. Renal function and the DPD phenotype were tested and not retained.

The central volume and the intercompartmental clearance have no size term of their own, so with the switch on they belong to the study's typical patient, not to stanpumpR's 70-kg reference man, and are scaled from that patient by fat-free mass: the medians of the three pooled studies (63 to 69 years, 67 to 76 kg, 168 to 171 cm; 422 men and 304 women). A 70-kg, 170-cm man therefore receives a central volume of 10.7 L and an intercompartmental clearance of 121 L/h, rather than the 9.58 L and 112 L/h of the study's patients. With the switch off, the published values are used.

### What is plotted

Plasma 5-FU in mcg/mL (mg/L). Doses are entered in mg, or mg/hr for the infusion, and a dose prescribed per m² as such, in mg/m² or mg/m²/hr (body surface area by Mosteller, from the Patient Profile's weight and height). There is no effect site.

The target for colorectal cancer is an **AUC of 20 to 30 mg·h/L over the 46-hour infusion**, the range recommended by the International Association of Therapeutic Drug Monitoring and Clinical Toxicology (Beumer 2019, doi:10.1002/cpt.1124), which is the plateau concentration times 46 hours; Briki's own dose-adjustment grid uses 20 to 29. That is an exposure, not a concentration band, and it is not drawn. A plateau of 0.43 to 0.65 mg/L meets it for that regimen only. For a 70-kg, 175-cm man of 60, 2400 mg/m² over 46 hours gives a plateau of about 0.38 mg/L, an AUC of about 18 mg·h/L, below the target, as the paper found for most patients dosed by surface area.

### Where to be careful

- **Interactions are not modelled.** The fluorouracil label warns that the INR may rise in patients taking warfarin (5.9, 7), so it should be checked more often; capecitabine, an oral 5-FU prodrug, also raises phenytoin levels (Xeloda label, 7.2). Neither the library's warfarin nor its phenytoin curve shows these effects.
- **DPD deficiency is not represented.** A patient with a DPYD variant or low DPD activity clears 5-FU far more slowly than this curve shows, and can be fatally overexposed. The CPIC and DPWG guidelines recommend dose reductions for them; those are not clearance estimates, and none is applied here.
- **The slow rise during the infusion.** 5-FU inhibits its own metabolism, and in patients the plateau takes 18 to 26 hours to settle. The model has no time-dependent term, so its plateau comes within hours. Circadian variation is not modelled either.
- **Adults only.**
- **Not a monitoring tool.** The curve is a population prediction for the covariates entered. It is not an estimate from a measured level, and it does not replace monitoring or the published dose-adjustment tables.

Every parameter and equation was confirmed against the paper's PDF and its supplement (Table S3 and its equations). See [chemotherapy](help:models/chemotherapy) for the other models considered.
