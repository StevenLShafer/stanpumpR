### The model

Codeine's disposition is **one compartment**, built from two published summaries of intravenous codeine: Persson and colleagues (*Eur J Clin Pharmacol* 1992;42:663-666) reported a total clearance of 10.8 mL/min/kg, and Guay and colleagues (*Clin Pharmacol Ther* 1988;43:63-71) a mean residence time of 3.90 hours. Together those fix clearance and steady-state volume, which at 70 kg are 0.76 L/min and 177 L. That is all the published literature identifies.

A second compartment is indicated (Guay's terminal half-life of 4.0 hours exceeds the 2.7 hours this model gives) but cannot be separated from the summaries, so the code deliberately does not invent one. The model reproduces the correct area under the curve, mean residence time and oral peak, and understates the terminal half-life. Codeine is given by mouth in almost all real use, where absorption governs the early curve and the distribution phase is largely invisible.

### A prodrug

Codeine has negligible affinity for the μ-opioid receptor; its analgesia is that of the **morphine** formed from it by CYP2D6. The model therefore gives codeine **no effect site**: tPeak is zero, ke0 is zero, only the plasma concentration is plotted, and MEAC is zero. The shaded band (50 to 150 ng/mL) is a range of plasma concentrations after ordinary oral doses, not a therapeutic effect-site range. The effect appears on the [morphine](help:drugs/morphine) row, which receives the formed metabolite whether or not morphine itself was given. See [Active metabolites](help:models/metabolites).

### Oral absorption

The absorption rate constant (0.0426/min, an absorption half-time of about 16 minutes) puts the plasma peak at 60 minutes, matching Chen 1991 and Shah 1990. Bioavailability is 0.5, from Spahn 1985 and Hull 1982; Persson found 12 to 84 per cent between subjects, so this is a central value for a genuinely wide distribution.

### Morphine formation

Only the *ratio* of formation to the metabolite's clearance is identifiable from concentration data, so the formation fraction was taken from Ashraf and colleagues' phenotype analysis (*Clin Pharmacokinet* 2024;63:1547-1560) and rescaled from the morphine clearance they fitted (357.5 L/h) to that of the Lötsch morphine model used here (75.3 L/h). Formation is 2.9 per cent of codeine's clearance in a normal metaboliser. A small first-pass branch (0.15 per cent of the dose) adds morphine formed before codeine reaches the circulation; its size is bounded by the observed time of the morphine peak, since a larger value would make the simulated morphine curve bimodal.

For 60 mg of oral codeine in a 70 kg normal metaboliser the model gives a codeine peak of about 130 ng/mL at 60 minutes and a morphine peak of about 1.4 ng/mL near 110 minutes. Its morphine-to-codeine exposure ratio (0.014) runs 30 to 50 per cent below the two small studies that measured it (0.020 and 0.027). The code leaves that disagreement standing rather than tuning it away, because the Ashraf value is traceable and phenotype-resolved.

### CYP2D6 phenotype

The **CYP 2D6** field in the Patient Profile scales formation by both routes. The weights (poor 0.035, intermediate 0.46, normal 1, ultrarapid 1.55) are derived from the median formation fractions Ashraf reported by phenotype group. Because the CYP2D6 branch is only about 3 per cent of codeine's clearance, codeine's own curve barely changes between phenotypes, consistent with the clinical finding that poor metabolisers have ordinary codeine concentrations and little analgesia. The table above shows the effect at the reference adult.

### Covariates

Weight, through the per-kilogram clearance and volume. Under the default [fat-free-mass scaling](help:models/fat-free-mass) the volume scales with fat-free mass relative to the reference man and the clearance with that ratio to the 0.75 power; with the switch off both scale linearly with weight. The formation rate constant is a clearance over a volume, so it falls slowly with size. Age, height and sex do not enter except through fat-free mass.

### Where to be careful

Codeine-6-glucuronide, morphine-3-glucuronide, norcodeine and morphine-6-glucuronide are not modelled. The last of these is active and is the obvious next addition, but it would need a two-stage cascade (codeine to morphine to morphine-6-glucuronide), which the engine does not yet do. The one-compartment disposition misstates the first minutes after an intravenous dose and the tail after many hours. The clinical warnings about codeine in ultrarapid metabolisers and in children rest on morphine exposure, which is exactly what this model displays; the model is not a safety assessment. See [the codeine scenario](scenario:codeine-cyp2d6).
