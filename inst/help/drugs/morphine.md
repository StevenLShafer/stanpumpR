### The model

Morphine's parameters are from Lötsch and colleagues (*Clin Pharmacol Ther* 2002;72:151-162), a study of morphine and its glucuronide metabolites in volunteers. The model is written as a central volume of 0.25 L/kg with fixed rate constants; at 70 kg the volumes are 17.5, 86 and 196 L and the clearances 1.23, 2.23 and 0.32 L/min.

### Covariates

Weight only, scaling every volume and clearance linearly.

### Effect site

The time to peak effect is 93.8 minutes, the slowest in the library by far. Morphine is relatively hydrophilic and crosses the blood-brain barrier slowly, so its effect-site concentration lags the plasma by well over an hour. A bolus of morphine therefore never achieves an effect-site concentration near its plasma peak, and the clinical corollary is that titrating morphine to effect every five minutes overshoots: the effect of the previous dose has barely begun.

### MEAC and typical concentrations

MEAC is 8 ng/mL (0.008 mcg/mL in the plotted units) and the shaded band 6.4 to 16 ng/mL.

### Active metabolite

Morphine-6-glucuronide is an active metabolite that accumulates in renal failure. It is not in the current model; the active-metabolite work in development models morphine as the metabolite of codeine, and a metabolite link for morphine itself could follow. See [In development](help:in-development).

### Where to be careful

Renal function, which governs the metabolite, is not a covariate. The model describes young healthy volunteers; the elderly and the renally impaired lie outside it. The naloxone scenario shows how morphine's slow kinetics outlast its antagonist: [Naloxone after morphine](scenario:naloxone-morphine).
