### The model

Morphine's parameters are from Lötsch and colleagues (*Clin Pharmacol Ther* 2002;72:151-162), a study of morphine and its glucuronide metabolites in volunteers. The model is written as a central volume of 0.25 L/kg with fixed rate constants; at 70 kg the volumes are 17.5, 86 and 196 L and the clearances 1.23, 2.23 and 0.32 L/min.

### Oral: tablet and liquid

Oral morphine is entered as **mg PO tablet** (immediate-release tablet) or **mg PO liquid** (oral solution), each also available qd, bid, tid or qid. Both are calibrated to one crossover study of 30 mg morphine sulfate in fasted volunteers (Atrux-Tallau and colleagues, *Clin Drug Investig* 2022;42:1101-1112): Sevredol tablets peaked at 28.5 ng/mL and Oramorph solution at 37.9 ng/mL, with the same total exposure. Doses are labelled mg of morphine sulfate, as prescribed.

The two forms share Lötsch's disposition and effect site and differ only in absorption. Bioavailability is 0.29 for the tablet and 0.30 for the liquid; both start to be absorbed 17.6 minutes after the dose, after which the liquid is absorbed faster (half-time 39 minutes against the tablet's 53). The plasma peak is about 36 minutes after liquid and 45 minutes after a tablet, and the liquid's peak is a third higher. Because morphine's effect site is so slow, the difference in peak effect is much smaller than the difference in plasma peak.

Modified-release morphine is not modelled, nor is food, which slows absorption. Oral morphine forms much more morphine-6-glucuronide than intravenous morphine, because of first-pass glucuronidation, and that active metabolite is not in the model (below), so the effect of an oral dose is understated.

### Covariates

Weight. Under the default [fat-free-mass scaling](help:models/fat-free-mass) the volumes scale with the patient's fat-free mass and the clearances with that ratio to the 0.75 power; with the switch off, every volume and clearance scales linearly with weight, as Lötsch's per-kilogram model did.

### Effect site

The time to peak effect is 93.8 minutes, the slowest in the library by far. Morphine is relatively hydrophilic and crosses the blood-brain barrier slowly, so its effect-site concentration lags the plasma by well over an hour. A bolus of morphine therefore never achieves an effect-site concentration near its plasma peak, and the clinical corollary is that titrating morphine to effect every five minutes overshoots: the effect of the previous dose has barely begun.

### MEAC and typical concentrations

MEAC is 8 ng/mL (0.008 mcg/mL in the plotted units) and the shaded band 6.4 to 16 ng/mL.

### Active metabolite

Morphine-6-glucuronide is an active metabolite that accumulates in renal failure. It is not in the current model. Morphine is itself now the modelled metabolite of [codeine](help:drugs/codeine): giving codeine adds a morphine row, which is where codeine's analgesia appears. A second-generation link to morphine-6-glucuronide would need a two-stage cascade, which the engine does not yet do. See [Active metabolites](help:models/metabolites).

### Where to be careful

Renal function, which governs the metabolite, is not a covariate. The model describes young healthy volunteers; the elderly and the renally impaired lie outside it. The naloxone scenario shows how morphine's slow kinetics outlast its antagonist: [Naloxone after morphine](scenario:naloxone-morphine).
