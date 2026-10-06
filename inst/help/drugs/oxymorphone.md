### The model

Oxymorphone's disposition is **one compartment**, built from the manufacturer's summary in the *Physicians' Desk Reference*: a clearance of 2.0 L/min and a steady-state volume of 3.08 L/kg. No central or peripheral split, no exchange clearance, and no population analysis supplying them, were found in the human literature. One compartment at the reported steady-state volume reproduces the clearance exactly and gives a terminal half-life of 1.2 hours against the reported 1.3. What it gets wrong is the first few minutes after an intravenous bolus, where a real central volume would give a higher early peak; treat early intravenous predictions with suspicion.

### Routes

Oxymorphone is offered by mouth (**mg PO**) and intravenously. Oral bioavailability is 0.10, from the product labelling, and the absorption rate constant is set so the predicted peak matches the 1.93 ng/mL Adams and Ahdieh measured after a 10 mg immediate-release tablet. Extended-release oxymorphone is a different input and is not represented.

### Also oxycodone's metabolite

[Oxycodone](help:drugs/oxycodone) names oxymorphone as its active metabolite, so a patient given oxycodone gets an oxymorphone row whether or not oxymorphone itself was given, and a patient given both sees the sum. The formation constant lives in the oxycodone model, calibrated against the roughly 2 per cent plasma ratio Agema and colleagues observed. See [Active metabolites](help:models/metabolites).

### Effect site and MEAC

The time to peak effect, 20 minutes, is **provisional** and carries no citation yet. The MEAC, 0.8 ng/mL, is also **provisional**, set to one tenth of morphine's on the received view that oxymorphone is about ten times as potent; the drug file explains at length why an equianalgesic dose ratio is not a concentration ratio and should be read with caution. Both are flagged in the code as values to replace.

### Covariates

Weight, through the per-kilogram clearance and volume. Under the default [fat-free-mass scaling](help:models/fat-free-mass) the volume scales with fat-free mass and the clearance with that ratio to the 0.75 power; with the switch off both scale linearly with weight. Age, height and sex do not enter.

### Where to be careful

The one-compartment reduction has no distribution phase, so the early plasma curve after a bolus is wrong in exactly the window ke0 is most sensitive to; a tPeak validated against a proper two-compartment model would not transfer to this one unchanged. Oxymorphone's own metabolites, the 3-glucuronide and 6-hydroxyoxymorphone, are not modelled.
