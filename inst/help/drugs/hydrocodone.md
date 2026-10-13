### The model

Hydrocodone's disposition is from Melhem and colleagues (*Clin Pharmacokinet* 2013;52:907-917), a population analysis in 220 subjects on an extended-release capsule, reproduced in the FDA clinical pharmacology review. The parameters are **apparent**: clearance and volumes divided by an unmeasured oral bioavailability. At 70 kg the model gives V1 714 L, V2 151 L, clearance 1.07 L/min and a slow intercompartmental clearance of 0.015 L/min.

### Oral only

The absolute bioavailability of hydrocodone has never been measured, so apparent parameters are all the literature supports. They predict oral concentrations correctly, because the unknown bioavailability cancels, but they would predict intravenous concentrations wrong by a factor of one over that bioavailability. Hydrocodone is therefore offered as **mg PO** only, and bioavailability is carried as 1 because the apparent scale already contains it.

### Absorption and effect site

The absorption rate constant puts the plasma peak at about 78 minutes, the 1.3 hours clinical references give. The time to peak effect, 90 minutes, is **provisional and measured after an oral dose**: it carries no citation yet and was set by Dr Shafer. Because it is an oral tPeak, ke0 is solved against the oral plasma curve rather than an intravenous bolus, which is the right driving curve for an orally observed peak. The MEAC, 8 ng/mL, is **provisional**, set equal to morphine's as a working value pending a hydrocodone-specific one; the drug file records both as gaps to fill.

### Active metabolite

Hydrocodone forms **hydromorphone** by CYP2D6. The formation constant is calibrated so that the hydromorphone-to-hydrocodone exposure ratio reproduces the 0.012 Kapil and colleagues measured (*Clin Ther* 2015;37:2286-2296). An exposure ratio is independent of the input shape, so it transfers from their extended-release product to the immediate-release input here. The **CYP 2D6** field scales formation; the floor for poor metabolisers (0.12 of normal) is from Otton and colleagues' measured partial clearance (*Clin Pharmacol Ther* 1993;54:463-472), well above codeine's, so the two drugs genuinely differ. See [Active metabolites](help:models/metabolites).

### Covariates

The apparent disposition deliberately carries no weight term: the source fitted creatinine clearance and body surface area but its reference values were not recoverable, so no normaliser exists. Weight enters only through the formation constant, which is scaled to track the weight-scaled hydromorphone model it feeds. The [fat-free-mass switch](help:models/fat-free-mass) therefore does not change hydrocodone's own curve.

### Where to be careful

Part of hydrocodone's effect is its own, not the metabolite's: Otton found poor metabolisers still report opioid effects. The model understates this, because its effect site waits on the provisional tPeak and MEAC above, and the metabolite's contribution appears on the hydromorphone row. Norhydrocodone and hydromorphone-3-glucuronide are not modelled.
