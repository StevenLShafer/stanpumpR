### The model

Hydrocodone's disposition is from Melhem and colleagues (*Clin Pharmacokinet* 2013;52:907-917), a population analysis in 220 subjects on an extended-release capsule, reproduced in the FDA clinical pharmacology review. The parameters are **apparent**: clearance and volumes divided by an unmeasured oral bioavailability. The published typical values are V1 714 L, V2 151 L, clearance 64.4 L/h (1.07 L/min) and a slow intercompartmental clearance of 0.015 L/min; for the 70 kg, 170 cm, 35-year-old reference man at a normal creatinine the covariates below give V1 707 L and clearance 66.7 L/h.

### Oral only

The absolute bioavailability of hydrocodone has never been measured, so apparent parameters are all the literature supports. They predict oral concentrations correctly, because the unknown bioavailability cancels, but they would predict intravenous concentrations wrong by a factor of one over that bioavailability. Hydrocodone is therefore offered as **mg PO** only, and bioavailability is carried as 1 because the apparent scale already contains it.

### Absorption and effect site

The absorption rate constant puts the plasma peak at about 78 minutes, the 1.3 hours clinical references give. The time to peak effect, 90 minutes, is **provisional and measured after an oral dose**: it carries no citation yet and was set by Dr Shafer. Because it is an oral tPeak, ke0 is solved against the oral plasma curve rather than an intravenous bolus, which is the right driving curve for an orally observed peak. The MEAC, 12 ng/mL, is **provisional**, set equal to oxycodone's as a working value pending a hydrocodone-specific one: equianalgesic oral doses, 30 mg of hydrocodone and 20 mg of oxycodone (the CDC's morphine milligram equivalent factors are 1 and 1.5), both peak near 37 ng/mL in the model, so they share a concentration. Until October 2026 it was morphine's 8 ng/mL, which left 30 mg by mouth above MEAC for about 20 hours, far longer than the equianalgesic 20 mg of oxycodone. The drug file records both values as gaps to fill.

### Active metabolite

Hydrocodone forms **hydromorphone** by CYP2D6. The formation constant is calibrated so that the hydromorphone-to-hydrocodone exposure ratio reproduces the 0.012 Kapil and colleagues measured (*Clin Ther* 2015;37:2286-2296). An exposure ratio is independent of the input shape, so it transfers from their extended-release product to the immediate-release input here. The **CYP 2D6** field scales formation; the floor for poor metabolisers (0.12 of normal) is from Otton and colleagues' measured partial clearance (*Clin Pharmacol Ther* 1993;54:463-472), well above codeine's, so the two drugs genuinely differ. See [Active metabolites](help:models/metabolites).

### Covariates

Melhem found two covariates, and both are applied. Clearance goes with the 0.223 power of creatinine clearance relative to 91.48 mL/min/1.73 m², the Cockcroft-Gault value indexed to body surface area. The central volume goes with the 0.486 power of body surface area (Gehan and George's formula, the one Melhem used) relative to 1.87 m². The other volume and the intercompartmental clearance have none. Both effects are modest: severe renal impairment (15 against 100 mL/min/1.73 m²) raises exposure by about half, and a patient of 120 kg has a central volume about 14% larger than one of 70 kg. Enter the **Serum creatinine** in the Patient Profile: left blank, it is assumed normal for the patient's age and sex, and renal impairment is then not represented. With the [fat-free-mass switch](help:models/fat-free-mass) on, both covariates see the pharmacokinetic weight (70 kg times the patient's fat-free mass over the reference man's); with it off, total body weight. Melhem's subjects were adults, so a child's values are extrapolation. The formation constant follows the weight-scaled hydromorphone model it feeds and is divided by the central volume's covariate factor, so that the hydromorphone-to-hydrocodone exposure ratio stays at Kapil's 0.012 whatever the patient's size.

### Where to be careful

Part of hydrocodone's effect is its own, not the metabolite's: Otton found poor metabolisers still report opioid effects. The model represents the parent's share only through the provisional tPeak and MEAC above; the metabolite's contribution appears on the hydromorphone row. Norhydrocodone and hydromorphone-3-glucuronide are not modelled.
