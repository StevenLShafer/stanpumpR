### The model

The [pramipexole](help:drugs/pramipexole) disposition of the FDA review of NDA 22-421, with the extended-release tablet's input: each tablet is released into the gut at a constant rate over 1.34 hours and absorbed from there at 0.0873 per hour, so a once-daily tablet peaks about 6 hours after the dose. At steady state on 4.5 mg once daily the model peaks at 5.3 ng/mL with an exposure of 108 ng·h/mL a day (healthy men in study 248.530: 4.9 and 92). Doses are entered as labelled, in mg of the dihydrochloride monohydrate, and the same renal and weight covariates apply.

### Why a separate drug

The immediate-release and extended-release tablets have different absorption, and the engine carries one oral release per drug, so the two tablets are two drugs; a patient switched between them is two rows. The label's food effect (Cmax +20%, peak 2 hours later) is not applied.
