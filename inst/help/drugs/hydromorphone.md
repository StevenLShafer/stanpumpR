### The model

Hydromorphone's intravenous kinetics are from Drover and colleagues (*Anesthesiology* 2002;97:827-836), a study in healthy volunteers that characterised the intravenous disposition and the input of immediate- and extended-release oral formulations. The model is a central volume of 0.16 L/kg with fixed rate constants.

### Covariates

Weight. Under the default [fat-free-mass scaling](help:models/fat-free-mass) the volumes scale with the patient's fat-free mass and the clearances with that ratio to the 0.75 power; with the switch off, both scale linearly with weight.

### Effect site

The time to peak effect is 19.6 minutes: slower than fentanyl, much faster than morphine.

### Extravascular routes

Hydromorphone is offered by mouth, intramuscularly and intranasally. The oral route uses an absorption rate constant of 0.01/min (a half-time of about 70 minutes) and a bioavailability of 0.6, both still provisional. The intramuscular and intranasal routes were revised so that each peaks at its measured time, about 30 and 20 minutes, with the delay carried by the absorption rate constant rather than by a lag: anchored on Coda's intranasal data (bioavailability about 0.55), with the intramuscular bioavailability set to 1 and a 30-minute peak as a judgement, since no human intramuscular pharmacokinetic study was found. The old placeholder lags of 90 and 180 minutes, which put the peaks hours too late and broke the time-until-threshold readout, are gone.

### Also the metabolite of hydrocodone

Hydromorphone is the modelled active metabolite of [hydrocodone](help:drugs/hydrocodone): giving hydrocodone adds a hydromorphone row. See [Active metabolites](help:models/metabolites).

### MEAC and typical concentrations

MEAC is 1.5 ng/mL, the shaded band 1.2 to 3 ng/mL. Hydromorphone is five to seven times as potent as morphine by mass.

### Where to be careful

The model describes young healthy volunteers. Hydromorphone-3-glucuronide, a neuroexcitatory metabolite that accumulates in renal failure, is not modelled. The intramuscular route has no direct human pharmacokinetic citation; its parameters are a reasoned estimate.
