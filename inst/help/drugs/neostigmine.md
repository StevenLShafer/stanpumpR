### The model

Neostigmine has **no verified adult population model**. The literature review behind this drug found individual two-compartment fits in six female surgical patients (Calvey and colleagues, *Br J Clin Pharmacol* 1979;7:149-155: a 15-second injection during tubocurarine reversal under halothane, atropine given, first sample at two minutes) and a volunteer study (Heier and colleagues, *Anesthesiology* 2002;97:90-95) whose abstract reports a clearance of 696 mL/min but whose parameter table was not recovered.

The row uses **Calvey's patient 1** as a complete, internally consistent row, scaled per kilogram to the reference adult: central volume 10.9 mL/kg (0.76 L at 70 kg), rate constants k10 0.527, k12 0.398 and k21 0.039 per minute, giving a peripheral volume of 7.8 L, clearance 0.40 L/min and half-times of 0.74 and 32 minutes. Averaging the six patients was rejected because one of them has a fitted central volume (0.11 mL/kg) that is not a physiological space.

### What this means for the curve

The first two minutes after a bolus were never observed and are not credible here: the tiny central volume puts the instantaneous concentration of a 3 mg dose near 4000 ng/mL before a sub-minute distribution phase removes most of it. From a few minutes on the curve reproduces the fitted decline, and the clearance (400 mL/min) sits below Heier's contemporary 696 mL/min. Treat the model as the best available shape, not a validated prediction, and replace it when an adult population model with early sampling appears.

### Effect site

The time to peak effect, 4.6 minutes, is Heier's measured time to maximum antagonism of a vecuronium block, counted from the start of a two-minute infusion; used as a bolus tPeak it slightly understates ke0. No concentration-effect model is attached: the effect-site curve is a delayed concentration, not a train-of-four prediction, and nothing here accounts for the depth of block being reversed. The recovery threshold is 30 ng/mL, the bottom of the shaded band (30 to 300 ng/mL).

### Covariates

Weight only, per kilogram with fixed rate constants in the source; under the default [fat-free-mass scaling](help:models/fat-free-mass) the volumes follow fat-free mass and the clearances its 0.75 power.

### Where to be careful

Six patients from 1979, with an assay calibrated against the bromide while the methylsulfate was given, so the dose is nominal labelled milligrams with no molar correction. Renal failure roughly doubles neostigmine's half-life and is not represented. Give [glycopyrrolate](help:drugs/glycopyrrolate) alongside to see the two curves together; their interaction is not modelled.
