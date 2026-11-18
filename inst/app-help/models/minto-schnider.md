Thomas Schnider and Charles Minto developed the first population pharmacokinetic and pharmacodynamic models of propofol (Schnider 1998, 1999) and remifentanil (Minto 1997). Those two models are the ones most target-controlled infusion devices run today. The Stanford data behind them were uploaded to the Open TCI initiative, with further data from the same laboratory and studies from other investigators, and Douglas Eleveld built from that pool the general-purpose models that extend to children and to the extremes of weight (see *The Open TCI initiative* in [The history of TCI](help:history)). The Eleveld models are thus built on a superset of the data behind Schnider's and Minto's. They are the library's own [propofol](help:drugs/propofol) and [remifentanil](help:drugs/remifentanil) models, except that for remifentanil in an adult with a BMI of 30 or more the library uses Kim's model instead.

For teaching it helps to see both at once. The **Minto / Schnider** box under [Graph Options](help:graph-options) runs the Schnider and Minto models beside the library's models whenever propofol or remifentanil is in the dose table, and draws them as a second line in the same panels. The box is enabled only while one of those drugs is in the table.

## What the switch does

**In ordinary simulation** the second model's plasma and effect-site concentrations are drawn as a second line in the drug's own panel, with the same line styles as the first (dashed for plasma, solid for the effect site by default) in a darker or lighter shade of the drug's colour, and named in the legend as *propofol (Schnider)* or *remifentanil (Minto)*. The hover readout gives both values, the library's model first and the other by its name.

**Under a [target-controlled infusion](help:tci)** the second model drives a pump of its own, from the same target rows. Its infusion rates are drawn as a second line in the drug's TCI rate panel, its loading dose is written below the first pump's and named for the model, and the concentrations it predicts are the second line in the concentration panel. Each pump hits its own target; what the two models disagree about is how much drug that takes. When a slide is emailed, both pumps' rate rows are merged into the exported dose table, under their own names.

**The time until threshold** is never computed for the second model. The thin black line stays on the library's own kinetics (Eleveld's, or Kim's for remifentanil in an adult with a BMI of 30 or more), which is also the model the shaded band, the MEAC panel, the interaction panel and the opioid-MAC interaction read: a second remifentanil line is a second model of the same drug, not a second opioid.

[The simulation scenario](scenario:eleveld-vs-minto-schnider) and [the TCI scenario](scenario:tci-eleveld-vs-minto-schnider) show the switch on, with the differences to look for, and [the obesity scenario](scenario:james-lean-body-mass) shows where the two models fail.

## The Schnider propofol model

Schnider and colleagues (1998) studied 24 volunteers aged 25 to 81 years, 44 to 123 kg and 155 to 196 cm, given propofol as a bolus and as infusions. The model is written exactly as the original STANPUMP ran it. Volumes are in litres, clearances in litres per minute, weight in kg, height in cm, age in years, and LBM is the James lean body mass (see [Covariates and body size](help:models/covariates)):

```
V1  = 4.27
V2  = 18.9 - 0.391 (age - 53)
V3  = 238
CL1 = 1.89 + 0.0456 (weight - 77) - 0.0681 (LBM - 59) + 0.0264 (height - 177)
CL2 = 1.29 - 0.024 (age - 53)
CL3 = 0.836
```

The time to peak effect is 1.6 minutes, from Schnider and colleagues' 1999 study of propofol pharmacodynamics, the same value the library's own propofol uses, and ke0 is solved from it for each patient as it is for every other drug (see [The effect site and ke0](help:models/effect-site)). For the reference adult that gives a ke0 of 0.41 per minute, close to the 0.456 the 1999 paper reported for its typical patient. The STANPUMP propofol file set the same tPeak.

## The Minto remifentanil model

Minto and colleagues (1997) studied 65 volunteers aged 20 to 85 years. The model, again as STANPUMP ran it:

```
V1  = 5.1  - 0.0201 (age - 40) + 0.072 (LBM - 55)
V2  = 9.82 - 0.0811 (age - 40) + 0.108 (LBM - 55)
V3  = 5.42
CL1 = 2.6  - 0.0162 (age - 40) + 0.0191 (LBM - 55)
CL2 = 2.05 - 0.0301 (age - 40)
CL3 = 0.076 - 0.00113 (age - 40)
ke0 = 0.595 - 0.007 (age - 40)
```

Here ke0 is the paper's own, a function of age, which is what the pumps implement and what STANPUMP set. The library's convention is to store a time to peak effect and solve ke0 from it; for this model the engine does the reverse, taking the published ke0 and solving the time to peak effect from it for the patient (1.4 minutes at 40 years, 2.3 minutes at 80), which the TCI controller needs to size the loading dose.

## Where the models are not defined

Both models use linear covariate terms and the James lean body mass, and outside the populations they were fitted to those terms can give a volume or clearance that is zero or negative.

The James lean body mass is a **parabola** in weight: at a given height it rises with weight, peaks at a BMI of 43.0 in men and 36.1 in women (about 125 kg in a man of 170 cm, 92 kg in a woman of 160 cm), and then falls, reaching zero at twice that BMI ([Covariates and body size](help:models/covariates) gives the algebra). Past the peak a heavier patient is credited with less lean tissue, which is not physiology but an artifact of the equation, unforeseen when these models were built. Taken literally it makes the Schnider clearance, which falls with lean body mass, climb steeply in the obese; Minto's V1, V2 and CL1 rise with lean body mass, so they shrink, and far enough out V1 goes negative. This is why the pumps restrict these models to adults with a BMI below 35 in women and 42 in men, just short of the peak.

stanpumpR **holds the James lean body mass at its peak**: below the peak the companions use the equation as published, and past it they use the most it gives for the patient's height, so lean body mass never falls as weight rises. That is what a pump computes for the heaviest patient it accepts, continued without the downward turn, and it keeps both models defined at every weight the Patient Profile allows. The weight and height terms are still the models' own, so the Schnider clearance still rises with total weight in the obese and a Schnider pump still gives far more propofol than an Eleveld one; the comparison with the Eleveld models, which scale to the Al-Sallami fat-free mass and rise with weight at every weight, is the lesson. The scenario [Obesity: the James lean body mass in the Schnider and Minto models](scenario:james-lean-body-mass) draws the parabola, shows the cap, and puts the two pumps side by side in a 150 kg woman.

In a small child the Schnider clearance intercepts make CL1 negative, and no cap helps there.

Where a volume, clearance or ke0 comes out zero or negative for the patient entered, that model is not drawn and the app says so in a notice; the library's model is shown alone. Where the numbers are positive but the patient is outside the study population, a child on Minto's model for instance, the line is drawn and is an extrapolation.

## Parameters at the reference patients

The volumes, clearances, ke0 and time to peak effect the engine computes from each model at the help's reference patients, with the library's own values for the same patient beside them. Built from the code when this page is shown.

<!-- generated: minto-schnider-table -->

## How it is built

In the code an alternative model is a *companion entry* of the drug list: `recalculatePK()` builds it from the model function in `R/alternativeModels.R` through the same `getDrugPK()` every drug goes through, names it `propofol (Schnider)` or `remifentanil (Minto)`, and marks it with the drug it belongs to. `processdoseTable()` simulates it on the parent's dose rows, caching it like any other drug, and `simulationPlot()` draws it in the parent's panel. See [What is in the repository](help:repository).

## References

Schnider TW, Minto CF, Gambus PL, Andresen C, Goodale DB, Shafer SL, Youngs EJ. The influence of method of administration and covariates on the pharmacokinetics of propofol in adult volunteers. *Anesthesiology* 1998;88:1170-1182.

Schnider TW, Minto CF, Shafer SL, Gambus PL, Andresen C, Goodale DB, Youngs EJ. The influence of age on propofol pharmacodynamics. *Anesthesiology* 1999;90:1502-1516.

Minto CF, Schnider TW, Egan TD, Youngs E, Lemmens HJM, Gambus PL, Billard V, Hoke JF, Moore KHP, Hermann DJ, Muir KT, Mandema JW, Shafer SL. Influence of age and gender on the pharmacokinetics and pharmacodynamics of remifentanil. I. Model development. *Anesthesiology* 1997;86:10-23.

Minto CF, Schnider TW, Shafer SL. Pharmacokinetics and pharmacodynamics of remifentanil. II. Model application. *Anesthesiology* 1997;86:24-33.

Eleveld DJ, Colin P, Absalom AR, Struys MMRF. Pharmacokinetic-pharmacodynamic model for propofol for broad application in anaesthesia and sedation. *Br J Anaesth* 2018;120:942-959.

Eleveld DJ, Proost JH, Vereecke H, Absalom AR, Olofsen E, Vuyk J, Struys MMRF. An allometric model of remifentanil pharmacokinetics and pharmacodynamics. *Anesthesiology* 2017;126:1005-1018.
