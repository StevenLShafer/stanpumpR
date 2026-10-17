### A research model, behind an opt-in

LSD is one of stanpumpR's **illicit-drug research models**. These are hidden until you turn on **Show illicit drugs** above the dose table (off by default), and their names appear in red in the dose table. The curve is a *modelled plasma concentration* offered for research and teaching. It is not dosing advice, a safety threshold, or a prediction of anyone's experience. It describes pharmaceutical LSD base given orally in a controlled study, not material of unknown identity or content.

### The model

Dolder and colleagues fitted a one-compartment model with first-order input, first-order elimination and **no lag time** to oral LSD in healthy adults (Dolder PC, Schmid Y, Steuer AE, et al. *Clin Pharmacokinet* 2017;56(10):1219-1230). Their own two-compartment attempt did not improve the fit.

stanpumpR implements their **100 microgram arm** (n = 24): an absorption rate constant of 1.4 per hour, an apparent clearance of 12.3 L/h and an apparent volume of 46 L. Those give a half-life of 2.6 hours and a peak at 1.4 hours, reproducing the values the paper published.

### Oral only, because the parameters are apparent

Clearance and volume were fitted as *clearance over bioavailability* and *volume over bioavailability*, because the absolute bioavailability was not measured in that study. Apparent parameters predict oral concentrations correctly, since the unknown factor cancels, and intravenous ones wrong by 1/F. LSD is therefore offered as an **oral** unit only, and its bioavailability is carried as 1 because the apparent scale already contains it.

### One dose arm, applied to every dose

The 200 microgram arm was fitted separately (absorption rate 1.2 per hour, apparent clearance 9.9 L/h, apparent volume 37 L, the same elimination rate constant). stanpumpR carries one parameter set per drug, so a 200 microgram dose here is simulated with the 100 microgram arm's parameters and its exposure is predicted **dose-proportionally**. What that misses is worth knowing: the two arms share an elimination rate constant, so the half-life is the same, but the 200 microgram arm's apparent clearance is lower, so its published exposure per microgram is about a quarter higher than this model predicts.

### A caution about the half-life this model carries

The 2.6 hour half-life of this 2017 analysis is shorter than the same group's later measurements. Holze and colleagues (*Br J Clin Pharmacol* 2024;90(1):200-208), with 85 and 170 micrograms in 28 subjects, reported 3.7 and 4.0 hours, and a 2025 oral/intravenous crossover reported a terminal half-life of 3.8 hours, an intravenous clearance of 4.7 L/h and an absolute oral bioavailability near 0.80. Different analytical methods and modelling approaches account for much of the difference.

So **this model clears LSD faster than the later data do**. Read it as Dolder's 2017 model, not as a consensus. The 2025 crossover is not implemented: its two-compartment intravenous micro-parameters are in a supplement rather than its main text, and the terminal volume it publishes is a non-compartmental quantity, not a central volume.

### Dose basis

Micrograms of **LSD base**, as the nominal capsule content of the source study, with no analytical content correction. LSD tartrate is a different mass basis and is not this model's input.

### No effect site

There is no effect site, no MEAC and no band. Dolder did fit sigmoid models with an effect-site delay, but separately for each rating scale and each dose arm, each with its own EC50, Hill coefficient and equilibration constant, and with heart rate measured in beats per minute while the visual analogue scales are bounded percentages. Those are **end-point-specific** fits of subjective and cardiovascular responses, not one equilibration constant for "the effect of LSD", so none is coded here.

### Covariates

Dolder published no size covariate, so the apparent parameters are a reference adult's: unscaled with *Adjust weight to fat-free mass* off, and scaled with the Al-Sallami fat-free mass ratio (volume) and its 0.75 power (clearance) with it on. That scaling is a stanpumpR convention, not a published fit.

### Where to be careful

Plasma concentration does not predict a psychedelic experience, which depends on set, setting, expectation and tolerance far more than on a number. Read this curve as exposure only.
