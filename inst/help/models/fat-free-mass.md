Most of the pharmacokinetic models in the library were reported for a typical 70 kg adult and, if they scaled at all, scaled with total body weight. Drug clearance tracks lean tissue rather than fat, so by default stanpumpR scales those models to the patient's **fat-free mass** instead. The checkbox **Adjust weight to fat-free mass** in the Patient Profile turns this on and off; it is on by default. The full account, with worked examples and references, is in the developer document `docs/weight-adjustment.md`; this page is the summary.

## Why not total body weight

A 120 kg patient is not clearing drug 1.7 times as fast as a 70 kg one: the extra weight is mostly fat, which is poorly perfused and contributes little to clearance. Scaling a model linearly with total weight therefore overpredicts clearance in obese patients and recommends too much drug. Across many drugs, fat-free mass is the body-size measure that best predicts clearance.

## What the switch does

With the box ticked, stanpumpR computes the patient's fat-free mass from weight, height, age and sex (the Al-Sallami 2015 equations, which extend the Janmahasatian 2005 formula to children; they give the same fat-free mass in adult men and 1 to 3 per cent more in adult women) and scales each affected model:

| Parameter | Multiplier |
|---|---|
| Every volume (V1, V2, V3) | FFM / FFM of the reference man |
| Every clearance (CL1, CL2, CL3) | (FFM / FFM of the reference man) to the 0.75 power |

The reference man is 70 kg, 170 cm, 35 years old, whose fat-free mass by the formula is 54.5 kg. His factors are exactly 1, so he receives every model's published parameters unchanged, and a per-kilogram label dose for him is unchanged. Everyone else is scaled from there. A 70 kg woman has about 18 per cent less fat-free mass than the reference man, so her volumes come out 18 per cent smaller; per-kilogram dosing treats them identically.

**Doses you type per kilogram are always converted with total body weight**, because that is what the clinician actually gives. The switch changes only the pharmacokinetics, not the dose.

## Which models it affects

Each drug's page says whether the switch changes it, under *Parameters at reference patients*.

- **Scaled to fat-free mass** (the default): every other drug model in the library, including both dexmedetomidine models (the adult and the infant) and the models of metabolites that are never dosed directly. A model with a weight covariate of its own (citalopram's S-citalopram clearance, for example) evaluates it at the pharmacokinetic weight, 70 kg times the patient's fat-free mass over the reference man's, with the switch on, and at total body weight with it off. So do the renal models' Cockcroft-Gault creatinine clearance and gentamicin's de-indexed eGFR, so the switch changes their renal function too (see [Covariates and body size](help:models/covariates)). Hydrocodone carries its source's own covariates, body surface area on the central volume and creatinine clearance on clearance; the switch moves them to the pharmacokinetic weight and scales its other two parameters to fat-free mass.
- **Not scaled, because they carry their own body-size covariate**: propofol and remifentanil already contain fat-free mass inside their Eleveld and Kim models; tirzepatide and warfarin carry their own size covariates, and tiagabine's clearance and volume are powers of height. (The Tham insulins do scale: they hold the weight they use, the fat-free-mass-derived weight with the switch on and the total weight with it off, within each source's studied range, so the two positions agree only where the two weights clamp to the same value: both within the range and equal, or both beyond the same end of it.)
- **Not scaled, deliberately**: oxytocin was fitted in parturients, a population the reference man does not describe.

## Turning it off

Unticking the box restores, exactly, each model's published scaling (for the older models, the scaling they used before fat-free mass was introduced): no scaling for the fixed-parameter models, linear weight scaling for the per-kilogram ones, and total-weight allometry for the allometric ones. Use it to see how far per-kilogram dosing departs from fat-free-mass dosing in an obese or female patient, or to reproduce a simulation made before the option existed. Simulations saved by URL before the switch existed restore with it off, so their output is unchanged, except for hydrocodone: its source's covariates were added in October 2026 and apply in both positions, so an older hydrocodone simulation does not reproduce. See [the obesity scenario](scenario:obesity-fat-free-mass).

## Limits

Below three years the Al-Sallami maturation function is extrapolated. The equations have not been validated in pregnancy, and pregnancy weight is mostly fat-free, which is why oxytocin is exempt. The same fat-free-mass ratio is applied to every volume, which understates the deep peripheral volume of a lipophilic drug such as fentanyl in obesity; clearance, the better-supported part, governs the maintenance rate and the eventual decline.
