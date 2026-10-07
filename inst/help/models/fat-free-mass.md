Most of the pharmacokinetic models in the library were reported for a typical 70 kg adult and, if they scaled at all, scaled with total body weight. Drug clearance tracks lean tissue rather than fat, so by default stanpumpR scales those models to the patient's **fat-free mass** instead. The checkbox **Adjust weight to fat-free mass** in the Patient Profile turns this on and off; it is on by default. The full account, with worked examples and references, is in the developer document `docs/weight-adjustment.md`; this page is the summary.

## Why not total body weight

A 120 kg patient is not clearing drug 1.7 times as fast as a 70 kg one: the extra weight is mostly fat, which is poorly perfused and contributes little to clearance. Scaling a model linearly with total weight therefore overpredicts clearance in obese patients and recommends too much drug. Across many drugs, fat-free mass is the body-size measure that best predicts clearance.

## What the switch does

With the box ticked, stanpumpR computes the patient's fat-free mass from weight, height, age and sex (the Al-Sallami 2015 equations, which reduce to the Janmahasatian 2005 formula in adults) and scales each affected model:

| Parameter | Multiplier |
|---|---|
| Every volume (V1, V2, V3) | FFM / FFM of the reference man |
| Every clearance (CL1, CL2, CL3) | (FFM / FFM of the reference man) to the 0.75 power |

The reference man is 70 kg, 170 cm, 35 years old, whose fat-free mass by the formula is 54.5 kg. His factors are exactly 1, so he receives every model's published parameters unchanged, and a per-kilogram label dose for him is unchanged. Everyone else is scaled from there. A 70 kg woman has about 18 per cent less fat-free mass than the reference man, so her volumes come out 18 per cent smaller; per-kilogram dosing treats them identically.

**Doses you type per kilogram are always converted with total body weight**, because that is what the clinician actually gives. The switch changes only the pharmacokinetics, not the dose.

## Which models it affects

Each drug's page says whether the switch changes it, under *Parameters at reference patients*.

- **Scaled to fat-free mass** (the default): alfentanil, codeine, dexmedetomidine (adult), etomidate, fentanyl, hydromorphone, ketamine, lidocaine, methadone, midazolam, morphine, naloxone, oliceridine, oxycodone, oxymorphone, pethidine, remimazolam, rocuronium, sufentanil, tramadol, desmetramadol, amiodarone and desethylamiodarone.
- **Not scaled, because they carry their own body-size covariate**: propofol and remifentanil already contain fat-free mass inside their Eleveld and Kim models.
- **Not scaled, deliberately**: oxytocin was fitted in parturients, a population the reference man does not describe; hydrocodone's disposition is apparent (divided by an unmeasured bioavailability) and carries no size term that could be scaled.

## Turning it off

Unticking the box restores, exactly, the scaling each model used before fat-free mass was introduced: no scaling for the fixed-parameter models, linear weight scaling for the per-kilogram ones, and total-weight allometry for the allometric ones. Use it to see how far per-kilogram dosing departs from fat-free-mass dosing in an obese or female patient, or to reproduce a simulation made before the option existed. Simulations saved by URL before the switch existed restore with it off, so their output is unchanged. See [the obesity scenario](scenario:obesity-fat-free-mass).

## Limits

Below three years the Al-Sallami maturation function is extrapolated. The equations have not been validated in pregnancy, and pregnancy weight is mostly fat-free, which is why oxytocin is exempt. The same fat-free-mass ratio is applied to every volume, which understates the deep peripheral volume of a lipophilic drug such as fentanyl in obesity; clearance, the better-supported part, governs the maintenance rate and the eventual decline.
