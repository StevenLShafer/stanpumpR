### A research model, behind an opt-in

GHB is one of stanpumpR's **illicit-drug research models**. These are hidden until you turn on **Show illicit drugs** above the dose table (off by default), and their names appear in red in the dose table. The curve is a *modelled plasma concentration* offered for research and teaching. It is not dosing advice, a safety threshold, or a prediction of anyone's experience.

GHB has a famously steep concentration-effect relationship and a narrow margin between a recreational dose and unconsciousness. **Nothing on this plot marks that margin**, because no source reviewed for this model fits one in a form the library could carry. Do not read the absence of a band as reassurance.

### The same molecule is a medicine

GHB is sodium oxybate, approved as Xyrem, Xywav and Lumryz for cataplexy and excessive daytime sleepiness in narcolepsy. The model here is fitted to that clinical programme. It is filed under illicit drugs because that is the use this library is asked about, but the parameters are the medicine's, from supervised dosing at 1 to 4.5 grams.

### The dose is the salt, the curve is the acid

This is the mistake this drug is known for, so the unit names the basis.

You enter **grams or milligrams of sodium oxybate** — the salt that is actually swallowed, whether as a prescription oral solution or as the recreational sodium salt. The plot shows **plasma GHB, the free acid**, in mcg/mL, which is the same as mg/L and is what every independent human study and the whole forensic literature report.

The two differ by their molecular weights: GHB is 104.10 g/mol and sodium oxybate 126.09, so a gram of the salt is 0.826 g of GHB. **Confusing them is a 21% error in either direction.** Chen's source reports both doses and concentrations as sodium oxybate, so stanpumpR rescales the mass-carrying parameters onto the acid basis. That rescaling is exact arithmetic, not an approximation, and the drug's test pins it.

Two cautions this cannot fix. Street GHB is usually a solution of unknown concentration, so the quantity swallowed is rarely known to the accuracy this plot implies. And **GBL and 1,4-butanediol are not GHB** — they are prodrugs with their own conversion kinetics, they are not modelled here, and entering a GBL dose as if it were sodium oxybate will be wrong.

### The model

Chen and colleagues fitted a population model in NONMEM to 3,775 plasma observations from 174 people pooled across five studies: 132 healthy adults, 13 adults with narcolepsy, and 29 children and adolescents (Chen C, Rosen CL, Ruoff C, et al. *Clin Transl Sci* 2020;13(5):932-940).

The structure is **two compartments with Michaelis-Menten elimination** from the central one. Saturable elimination is the whole point of this drug: a second gram does not add a second gram's worth of curve, it adds more. On this model, doubling a 2.25 g dose multiplies the area under the curve by about 3.2 rather than 2.

stanpumpR carries a maximum metabolic rate of 1,508 mg/h, a half-saturating concentration of 22.0 mg/L, a central volume of 29.5 L and an absorption rate constant of 4.12 per hour, all at a 70 kg reference adult and all on the source's sodium oxybate basis before rescaling.

### Oral only

The model did not estimate bioavailability; it fixed it at 1, while the Xyrem label puts absolute oral bioavailability at 88%. So the volume and the maximum rate are *apparent* — divided by an unmeasured bioavailability. They predict oral concentrations correctly, because the unknown factor cancels, and intravenous ones wrong by 1/F.

GHB is therefore offered as an **oral** unit only. That GHB was once used as an intravenous anaesthetic does not change it: this parameter set cannot be used for that.

### How well does it hold up?

The model reproduces its own source's simulated adult peak after 4.5 g twice nightly to within a few percent. More usefully, it was checked against two studies it was *not* fitted to:

- **Liechti 2016**, 32 healthy men given 20 and 35 mg/kg orally, reported peak concentrations of 22.7 and 47.2 mcg/mL. This model predicts 24.1 and 46.7 — about 6% high and 1% low.
- **Palatini 1993**, 8 healthy men given 12.5, 25 and 50 mg/kg of the free acid, reported peaks of 23, 46 and 80 mcg/mL. This model gives 18, 41 and 93 — low at the small doses and **16% high at the largest**, which is the limitation below showing itself.

### What is not modelled

- **Saturable absorption.** Palatini found the peak time stretching from 25 to 45 minutes across a fourfold dose, which a fixed first-order absorption rate cannot produce, and read it as capacity-limited absorption. Chen fitted a single absorption rate and no saturable input, so none is coded. **Above about 4.5 g this model peaks earlier and higher than the real drug**, and nothing here is calibrated above that dose.
- **Food.** The source fitted a 68% reduction in the absorption rate when fed, which the app has no field for, so the **fasted** case is carried. Food moves timing much more than total exposure. A dose taken after eating peaks lower and later than this curve.
- **Time of day.** The source fitted a 17.7% lower maximum metabolic rate at one of its two dosing times. The app has no clock, so the reference level is carried; the source does not print which level that is. The other would raise exposure, by about 13% on the second peak of a 4.5 g dose.
- **Any effect.** There is no effect site, no MEAC and no band. Liechti found subjective effects tracking plasma concentration with no clockwise hysteresis, so an effect-site delay would be the wrong shape even if a fitted one existed.
- **Alcohol**, which is taken with GHB often and is the common factor in serious outcomes. No interaction of any kind is modelled.

### Covariates

The source fitted its own allometry, so stanpumpR carries that rather than the package's: the central volume with an exponent of 0.650 on weight and the maximum rate with 0.633. The source does **not** print the weight it normalised to; 70 kg is assumed, which is the library's own reference and the usual convention. The dataset's median weight was 72.1 kg, and using that instead would move predictions by under 3% — well inside the uncertainty of the maximum rate alone, which spans about ±11%.

The age effect is **categorical**, on the central volume, and is carried as published: a 7 to 17 year old gets a 22% larger central volume and a 7 to 11 year old a 12% smaller one than an adult. Being categorical, it steps at the twelfth and eighteenth birthdays rather than varying smoothly, so the curve will jump as you change the age across those boundaries. The youngest child in the data was 8, so nothing below 8 was fitted; below 7, the category's published lower edge, the adult value is used.

### Where to be careful

Read this as single-session oral exposure for a known quantity of pharmaceutical-grade sodium oxybate in a fasted adult. Recreational use matches almost none of those conditions, and the plot does not show the thing that makes GHB dangerous.
