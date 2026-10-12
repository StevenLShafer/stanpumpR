### The model

Vecuronium's kinetics are from Vega-Villa and colleagues (*Br J Anaesth* 2014;112:304-310), who compared 20 adults with major burns with 20 age- and sex-matched surgical controls after 0.12 mg/kg, sampling for 12 hours. The library uses their **non-burn** reference values: a **three-compartment** model with V1 10.25 L, V2 9.7 L, V3 43.79 L, clearance 0.51 L/min and distribution clearances 0.66 and 0.095 L/min. The long sampling found a terminal half-life of about 6.6 hours, at concentrations far below any neuromuscular effect; older, shorter studies reported 1 to 2 hours.

### Covariates

**Age.** Vega-Villa's model carries no age term, so the age effect comes from Rupp, Fisher and colleagues (*Anesthesiology* 1987;67:45-49), who studied vecuronium in adults aged 30 to 40 and 70 to 84 under halothane. Only two of their comparisons were significant: in the elderly, clearance was 3.7 against 5.2 mL/kg/min and steady-state volume 179 against 244 mL/kg. Those two ratios are applied here to every clearance and every volume, which reproduces both published figures and leaves the half-lives almost unchanged, matching their third, null finding. The central volume alone (29 against 52 mL/kg) is **not** given its own factor: that difference was not significant and its standard deviations nearly span it. Both ratios are interpolated linearly between ages 40 and 70 and held flat outside, since nobody between those ages was studied.

The effect is substantial. A 70-year-old recovers to 25% twitch about 45 minutes after 0.1 mg/kg, against 32 minutes at age 40 — the direction d'Hollander and colleagues reported from steady-state infusions, though they found a larger difference still.

**Size.** The published parameters are absolute, not per kilogram, and burn status was the only covariate Vega-Villa retained. With *Adjust weight to fat-free mass* ticked, the volumes scale with the patient's fat-free mass relative to the 70 kg, 170 cm reference man and the clearances with that ratio to the 0.75 power; unticked, the parameters are used as published.

### Effect site

The effect site and the potency come from Rupp, Fisher and colleagues (*Anesthesiology* 1987;67:45-49), who fitted the disposition and the twitch response together in young adults under halothane and nitrous oxide: ke0 0.169/min, a steady-state concentration for 50% twitch depression of 92 ng/mL, and a slope of 5.80.

Because ke0 belongs to the kinetics it was fitted with, it is carried here as the **time to peak effect** it produces with Rupp's own model, following Minto and colleagues (*Anesthesiology* 2003;99:324-333). Rupp's kinetics, rebuilt from his table, put the peak at 5.37 minutes after a bolus; the app solves that back to about 0.300/min against the Vega-Villa disposition. Pairing Rupp's 0.169/min directly with a different kinetic model, which is the naive approach Minto warns against, would equilibrate far too slowly.

Rupp's elderly ke0, 0.165/min, is almost identical to the young one, but his elderly kinetics differ, so the same calculation gives a peak at 4.80 minutes. That is the time to peak effect used from age 70, interpolated from 5.37 across the same 40-to-70 span as the kinetics.

Heier and colleagues (*Anesth Analg* 1994;78:973-977) measured the same relationship under isoflurane and agree closely (50% concentration 79 ng/mL, slope 4.4), but reported no kinetics of their own: 60 minutes of sampling after a small infusion was too short to define the elimination phase, so their ke0 cannot be converted to a time to peak effect.

### Typical concentrations and recovery

From Rupp's potency, the shaded band runs from 110 ng/mL (the first twitch recovered to 25% of baseline) through 135 ng/mL (90% block) to 155 ng/mL (95% block), and the default recovery threshold is 110 ng/mL, so *Time until threshold* is the time to 25% recovery. Heier's values would give 101, 130 and 154, so the band barely depends on which study is used. The plot shows concentration, not twitch height or train-of-four counts.

The band does not move with age. Rupp found no significant age difference in the 50% concentration (106 against 92 ng/mL) or the slope (4.84 against 5.80), so the elderly are modelled as no more sensitive, only slower to clear the drug.

As a check, 0.1 mg/kg in a 40-year-old, 70 kg adult gives complete block and recovers to 25% at about 32 minutes, against a labelled clinical duration of 25 to 40 minutes.

### Where to be careful

**Inhaled agents potentiate vecuronium, and that is not modelled.** Wright, Fisher and colleagues (*Anesthesiology* 1994;81:59-68) measured 15.5 and 34.3 minutes to 25% recovery after 26 and 66 mcg/kg under nitrous oxide and isoflurane; this model predicts 6 and 24 minutes for those doses, because their patients were far more sensitive than the reference patient here. Their estimated dose for 95% block was 21.8 mcg/kg, roughly half the unpotentiated value. Read the predictions as applying to a patient without volatile anesthetic.

Vecuronium's 3-desacetyl metabolite is active, accumulates in renal failure, and contributes to the drug's cumulative effect even after ordinary clinical doses (Wright 1994); it is not modelled. Duration is also prolonged by hepatic and renal dysfunction and by hypothermia, and burns cause resistance. Reversal by sugammadex or neostigmine does not change the vecuronium row.
