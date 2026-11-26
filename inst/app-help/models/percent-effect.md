A drug's effect can be drawn as a **percentage of its maximal effect**, from 0 to 100% on a linear axis. A blood pressure, a heart rate or a pain score each start from a baseline that differs from patient to patient, and a published model's maximal effect is measured in the source's own population. A percentage of the maximum leaves both out. What is left is the shape of the concentration-effect relation and its time course, which is what a model of this kind can be expected to carry from one population to another.

## How stanpumpR shows it

Tick **Graph Options → % effect**. The option is offered only while a drug with a published concentration-effect model is drawn. Under that drug's panel (and under its peripheral compartments and its tolerance panel, if they are shown) a panel named for the drug and the endpoint, for example *irbesartan SBP fall*, is drawn in *% of maximal effect*, always from 0 to 100%. Hovering over it gives the effect and the concentration driving it.

The effect is the sigmoid Emax model as a share of its own maximum:

E(t) = 100 × C(t)<sup>γ</sup> / (EC50<sup>γ</sup> + C(t)<sup>γ</sup>)

where EC50 is the concentration at half the maximal effect and γ the Hill coefficient, the steepness of the curve. At the EC50 the effect is 50%, whatever γ is.

The concentration C that drives it is one of three:

- **An effect site of the endpoint's own.** Most published effect models were fitted with their own effect site, through a rate constant k<sub>e0</sub>: dC<sub>e</sub>/dt = k<sub>e0</sub> (C<sub>p</sub> − C<sub>e</sub>). stanpumpR fills that effect site from the drug's plasma curve with the source's k<sub>e0</sub>. This is how a model is carried onto a drug that the library draws as plasma only, or whose own effect site belongs to another endpoint. The effect site is computed exactly between the simulated points, reading the plasma concentration as a straight line between them, which is how the plot draws it.
- **The drug's own effect site**, the one its panel draws, where the endpoint is the one that effect site was built for.
- **Plasma**, directly, where the source found no delay.

Nothing is simulated again. The panel follows the drug's own curve, whatever the route, dose or schedule.

## Reading it back in the source's units

The table gives each model's maximal effect in its source's units. Multiply the percentage by it to read the curve as the source would: 50% of a systolic Emax of 14.8 mmHg is a fall of 7.4 mmHg in the source's population. That conversion is only as good as the population match. The percentage itself does not depend on it.

## The models

Each model is a row of the table below, taken from a published study. A drug may have more than one published model for the same endpoint. One is **drawn**. The others are listed as **alternatives**, so that a disagreement between sources stays in view. A model marked **provisional** was chosen without a source that settles the choice; its drug's help page says why. A drug that no row covers has no panel: stanpumpR does not guess a concentration-effect relation.

<!-- generated: pd-effect-table -->

### Irbesartan

Both published irbesartan models describe the fall in systolic pressure in normotensive men after a single 300 mg dose, each fitted on its own pharmacokinetics, not on the Karatza model this library uses. Their EC50s differ sevenfold (0.29 and 2.1 mcg/mL). After a single 150 mg dose, Huang 2006 peaks at 74% at 3.4 hours and leaves 32% at 24 hours. Hedaya 2015 peaks at 20% at 3.8 hours and leaves almost nothing at 24 hours. At steady state on 150 mg once daily the trough-to-peak ratio is 0.45 with Huang and 0.02 with Hedaya. The Avapro label puts the peak effect at 3 to 6 hours, with a trough-to-peak ratio of 60 to 70%. Both models place the peak correctly, but only Huang leaves an effect at the trough, as a once-daily drug must, so Huang is drawn, **provisionally**. Two cautions. Huang's apparent clearance was about twice the label's, so its EC50 was fitted on lower concentrations than Karatza's, and the curve here probably overstates the effect. And Huang's own abstract reports that blood pressure was almost unchanged after 150 mg, which the curve does not reproduce.

## What it does not do

The panel is a percentage, never a pressure or a rate. It does not add a baseline, and it changes nothing else: the drug's band, MEAC and time until threshold are unchanged. Tolerance, where a drug has a tolerance model, is drawn on its own panel and does not reduce this one.

## Why there is no EEG endpoint

A good deal has been published on propofol's effect-site concentration against BIS, the A-line ARX index and the spectral edge, and none of it is drawn here. The reason is not that the numbers are poor but that **the EEG is not solely a measure of drug effect**.

On emergence the EEG is nearly unchanged for several minutes after the drug is stopped. Then the patient opens their eyes and it changes instantly, showing brain arousal with the return of consciousness. What the monitor reports is a combination of the drug's effect and the state of the brain, so a curve of concentration against index credits the drug with a change the brain made.

The drugs also do not move the EEG in one direction, which matters because an anaesthetic combines them. Ketamine and midazolam activate the EEG. Opioids do little to it except in very large doses, where they depress it. The hypnotics -- propofol, the inhaled agents, etomidate -- activate it and then depress it. A panel showing % effect against one drug's concentration would suggest a single monotone relation that does not exist at the bedside.

Steven L. Shafer, 2026-11-26.
