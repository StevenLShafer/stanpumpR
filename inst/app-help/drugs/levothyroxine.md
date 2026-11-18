### What is plotted

The curve is the thyroxine (T4) that the doses add to the serum: total T4 above the patient's own baseline, in ng/mL (10 ng/mL is 1 mcg/dL, the unit T4 is usually reported in). In a patient with no thyroid function (after thyroidectomy or ablation, or with advanced primary hypothyroidism) the baseline is close to zero, and the curve is the serum T4.

In a patient whose thyroid still works, the curve is not the change in measured T4. Levothyroxine lowers TSH, and with it the gland's own secretion. That feedback is not modelled.

### The model

There is no modern population model of levothyroxine, so this model comes from the classical tracer studies of thyroxine kinetics.

**Disposition.** The disposition comes from Inada and colleagues (*J Clin Invest* 1975;55:1337-1348, doi 10.1172/JCI108053). They gave <sup>125</sup>I-labelled T4 intravenously and sampled serum for two weeks. Their Table II gives, for five normal men aged 21 to 28, the terminal exponential of each subject's curve. The volume is 8.4 L and the clearance 1.06 L/day, both per 60 kg of body weight. That makes one compartment with a half-life of 5.5 days. At 70 kg it is a volume of 9.8 L and a clearance of 1.24 L/day.

Inada also fitted a three-pool model of serum, interstitial fluid and cells. For T4, though, the paper prints only its total volume and the cellular pool, not the other two volumes or the exchange rates, so it cannot be built. The one-compartment model has the right clearance and volume for the days and weeks over which levothyroxine is dosed. It misses the distribution phase in the first day after a dose, when serum T4 rises higher and falls faster than this curve.

**Absorption.** The model is built for steady-state dosing, where clearance is what matters (Steven L. Shafer, 2026-11-18):

- **Rate:** from Marguerite Hays's 1991 study (*Thyroid* 1:241-248, doi 10.1089/thy.1991.1.241), Table 1, which gave labelled T4 orally and intravenously to five normal volunteers. Hays called one subject's absorption "abnormally low" (27%), and that subject is left out. In the other four, after a mean delay of 15.3 minutes, the absorptive site empties at 0.0131 /min, either into the body or down the gut.
- **Extent:** 48%, from Surks and colleagues (*J Clin Invest* 1973;52:805-811), Table I: absorption in seven hypothyroid patients taking tablets at steady state. That is what a patient takes.

Other measurements give more. Hays and Nielsen 1994 (*Thyroid* 4:55-64) measured 69.3% in subjects aged 21 to 69 and 62.8% at 70 to 79, but from a tracer solution, not a tablet. The Synthroid label gives the tablet 93% of an oral solution. By this model, Inada's treated patients absorbed 53% (see Checks).

### Checks

Against the source's own subjects and other studies:

- **Nicoloff 1972** (*J Clin Invest* 51:473-483) measured a clearance of 1.30 L/day in eight euthyroid adults averaging 73.3 kg. That is 1.064 L/day per 60 kg, within 0.4% of Inada's. Their half-life was 6.2 days. Sterling 1956 measured 6.7 days.
- **Inada's treated patients.** Three hypothyroid patients took 150 mcg a day and had a mean serum T4 of 9.5 mcg/dL. At their mean weight of 46.3 kg this model predicts 8.55 mcg/dL, 10% low. That corresponds to a bioavailability of 0.53 rather than 0.48.
- **XP-8121's phase 1** (Fitch 2025, *Clin Transl Sci*, doi 10.1111/cts.70244), not used in the model. Healthy volunteers averaging 77.7 kg took 600 mcg of Synthroid. Their baseline-adjusted peak was 47.6 ng/mL at a median of 3.1 hours. This model gives 24.9 ng/mL at 6.7 hours, half the measured peak. It is low and late because it lacks the distribution phase, its absorption is slower without Hays's subject 1, and its bioavailability is for tablets at steady state. The terminal half-life was 144 hours against this model's 132.

### Body size and age

The source's volumes and clearances were adjusted to body weight in proportion. With [the scaling switch](help:models/fat-free-mass) on they scale by fat-free mass; with it off, in proportion to total weight, as the source did. The subjects were adults; children get the same model scaled by size, an extrapolation.

### Dose

Doses are entered as labelled, in micrograms of levothyroxine sodium, orally or intravenously (levothyroxine sodium for injection). Each microgram delivers 0.972 mcg of thyroxine (776.87/798.86 g/mol), which is what the curve shows.

### Pregnancy

Requirements rise by about 47% in the first half of pregnancy (Alexander 2004, *N Engl J Med*). That is a dose titrated to TSH, not a measured change in clearance. Thyroxine-binding globulin also rises in pregnancy, which raises total T4 for the same free T4. Nothing is applied.

### No band

No band is drawn. The total T4 reference interval (6.1 to 11.2 mcg/dL in Inada's laboratory) is a normal range for the hormone, not a therapeutic range for the drug. Levothyroxine is titrated to TSH.
