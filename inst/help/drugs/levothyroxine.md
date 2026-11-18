### What is plotted

The curve is the thyroxine (T4) that the doses add to the serum: total T4 above the patient's own baseline, in ng/mL (10 ng/mL is 1 mcg/dL, the unit T4 is usually reported in). In a patient with no thyroid function (after thyroidectomy or ablation, or with advanced primary hypothyroidism) the baseline is close to zero, and the curve is the serum T4.

In a patient whose thyroid still works, the curve is not the change in measured T4. Levothyroxine lowers TSH, and with it the gland's own secretion. That feedback is not modelled.

### The model

There is no modern population model of levothyroxine, so this model comes from the classical tracer studies of thyroxine kinetics.

**Disposition.** The disposition comes from Inada and colleagues (*J Clin Invest* 1975;55:1337-1348, doi 10.1172/JCI108053). They gave <sup>125</sup>I-labelled T4 intravenously and sampled serum for two weeks. Their Table II gives, for five normal men aged 21 to 28, the terminal exponential of each subject's curve. The volume is 8.4 L and the clearance 1.06 L/day, both per 60 kg of body weight. That makes one compartment with a half-life of 5.5 days. At 70 kg it is a volume of 9.8 L and a clearance of 1.24 L/day.

Inada also fitted a three-pool model of serum, interstitial fluid and cells. For T4, though, the paper prints only its total volume and the cellular pool, not the other two volumes or the exchange rates, so it cannot be built. The one-compartment model has the right clearance and volume for the days and weeks over which levothyroxine is dosed. It misses the distribution phase in the first day after a dose, when serum T4 rises higher and falls faster than this curve.

**Absorption.** The absorption comes from Marguerite Hays's studies, which gave labelled T4 both orally and intravenously:

- **Rate:** from Hays 1991 (*Thyroid* 1:241-248, doi 10.1089/thy.1991.1.241), Table 1. After a delay of 13.7 minutes, the absorptive site empties at 0.0194 /min, either into the body (0.0099 /min) or down the gut (0.0095 /min).
- **Extent:** from Hays and Nielsen 1994 (*Thyroid* 4:55-64, doi 10.1089/thy.1994.4.55), Table 1. Absorption was 69.3% in subjects aged 21 to 69 (n 45) and 62.8% at 70 to 79 (n 8). The paper reports these two groups rather than a model of age, so bioavailability steps from 0.693 to 0.628 at 70 years.

Hays gave the tracer as a solution. The Synthroid label gives the tablet 93% of an oral solution, about 64.5% here. In patients on tablets at steady state, absorption has been measured lower still: 48% (Surks 1973) and, by this model, 53% in Inada's treated patients (see Checks).

### Checks

Against the source's own subjects and other studies:

- **Nicoloff 1972** (*J Clin Invest* 51:473-483) measured a clearance of 1.30 L/day in eight euthyroid adults averaging 73.3 kg. That is 1.064 L/day per 60 kg, within 0.4% of Inada's. Their half-life was 6.2 days. Sterling 1956 measured 6.7 days.
- **Inada's treated patients.** Three hypothyroid patients took 150 mcg a day and had a mean serum T4 of 9.5 mcg/dL. At their mean weight of 46.3 kg this model predicts 12.3 mcg/dL. That corresponds to a bioavailability of 0.53 rather than 0.693.
- **XP-8121's phase 1** (Fitch 2025, *Clin Transl Sci*, doi 10.1111/cts.70244), not used in the model. Healthy volunteers averaging 77.7 kg took 600 mcg of Synthroid. Their baseline-adjusted peak was 47.6 ng/mL at a median of 3.1 hours. This model gives 36.3 ng/mL at 4.9 hours, low and late because it lacks the distribution phase. The terminal half-life was 144 hours against this model's 132.

### Body size and age

The source's volumes and clearances were adjusted to body weight in proportion. With [the scaling switch](help:models/fat-free-mass) on they scale by fat-free mass; with it off, in proportion to total weight, as the source did. The subjects were adults. Below 21 years, the bioavailability of the 21-69 group is used, an extrapolation.

### Dose

Doses are entered as labelled, in micrograms of levothyroxine sodium, orally or intravenously (levothyroxine sodium for injection). Each microgram delivers 0.972 mcg of thyroxine (776.87/798.86 g/mol), which is what the curve shows.

### Pregnancy

Requirements rise by about 47% in the first half of pregnancy (Alexander 2004, *N Engl J Med*). That is a dose titrated to TSH, not a measured change in clearance. Thyroxine-binding globulin also rises in pregnancy, which raises total T4 for the same free T4. Nothing is applied.

### No band

No band is drawn. The total T4 reference interval (6.1 to 11.2 mcg/dL in Inada's laboratory) is a normal range for the hormone, not a therapeutic range for the drug. Levothyroxine is titrated to TSH.
