### The model

Suzetrigine's parameters come from the applicant's final population model (model 2822). It is printed in the FDA's Integrated Review of Journavx (NDA 219209, 2025: Table 149 and Figure 24). No population model of suzetrigine has been published in a journal.

The model was fitted to 1,435 subjects in 12 studies: healthy volunteers, people with hepatic impairment, and patients after bunionectomy or abdominoplasty. Absolute bioavailability has not been measured, so every value is apparent.

**Suzetrigine:**
- two compartments: central volume 103 L, peripheral volume 204 L, intercompartmental clearance 19 L/h;
- clearance 5.56 L/h in women and 1.21 times that in men, times (BMI/27.8)<sup>−0.605</sup>;
- tablet absorption: after a lag of 0.182 h, the dose is released into the gut over 1.1 h and absorbed from there at first order.

**Its active metabolite, [M6-SUZ](help:drugs/M6SUZ):**
- forms from 80% of suzetrigine's clearance;
- has a central volume of 1.7 L, a peripheral volume of 29.5 L and an intercompartmental clearance of 8.75 L/h;
- is cleared at 0.273 times suzetrigine's clearance;
- is 3.7 times less potent at NaV1.8 than suzetrigine and reaches about three times its exposure.

Every volume scales with weight. The two intercompartmental clearances, which carry no size term in the review, scale with fat-free mass to the 0.75 power when [the scaling switch](help:models/fat-free-mass) is on, anchored at the data's median subject (76.2 kg, BMI 27.8, 42 years, 77% men), so that subject receives 19 and 8.75 L/h; with the switch off they are fixed.

### Autoinduction

Suzetrigine induces its own clearance. An enzyme state, starting at 1, multiplies every clearance of both suzetrigine and M6-SUZ. It turns over at 0.00939 /h, a half-life of 74 hours, and is induced by up to 1.81-fold. The induction is half-maximal at 432 ng/mL, with a Hill coefficient of 10, so it acts almost as a switch above about 400 ng/mL.

The enzyme rises over the first week and holds suzetrigine's trough near that level. The review's own numbers show the effect: an apparent clearance of 7.01 L/h after a single dose and 13.9 L/h at day 14.

Without induction the curve would be within 4% for 48 hours but 31–55% too high by day 14. The library's saturable engine therefore carries the enzyme as a state, through the whole dose table.

### The dose effects

Table 149 prints the parameters of two dose effects but not their equations. They are applied in the usual form for such parameters:

- **Bioavailability** is 0.88 × (1 − 0.419 D<sup>5</sup>/(162<sup>5</sup> + D<sup>5</sup>)): 0.88 at 50 mg and 0.85 at 100 mg.
- **The absorption rate constant** is 4.52 × (1 − 0.895 D/(36 + D)) /h: 2.17 /h at 50 mg and 1.55 /h at 100 mg, so the larger first dose peaks later.

D is the dose in mg. The forms are an assumption, checked only by the agreement below.

### Checks

Table 150 is the applicant's simulation of this model for 100 mg followed by 50 mg every 12 hours, so it checks the implementation rather than the model. This model's values are for a woman at a BMI of 27.8.

| | Table 150 (means) | This model |
|---|---|---|
| Suzetrigine AUC, first 48 h | 15.8 µg·h/mL | 18.1 µg·h/mL |
| Suzetrigine peak, first 48 h | 0.558 µg/mL | 0.61 µg/mL |
| Suzetrigine AUC over 24 h, steady state | 11.5 µg·h/mL | 11.0 µg·h/mL |
| Suzetrigine peak, steady state | 0.623 µg/mL | 0.65 µg/mL |
| M6-SUZ AUC over 24 h, steady state | 34.7 µg·h/mL | 32.4 µg·h/mL |
| M6-SUZ peak, steady state | 1.52 µg/mL | 1.44 µg/mL |

### Covariates and assumptions

The review prints no reference weight or BMI, so the data's medians are assumed: 76.2 kg and 27.8 kg/m².

- **Weight** acts on the volumes. With [the scaling switch](help:models/fat-free-mass) on it is the pharmacokinetic weight; with it off, total weight.
- **BMI** acts on clearance. It is computed from total weight in both positions of the switch, because it stands for body composition: a heavier patient of the same height clears suzetrigine more slowly. It is held within the range studied, 18.1 to 39.7.
- **Fat-free mass** acts on the two intercompartmental clearances when the switch is on (stanpumpR's rule that a parameter without a size term in its source still scales with size, Steven L. Shafer, 2026-11-19).

Not modelled:
- food, which delays the peak to 5 hours;
- hepatic impairment;
- CYP3A inhibitors and inducers (strong inhibitors are contraindicated);
- the slower first-dose absorption of the abdominoplasty patients.

### Dose and band

Journavx tablets contain 50 mg of suzetrigine (no salt). The regimen is 100 mg first, on an empty stomach, then 50 mg every 12 hours. No therapeutic range has been established.
