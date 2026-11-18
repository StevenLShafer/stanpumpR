## What to look for

Three months of oral phenytoin in a 70 kg 40-year-old man, as extended-release capsules once a day: 200 mg/day for four weeks, 300 mg/day for the next four, 400 mg/day for the last five. The steps in the dose are equal. The steps in the concentration are not.

Over the last day of each step the curve runs between these values, with the average the model predicts for that dose rate alongside:

| Dose | Trough to peak | Average |
|---|---|---|
| 200 mg/day | 4.2 to 5.6 mg/L | 5.0 mg/L |
| 300 mg/day | 9.0 to 11.1 mg/L | 10.3 mg/L |
| 400 mg/day | 19.6 to 22.4 mg/L | 22.0 mg/L |

The first 100 mg a day adds about 5 mg/L. The second adds about 12. That is saturable elimination: phenytoin's maximal elimination rate in this patient is 521 mg/day of phenytoin acid, and the three dose rates ask for 35, 53 and 71 per cent of it. As the demand approaches the maximum, the concentration that balances it rises steeply.

Watch the shaded band, 10 to 20 mg/L. On 300 mg/day the average sits on its lower edge, with the trough just below. One more 100 mg capsule carries the patient across the band and out of the top of it, where nystagmus begins.

The approach to each new level slows as well. The concentration reaches 90 per cent of its new value about 2 days after the first step, 3 days after the second and 9 days after the third, and at day 90 it is still climbing. A drug with first-order elimination would take the same number of half-lives every time.

## Try next

- Change the third step from 400 to 350 mg/day (edit the dose at day 56). The average settles near 14.8 mg/L instead of 22, inside the band. This is why phenytoin is titrated in 25 to 50 mg steps once the concentration is over about 10 mg/L.
- Set **CYP 2C9** in the Patient Profile to `*1/*3`. The maximal elimination rate falls by a third, to 349 mg/day, and the whole titration moves up: 9.2 mg/L at the end of the first step instead of 4.2, and 25 mg/L at the end of the second. The third step now asks for 368 mg/day, more than the patient can eliminate, so there is no steady state at all and the curve simply keeps rising. Treat the late numbers as a direction, not a prediction: the model has no ceiling and a real patient's absorption and binding do.
- Put the whole plot on a log Y axis under Graph Options. The first-order drugs in the library fall as straight lines on log axes; phenytoin's climb between steps does not have a constant slope.
- Add a loading dose at time zero: 18 with units `mg/kg PE`, which is fosphenytoin in phenytoin sodium equivalents. It reaches 18.6 mg/L about four hours later, is still at 15.3 mg/L at a day, and has fallen to 6.5 mg/L by the end of the first week. A load buys the therapeutic range at once; the maintenance dose alone decides where the patient ends up, and here it is 5 mg/L.

## Background

[Phenytoin](help:drugs/phenytoin) records the Odani model, the salt and prodrug conversions, and where the CYP2C9 multiplier comes from. Phenytoin is the only drug in the library that is not simulated by the closed-form solver: superposition does not hold when clearance depends on concentration, so the whole dose table is integrated numerically.

The doses here are the ordinary oral ones and the patient is a typical adult. They are chosen to make the shape of saturation visible, not as a dosing recommendation; see [Cautions](help:cautions).
