## What to look for

Lamotrigine 100 mg twice daily in a 70 kg adult, with valproate 500 mg twice daily added on day 21. **Include antiseizure drug interactions** is ticked under Graph Options (the scenario ticks it); two lamotrigine lines are drawn, the solid one with the interaction and the lighter *lamotrigine (no interactions)* without it.

Before day 21 the two lines sit together near 3.6 mg/L. The moment valproate starts, lamotrigine begins to climb, and over about two weeks it roughly doubles, settling near 7.2 mg/L. Valproate itself reaches about 65 mg/L, inside its 50–100 mg/L range.

The rise is not instant, but it is not slow for an inhibition either: valproate inhibits lamotrigine's glucuronidation as soon as it is present, and the two-week climb is simply lamotrigine approaching a new steady state at its new, halved clearance (a longer half-life). This is why lamotrigine is started at half the usual dose, and titrated more slowly, in a patient already on valproate: the same maintenance dose gives twice the level.

## Try next

- Lower the valproate dose. Even 125 mg twice daily inhibits lamotrigine almost as much: the interaction is half-maximal at 5.6 mcg/mL of valproate (Gidal 2003), well below any therapeutic level, so it is nearly saturated throughout the usual range.
- Remove the valproate row (set its dose to 0 on day 21 instead of starting it). The second line never separates from the first.
- Switch valproate for carbamazepine 400 mg twice daily. Now lamotrigine *falls*, by about a third: carbamazepine induces the same enzyme valproate inhibits, and the change builds over the same two weeks.

## Background

[Antiseizure drug interactions](help:models/antiseizure-interactions) describes the model, its sources and its limits. The magnitude here is from Rivas 2008 (600 patients, lamotrigine clearance × 0.49 with valproate); the concentration dependence is from Gidal 2003.
