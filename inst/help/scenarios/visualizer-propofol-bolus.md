## What to look for

Propofol 140 mg (2 mg/kg) as a bolus at time zero, in a 70 kg, 40-year-old man. *Graph Options → Peripheral compartments* is on, so V2 and V3 are drawn under the propofol panel. This run is made to be watched: double-click the propofol panel, then press **Minto Visualizer** in the *Edit propofol doses* dialog. The visualizer opens in its 2D view, where each compartment is a box whose fill height is its concentration, all on one scale, so drug can be seen running downhill from one box to the next. Press **Play**.

**The bolus fills V1 alone.** In this man V1 is 6.3 L, V2 23.6 L and V3 157 L. At 30 seconds the plasma concentration is 16.2 mcg/mL, and V1 holds 102 of the 140 mg. V2 and V3 are still nearly empty, so thick green arrows carry drug out of V1 into both.

**The effect site follows, a little late.** The thin blue bar beside V1 is the effect site. It peaks at 8.2 mcg/mL at 1.6 minutes, at the moment the falling plasma comes down to meet it, also at 8.2: until then the effect site's level was below the plasma's and drug flowed in, and from then on it flows out.

**The effect wears off as much by redistribution as by elimination.** The effect site leaves the band, falling below its lower edge of 2.5 mcg/mL, at 6 minutes, and falls below 1 mcg/mL, about where patients wake, at 10.4 minutes. By then 66 mg (47% of the dose) has been eliminated, and 69 mg (49%) has moved into V2 and V3: only 5 mg is left in V1. Elimination alone would have taken far longer to empty V1; it is the periphery's uptake, as large as elimination, that makes the fall so fast.

**The flow between V1 and V2 turns round when their levels cross.** At 4.5 minutes V2 reaches its peak, 2.0 mcg/mL, at the same moment as the plasma falls to 2.0. Watch the arrow between them turn from green (out of V1) to red (into V1): from then on V2 is giving drug back, because its level is now the higher. Drug always flows down the concentration gradient, and the boxes' heights show the gradient directly.

**V3 is still filling at the end of the hour.** V3's level stays low, below 0.23 mcg/mL, but it is 25 times larger than V1, so it keeps taking up drug until 49 minutes, and at an hour it holds 35 mg, a quarter of the dose.

The boxes are not drawn to scale: V3 is 25 times V1, and the visualizer caps the ratio of the largest box to the smallest at 10, as the note above the boxes says. The heights, the concentrations, are exact.

## Making a video

1. Choose the view: **3D view** (or the **v** key) switches to Charles Minto's 3D vessels; drag to turn the scene to the viewpoint you want. The video is made from the view on the screen.
2. Leave the speed where the run opens, 1 s = 30 s, so the hour plays in 2 minutes.
3. Press **Video**, then **Save MP4**. The video, about 2 minutes long, is made in the browser and saved to your computer. The patient's weight, height, age and sex appear on it only if you tick the box for them.

For a lecture, the first ten seconds of the video carry the story: the bolus, the effect site's peak, and the turn of the V1–V2 arrow. Pause it there and ask the audience where the drug went.

## Try next

- Double the bolus to 280 mg. Every level doubles, and the effect still wears off by redistribution, only later: the effect site falls below 2.5 mcg/mL at about 9 minutes, 3 minutes later.
- Load [Watching a TCI pump fill the compartments](scenario:visualizer-propofol-tci) to see the same compartments kept full rather than left to drain.

## Background

[Peripheral compartments and the depot](help:models/compartments) explains V2 and V3. [Propofol](help:drugs/propofol) gives its parameters.
