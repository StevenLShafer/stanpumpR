## What to look for

A propofol effect-site target of 3 mcg/mL from time zero, set to zero at one hour, in a 70 kg, 40-year-old man. *Peripheral compartments* is on, so V2 and V3 are drawn under the propofol panel. To watch it, press **Visualizer** under the plot (or double-click the propofol panel and press **Visualizer** in its dialog), then **Play**. The gold line on the thin blue effect-site bar is the target.

**A 10-second loading burst.** The pump runs at 306 mg/min for 10 seconds, giving 51 mg, and V1's level leaps to 7.7 mcg/mL, far above the target. The arrow is drawn at its thickest, and its label gives the rate.

**Then the pump stops while the effect site catches up.** For a minute and a half nothing is given. V1's level falls as it loses 29 of its 48 mg, 17 mg into V2 and V3 and 12 mg to elimination (the effect site's share is too small to see), and meanwhile the blue bar rises to the gold line, reaching 3 mcg/mL at 1.65 minutes. The pump starts again at 1 minute 40 seconds, at 10 mg/min.

**The rate falls as the peripheral compartments fill.** The target holds V1 and the effect site at 3 mcg/mL, so the pump only has to replace what leaves V1. At 2 minutes that is 11.0 mg/min; at 10 minutes 9.2; at 30 minutes 7.2; at 59 minutes 6.5. The pump's arrow narrows as the arrows out to V2 and V3 narrow, because those levels are rising towards V1's: at an hour V2 is at 2.97 mcg/mL, nearly level with V1, and V3 at 0.75.

**After the target is set to zero, the effect site falls fast and V3 still fills.** The effect site falls to half the target, 1.5 mcg/mL, in under 6 minutes, because V2 and V3 can still take drug from V1. V3's level, far below the plasma's, goes on rising for another 22 minutes, to 0.79 mcg/mL. At an hour V3 held 117 of the 498 mg given, and 292 mg had been eliminated.

The boxes are not drawn to scale: V3 is 25 times V1, and the visualizer caps the ratio at 10, as the note above the boxes says. The heights, the concentrations, are exact.

## Making a video

1. Choose the 2D or 3D view (**3D view** or the **v** key) and, in 3D, drag the scene to the viewpoint you want.
2. Leave the speed at 1 s = 1 min, where this two-hour run opens: the video is 2 minutes long.
3. Press **Video**, then **Save MP4**.

The loading burst and the pause take the first two seconds of the video. For a lecture on how a TCI pump works, show them live as well: in the visualizer, set the speed to 1 s = 5 s and play the first two minutes, pausing at the burst and again when the pump restarts. (The video always holds the whole run, so it cannot be made at that speed: two hours at 1 s = 5 s would be 24 minutes, past the 5-minute limit.)

## Try next

- Set the target to the plasma instead (*Plasma target* in place of *Effect site target*). There is no overshoot: V1 is raised straight to 3 mcg/mL, and the effect site follows slowly, reaching 2.5 mcg/mL at 3 minutes and 2.9 at 5.
- Raise the target to 4 mcg/mL at 30 minutes. The pump gives another, smaller burst, 24 mg, and pauses for about 50 seconds while the effect site rises to the new line.
- Load [Watching a propofol bolus redistribute](scenario:visualizer-propofol-bolus) to see the same compartments left to drain.

## Background

[Peripheral compartments and the depot](help:models/compartments) explains V2 and V3. [Propofol](help:drugs/propofol) gives its parameters.
