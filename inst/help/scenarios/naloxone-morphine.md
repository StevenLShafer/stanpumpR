## What to look for

Morphine 10 mg at time zero; naloxone 400 mcg at 60 minutes, as might be given to a patient found with a low respiratory rate. The two drugs differ in concentration a thousandfold, so the curves are **normalized to each drug's peak effect site** to put them on one picture. What the picture compares is how long each drug stays at its effect site: it is a kinetic comparison of durations. The program has no model of receptor competition or of breathing, so the curves do not show when reversal is achieved, when it ends, or whether another dose is needed; they show only that the antagonist leaves long before the agonist does.

Naloxone's effect site peaks about three and a half minutes after the dose (its effect-site equilibration half-time is about 7 minutes) and is down to half its peak about a quarter of an hour after the dose. Morphine's effect site, which peaks at about an hour and a half, is still near its peak when the naloxone has largely gone. By two hours the antagonist has left and the agonist remains: the pharmacokinetics of **renarcotization**.

The picture understates the problem, because morphine's effect site is still *rising* when the naloxone is given at 60 minutes.

## Try next

- Add naloxone 400 mcg at 90 and 120 minutes, or an infusion (the units offered include mcg/min and mcg/kg/min), which is how the kinetic mismatch is managed in practice. The plot will show the antagonist's presence extended; whether that presence is enough is a clinical judgement the model does not make.
- Replace the morphine with fentanyl 200 mcg at 0. Now the agonist leaves as fast as the antagonist, and a single dose of naloxone is more likely to suffice.
- Set *Normalize to* to *none* and *Show typical* to *Range* to see the two drugs on their own scales and the morphine against its MEAC band.

## Background

[Naloxone](help:drugs/naloxone) and [morphine](help:drugs/morphine) describe the models. The model says nothing about how much naloxone is needed; that depends on the opioid's receptor affinity and the degree of depression. [Normalization](help:models/normalization) explains the axis.
