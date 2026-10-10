## What to look for

A 400 mg dose of lidocaine (20 mL of 2%) was drawn up for a nerve block, but all of it went into a vein. The dose table has it as **mg**, an intravenous bolus, rather than the **mg RA** of the block. The y axis is logarithmic, so the effect site and the first-minute plasma fit on one plot.

- **Plasma** (dashed) starts at about 65 mcg/mL and is still 29 mcg/mL at one minute, 7 mcg/mL at five. The planned block never takes the plasma above 1.8 mcg/mL.
- **Effect site** (solid) rises behind it, passes 5 mcg/mL at about 1.5 minutes, peaks at about 6.8 mcg/mL at 5 minutes and stays above 5 mcg/mL until about 20 minutes. Central nervous system toxicity (perioral numbness, tinnitus, then seizures) begins above about 5 mcg/mL in most patients; see [Lidocaine](help:drugs/lidocaine).

Lidocaine is the clearest drug for this lesson because it has an effect site; [bupivacaine](help:drugs/bupivacaine) and [ropivacaine](help:drugs/ropivacaine) are modelled as plasma only.

Two limits matter here more than in most scenarios. The first-minute plasma is the model's assumption that the dose mixes instantly in the central volume; the real arterial concentration after a rapid injection rises and falls faster than that, and an injection into an artery that supplies the brain (the vertebral or carotid, during a neck block) can cause a seizure within seconds from a few milligrams, which no part of this model represents. And the effect site's 5-minute time to peak was set for lidocaine's analgesic effect, not for toxicity: read the timing as the shape of the delay, not a guarantee of minutes in hand.

## Try next

- Change the units to **mg RA**: the block as intended. The plasma rises over 40 minutes to about 1.8 mcg/mL and the effect site follows to the same level about 20 minutes later, nowhere near 5.
- Make the first row **100 mg** (the first 5 mL) and add a row of **300 mg RA** at 1 minute: an incremental injection in which the first increment went into the vein, the injection was stopped, and the rest was placed correctly. The effect site peaks below 2.2 mcg/mL. Injecting in small increments, with aspiration and observation between them, is what keeps the intravascular part to one increment.
- Turn the log axis off to see how far above the block's curve the first minutes of plasma are on an ordinary scale.

## Background

[Regional anesthesia doses](help:models/absorption) explains how an **mg RA** dose is absorbed from tissue. [Local anesthetics after a nerve block](help:scenarios/regional-anesthesia-absorption) shows lidocaine, bupivacaine and ropivacaine given as intended, and [Mepivacaine: fast and slow absorption from one block](help:scenarios/mepivacaine-two-depot-absorption) shows a two-depot absorption.
