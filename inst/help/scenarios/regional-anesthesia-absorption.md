## What to look for

Three blocks given at the same moment, each as a dose in **mg RA**: lidocaine 400 mg, bupivacaine 150 mg and ropivacaine 175 mg, injected into tissue rather than a vein. Each dose sits in a depot at the injection site and is absorbed into the circulation by first-order kinetics, so the plasma concentration rises over the first hour instead of starting at its highest.

- **Lidocaine** peaks at about 1.8 mcg/mL at 40 minutes and is mostly gone by 4 hours. Its absorption rate comes from axillary block with epinephrine.
- **Bupivacaine** peaks at about 1.5 mcg/mL at 50 minutes and falls more slowly, because its intravenous clearance is about a third of lidocaine's. Its absorption rate was matched to the peak after femoral and sciatic block with plain solution.
- **Ropivacaine** peaks at about 1.3 mcg/mL, but not until close to 2 hours, and is still above 1 mcg/mL at 4 hours. Its absorption rate was matched to the peak after axillary block with plain solution. Absorption, not elimination, sets the late part of its curve: the concentration falls with the absorption half-time (about 2 hours), not the intravenous one. Vainionpää and colleagues saw this flip-flop as a terminal half-life of 7 hours after the block against 1.7 hours intravenously.

The curves are total (bound plus unbound) drug in plasma. They are not the block, and they do not predict local anesthetic systemic toxicity on their own, which depends on the unbound concentration, how fast it rises, and the patient. Bupivacaine and ropivacaine therefore have no shaded band. Lidocaine's band, 0.5 to 1.5 mcg/mL, is the range for intravenous analgesia, not a target for a block.

Each drug has a single absorption rate taken from one study of one block, and absorption differs a great deal with the site (fastest after intercostal and epidural injection, slowest after subcutaneous and lower-limb blocks) and with epinephrine. Read the shapes and the order of magnitude, not the exact values.

## Try next

- Change one row's units from **mg RA** to **mg**: the same dose given intravenously, as an unintended intravascular injection would give it. The first few minutes are far above anything the block produces. In the first minute the model assumes the dose is instantly mixed in the central volume, so the very first value overstates what a real artery would carry, but the difference in scale is the point.
- Give a second ropivacaine block of 100 mg at 2 hours, as a rescue block might be given. Much of the first dose is still in the tissue, so the two add: the peak rises to about 1.9 mcg/mL at about 3 hours.
- Set the age to 30. Nothing changes for ropivacaine: its model is from patients 61 and over and has no age term, so it overpredicts a younger patient's exposure; see [Ropivacaine](help:drugs/ropivacaine).
- Add [mepivacaine](help:drugs/mepivacaine) 400 mg RA. It is absorbed through a fast and a slow depot in parallel, so it reaches about 2.1 mcg/mL within 10 minutes, peaks at about 2.5 mcg/mL near 30 minutes, and then falls only slowly as the slow depot keeps feeding it.

## Background

[Regional anesthesia doses](help:models/absorption) explains the tissue depot and first-order absorption. [Lidocaine](help:drugs/lidocaine), [bupivacaine](help:drugs/bupivacaine) and [ropivacaine](help:drugs/ropivacaine) record where each absorption rate comes from and how far it can be trusted.
