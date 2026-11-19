## What to look for

Morphine 30 mg by mouth as a tablet at time zero, in a 70 kg, 40-year-old man. *Graph Options → Peripheral compartments* is on, so under the morphine panel, which is the plasma (V1), come **V2** and **V3**, in mcg/mL, and the **depot**, the amount of drug still to be absorbed, in mg. See [Peripheral compartments and the depot](help:models/compartments).

**The depot holds the bioavailable dose.** Morphine's oral bioavailability in the model is 29%, so the depot receives 8.7 mg, not 30. It receives it after the absorption lag of about 18 minutes, which is why the depot panel steps up from zero rather than starting full: in the model the dose is not in the depot until the lag has passed. It then empties exponentially with ka, 0.013 per minute, a half-time of about 53 minutes: 5.0 mg is left at an hour, 2.3 mg at two hours and 0.5 mg at four.

**The plasma peaks while the depot is still half full.** The plasma concentration peaks at about 47 minutes, when absorption from the depot no longer keeps pace with distribution and elimination; the depot still holds more than half of what it received. V2 peaks at about two hours and V3 later still, so the drug moving out of the plasma into the tissues is part of why the plasma falls while absorption continues.

**The effect site peaks last of all**, at about 3.7 hours, long after the plasma. Morphine's slow equilibration with its effect site is a separate delay from the absorption, and the two add.

Hover over the depot panel to read the amount left at any moment.

## Try next

- Add morphine 5 mg IV at time zero. The depot is unchanged, because an intravenous dose never passes through it, and the plasma and both peripheral compartments rise at once.
- Change the unit to *mg PO liquid*. The oral solution is absorbed faster, so the depot empties sooner and the plasma peaks earlier and higher.
- Replace the morphine with oxycodone 10 mg PO and compare the depot's half-time with morphine's.
- Untick *Peripheral compartments* to return to the usual one panel per drug.

## Background

[Oral, intramuscular, intranasal, subcutaneous and regional anesthesia doses](help:models/absorption) explains the depot and the absorption lag. [Morphine](help:drugs/morphine) gives its parameters.
