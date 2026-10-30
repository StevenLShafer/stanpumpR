### The model

Intravenous propranolol's parameters come from Fagan, Walle and colleagues in Charleston (*Br J Clin Pharmacol* 1982;13:571-574), who gave six healthy men 0.05 mg/kg at 1 mg/min and sampled from the first minute to eight hours, to capture the early distribution that earlier studies had missed. Three exponentials fitted best: a half-life of 0.8 minutes, then 13 minutes, then 3.4 hours. A triexponential and its dose determine a three-compartment model exactly, and the mean curve gives a central volume of 11.1 L, a clearance of 1.11 L/min and a steady-state volume of 264 L. The area under that curve is the paper's own 57.6 ng·h/mL.

### Checked against a second study

Cheymol and colleagues in Paris (*J Clin Pharmacol* 1987), with a different assay and an 8 mg infusion over 10 minutes, found a clearance of 75.9 L/h (1.27 L/min), a steady-state volume of 319 L and a terminal half-life of 3.1 hours in twelve normal-weight volunteers. This model gives 1.11 L/min, 264 L and 3.4 hours, all within 17%. In their obese subjects clearance and volume were both **lower**, not higher. No size scaling predicts that: fat-free mass also rises with weight, so [scaling to fat-free mass](help:models/fat-free-mass) raises both, if less than scaling to total weight does, and the obese and normal-weight groups also differed in age and sex. The comparison is a caution about extrapolating to obese patients, not a validation of the scaling.

### Dose basis and covariates

Doses are propranolol hydrochloride, as labelled. No covariates were fitted; the parameters are those of the study's mean 76 kg man, scaled to fat-free mass when the switch is on.

### Where to be careful

Six young healthy men. Propranolol's clearance follows hepatic blood flow, so it falls with cardiac output, in heart failure and in shock. The racemate is measured, and only l-propranolol is a beta blocker.

The effect site peaks **5 minutes** after a bolus. That is a clinical estimate, not a measurement. Achong and colleagues (*Clin Pharmacol Ther* 1976;19:148-152, read in full) gave 10 mg over 10 minutes to six healthy men and found the fall in exercise tachycardia largest 5 minutes after the infusion ended, but that was their first measurement; the next was at 90 minutes. Their study therefore bounds the onset rather than locating the peak — nothing was measured before 5 minutes, or between 5 and 90 — so it is consistent with a peak 5 minutes after a bolus without measuring one. Riva and colleagues (*J Cardiovasc Pharmacol* 1980;2:707-714, read in full) gave 0.1 mg/kg intravenously over 2 to 3 minutes to ten patients and measured heart rate and the atrial and AV-nodal refractory periods alongside the plasma concentration. The effects on all three were first seen 5 to 8 minutes after the injection and were significant over 5 to 45 minutes. That paper bounds the onset rather than locating the peak, for the same reason Achong's does: its first measurement is at 5 minutes and its next at 15, so an effect peaking at or before 5 minutes cannot be distinguished from one peaking earlier. No heart rate is predicted. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
