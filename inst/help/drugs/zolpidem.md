### The model

Zolpidem's parameters are from Kim and colleagues (*CPT Pharmacometrics Syst Pharmacol* 2026;15:e70208), who gave a single 10 mg tablet to 30 healthy Korean adults (15 men and 15 women, 20 to 44 years, 50 to 83 kg) and sampled for 12 hours: one compartment, with an apparent clearance of 18.0 L/h and volume of 64.0 L, and absorption through a chain of transit compartments (mean transit time 0.25 h) into a depot emptied at 11.7/h. No covariate was retained. Doses are in milligrams of zolpidem tartrate, as tablets are labelled.

The engine has a lag and one absorption rate, not a transit chain. The chain delivers the dose almost as a pure delay (its spread is about 3 minutes either side of 15), so it is represented by a lag of 0.25 h followed by Kim's absorption rate. Compared with the transit model itself, the curves differ only in the first 20 minutes; from 45 minutes on they agree within 0.1 ng/mL, and the peaks are 142.5 and 141.7 ng/mL. During the lag, time until threshold reads blank, as it does for gabapentin and pregabalin.

In the reference man, 10 mg peaks at 143 ng/mL at 35 minutes, with a half-life of 2.5 hours. The US label reports a mean peak of 121 ng/mL (58 to 272) at 1.6 hours and a half-life of 2.5 hours; Greenblatt and colleagues (*J Clin Pharmacol* 2006;46:1469-1480) found about 140 ng/mL after 10 mg. The peak height and half-life agree; the peak comes earlier than in the US studies, as it did in Kim's subjects and in de Haas and colleagues' (median 0.78 hours).

### Oral only

Zolpidem has no intravenous product, and the model was fitted to oral data alone: its clearance and volume are divided by an unknown bioavailability (about 70 per cent; Salvà and Costa, *Clin Pharmacokinet* 1995;29:142-153). They predict oral concentrations correctly and intravenous ones wrongly, so zolpidem is offered only as `mg PO`, with `mg PO qd` for a nightly dose. The extended-release and sublingual products are different inputs and are not represented.

### Covariates

None in the source. With the [fat-free-mass switch](help:models/fat-free-mass) on, the volume and clearance scale with fat-free mass; with it off, everyone receives the published values. The label reports peak and exposure about 45 per cent higher in women at the same dose, and in 2013 the FDA lowered the starting dose for women to 5 mg. Fat-free-mass scaling accounts for part of the difference: a 60 kg, 165 cm woman has a peak about 35 per cent and an exposure 26 per cent higher than the reference man. The rest, which Greenblatt and colleagues (*J Clin Pharmacol* 2014;54:282-290) found was not explained by body size, is not modelled. Nor is age: the elderly clear zolpidem about half as fast (Olubodun and colleagues, *Br J Clin Pharmacol* 2003;56:297-304).

### Effect site

None; only the plasma concentration is plotted, and it is the concentration that drives the effect. Kim related the Digit Symbol Substitution Test, choice reaction time and sleepiness directly to plasma concentration, and an effect compartment did not improve the fit. The largest changes in the tests coincided with the plasma peak. No human ke0 for zolpidem has been published. Effects also wane faster than plasma concentrations fall (de Haas and colleagues, *J Psychopharmacol* 2010;24:1619-1629), an acute tolerance that an effect site cannot represent.

### Typical concentrations and the driving threshold

The shaded band, 80 to 200 ng/mL, is a therapeutic range quoted by Cha and colleagues (*Pharmaceutics* 2024;16:689); the typical value, 120 ng/mL, is the label's mean peak after 10 mg. The default **time-until-threshold** level is **50 ng/mL**, read against plasma: in January 2013 the FDA warned that "zolpidem blood levels above approximately 50 ng/mL appear capable of impairing driving to a degree that increases the risk of a motor vehicle accident". After 10 mg the reference man falls below it about 4.4 hours after the dose. A woman, an older patient, or anyone taking the dose late in the night will be above it for longer.

### Where to be careful

The model was fitted to 30 young healthy adults after a single fasting dose. Food delays and lowers the peak; CYP3A4 inhibitors raise concentrations; hepatic impairment slows clearance. Zolpidem adds to the sedation and respiratory depression of opioids and other sedatives. Kim's DSST and reaction-time models found impairment at half its maximum near 205 and 282 ng/mL, far above the driving threshold: laboratory tests are less sensitive than driving to the impairment zolpidem causes.
