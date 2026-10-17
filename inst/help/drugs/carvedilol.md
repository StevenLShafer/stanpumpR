### The model

Carvedilol's parameters come from Hwang and colleagues (*J Korean Med Sci* 2023;38:e173), a population analysis of 21 healthy Korean men given a single 12.5 mg dose and then once-daily 25 mg, with 450 plasma samples. Two compartments, with **zero-order absorption** after a lag: apparent clearance 153 L/h, apparent central and peripheral volumes 440 and 754 L, apparent intercompartmental clearance 41.3 L/h, release duration 0.383 hours, lag 0.215 hours.

Every parameter is **apparent**, divided by the oral bioavailability, because the study had no intravenous arm. So the dose enters without being multiplied by a bioavailability, which the apparent volume and clearance already contain, and **no intravenous unit is offered**: an apparent parameter set predicts an intravenous concentration wrong by a factor of one over F.

### Zero-order release, and why it matters here

Most oral models absorb a dose with a rate constant, giving a rising and falling exponential input. This one does not. The paper tried first-order absorption, first-order with a lag, and a dual-absorption model, and reports that none described the data; it selected a constant-rate release instead. Its sampling was dense through the absorption phase, at 15 and 30 minutes and 1, 2 and 3 hours, which is what makes a rectangular input identifiable rather than a curiosity.

So the whole dose arrives at a steady rate over 23 minutes, beginning 13 minutes after it is swallowed. The plotted consequence is a straight climb to a sharp corner: the concentration rises almost linearly, peaks exactly when release stops, and turns over abruptly rather than rounding off. [Carvedilol is the drug this engine mode was built for](help:models/absorption), and the release is delivered as a short infusion on the apparent parameters, which the closed-form engine already solves exactly.

The release duration is the one weak parameter in the fit, with a relative standard error of 70%, and the authors flag it themselves; its bootstrap interval runs from 0.330 to 0.515 hours against an estimate of 0.383. **The first half hour after a dose is the least certain part of this curve.** Everything after absorption rests on much tighter estimates.

### CYP2D6: measured, and deliberately not applied

The source fitted a CYP2D6 effect, and it is real: clearance in its second intermediate-metaboliser group was 32.8% lower, so that group clears 0.672 of what the others do. It is **not** applied here, and the reason is a mismatch of categories rather than any doubt about the finding. The paper splits intermediate metabolisers into two groups and finds reduced clearance in only one of them, while the Patient Profile's **CYP 2D6** field has a single *intermediate* value that cannot tell them apart. Applying the reduction to every intermediate metaboliser would impose a third less clearance on the paper's own first group, in whom it measured none. The curve is therefore the typical patient's, and the fitted effect is recorded in the antihypertensive parameter audit as an unapplied covariate.

### Covariates

None. The paper screened age, height and weight and reports that none was significant, in a cohort whose weight was 70.4 kg give or take 9.6. The parameters take the library's [fat-free-mass scaling](help:models/fat-free-mass) and are used exactly as published when the switch is off, for every patient.

### Effect site and blood pressure

Neither is modelled, and that is worth being clear about for a drug given to lower blood pressure. The paper's pharmacodynamic endpoint is the **heart-rate rise caused by an injection of isoproterenol**, modelled as competitive antagonism, with a half-maximal inhibitory concentration of 16.5 ng/mL. That measures how well carvedilol blocks a beta-agonist challenge. It is not a blood pressure, not a resting heart rate, and not evaluable here, because predicting it would need the isoproterenol dose as a second input. [No antihypertensive in this library plots a blood pressure](help:models/antihypertensives). No band is drawn either: there is no established therapeutic plasma range for carvedilol, and an inhibitory concentration for a drug challenge is not one.

### Where to be careful

The plotted curve is **racemic parent carvedilol**, as the assay measured it. Beta-blocking activity belongs to the S enantiomer, which circulates at roughly half the R concentration, so an S-enantiomer concentration-effect estimate from another study cannot be read against this line; the paper notes one such estimate of 7.7 ng/mL and it refers to a different quantity. The active metabolite 4'-hydroxyphenyl carvedilol is thirteen times more potent than the parent but reaches about a tenth of its concentration, and the source modelled the parent alone after measuring metabolic ratios below 0.1; this model follows it, so the metabolite's contribution to effect is absent rather than zero.

The cohort was 21 healthy young men, and the published carvedilol models fitted in patients with heart failure found that body weight, alpha-1-acid glycoprotein, digoxin and smoking all mattered. None of those can be applied from this fit, so a patient on carvedilol for heart failure is outside the population this curve describes.
