### The model

Clevidipine's parameters come from Ericsson and colleagues (*Br J Clin Pharmacol* 1999;47:531-538), a study of 25 healthy men given 20-minute infusions at dose rates from 0.12 to 48 nmol/min/kg, 21 of them twice. A population analysis of sparse whole-blood samples found two compartments best, and the paper reports clearance 0.121 L/min/kg, steady-state volume 0.56 L/kg, and initial and terminal half-lives of 1.8 and 9.5 minutes. The concentration rose linearly with dose rate across that whole range.

The paper does not print the individual volumes or the intercompartmental clearance, and it does not need to: a two-compartment model has four free parameters, and those four reported numbers fix them uniquely. The decomposition gives a central volume of 0.369 L/kg, a peripheral volume of 0.191 L/kg and an intercompartmental clearance of 0.0164 L/min/kg, every one positive. These parameters are therefore **derived, not fitted**, and two checks support them: the area under the curve per unit dose comes to exactly 1 over the clearance, and the fast phase carries 81.7% of it, against the paper's own statement of "more than 80%" -- a number the derivation never used.

### What is plotted

**Whole blood**, in ng/mL, which is the matrix this study assayed. Clevidipine is cleared by blood and tissue esterases and is reported in whole blood rather than plasma. The postoperative cardiac-surgery study (Bailey and colleagues, *Anesthesiology* 2002;96:1086-1094) measured **plasma** instead, and reported threshold concentrations of 9.7 and 26.3 mcg/L for a 10% and a 20% fall in mean arterial pressure. Those numbers must not be read against this curve: the matrix is different with no measured blood-to-plasma ratio, and a threshold concentration from a logistic fit is the point of 50% *probability* of crossing a threshold, not the concentration giving half of a continuous effect.

### Covariates

None in the source. Every published parameter is per kilogram, which is this model's own size term, linear in weight; it is applied to the fat-free-mass weight when [the scaling switch](help:models/fat-free-mass) is on and to total body weight when it is off, so unticking the box reproduces the published per-kilogram values exactly.

### Effect site and blood pressure

Neither is modelled. No equilibration rate constant is published, so ke0 is zero and only the blood concentration is drawn. The source did fit a maximum-effect model, but to the **ratio of mean arterial pressure to heart rate** -- a surrogate for vascular resistance, not a blood pressure -- and its coefficients could not be verified against the full text, with reachable secondary sources disagreeing about them. They are recorded as unverified in the antihypertensive parameter audit and are not used here. No shaded band is drawn: there is no therapeutic whole-blood range, and the postoperative thresholds are a different matrix.

### Where to be careful

This is a **healthy-volunteer** disposition model. The paper notes that the blood-pressure effect was marginal in these subjects, "probably due to a compensatory baroreflex activation", so the population is one whose haemodynamic reflexes were deliberately intact -- the opposite of the anaesthetised or postoperative patient in whom clevidipine is actually titrated. Clearance in postoperative cardiac-surgery patients was reported as 4.3 L/min for a three-compartment model, and in a 72-hour hypertension study as roughly 26 to 33 L/min by cohort; those are different populations and, in the postoperative case, a structure this vector cannot represent, since three compartments need six parameters and that paper reports three numbers. Simulate the curve as a disposition study, not as a blood-pressure prediction.
