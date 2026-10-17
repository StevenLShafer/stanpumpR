## Antihypertensives: why so few, and no blood pressure

Six antihypertensives are in the drug library: [clevidipine](help:drugs/clevidipine), [fenoldopam in children](help:drugs/fenoldopamPediatric), [carvedilol](help:drugs/carvedilol), [azilsartan](help:drugs/azilsartan), [chlorthalidone](help:drugs/chlorthalidone) and [hydrochlorothiazide](help:drugs/hydrochlorothiazide). Every one of them plots a **concentration**. No drug in stanpumpR plots a blood pressure, and this page explains why that is a statement about the published evidence rather than a gap in the program.

### The problem is not the engine

stanpumpR solves three-compartment models in closed form and would have no difficulty drawing a pressure from an effect-site concentration. What is missing is upstream. Survey the antihypertensives in routine use and three things turn out to be true at once.

**Most have no complete pharmacokinetic model.** A label tells you a half-life, a time to peak and a bioavailability. Those are observable checks on a model, not a model: a half-life does not identify a clearance and a volume, and two of the three numbers a label prints are usually non-compartmental summaries that no single compartment vector reproduces. Of the drugs in the catalog, the great majority have label descriptors and nothing else.

**Where a fit exists, the endpoint is often not a blood pressure.** This is the part that surprises people. The best-fitted human response models in this class are for exercise systolic pressure (metoprolol), exercise heart rate (propranolol, metoprolol), the heart-rate rise caused by an infusion of isoproterenol (carvedilol), urinary sodium excretion (torsemide, furosemide), plasma ACE inhibition (enalapril, lisinopril), plasma renin activity (aliskiren), the ratio of mean pressure to heart rate as a stand-in for vascular resistance (clevidipine), or the *probability* that mean pressure falls by at least a tenth (clevidipine again). Not one of those is the resting pressure a clinician would want to see, and the coefficients of one are not the coefficients of another.

**Where a pressure was fitted, the model is tied to its study.** Amlodipine's is a local linear slope in twelve patients, erect, placebo-corrected, with no ceiling anywhere in it. Terazosin's is an acute fit in healthy volunteers after a single 2 mg dose. The controlled-onset verapamil model is a circadian baseline with fitted *minimum* pressures for one product and one enantiomer. The azilsartan and chlorthalidone exposure-response is a change at week eight, not an hourly trace. Losartan's day-seven effect was larger than its day-one effect with almost no change in concentration, which is direct evidence that no instantaneous concentration-effect model can describe it at all.

Those three findings do not combine into one curve per drug, so stanpumpR does not draw one.

### What is here instead

A **registry**, keyed by model record rather than by drug name. A record is a molecule in one formulation by one route, and, where a paper fitted something, in one population for one endpoint from one publication. Propranolol immediate-release and long-acting are two records. Furosemide tablet and furosemide injection are two records. The three clevidipine studies are three records, because they share no population, no matrix and no endpoint. Each record carries independent evidence fields, so "there is a product" and "there is a model" never look alike.

Alongside it sits a **parameter audit**: every number, with its unit, matrix, population, endpoint, whether it was fitted, fixed or derived, the uncertainty, the *type* of uncertainty the source itself names, and whether the number was read in the primary source or taken on trust. Several of these papers print a plus-or-minus and never say what it is; the audit records that as not reported rather than guessing at a standard deviation.

Both are available from the console, as `antihypertensiveRegistry()` and `antihypertensiveParameters()`.

### The four relations you can evaluate

Four records publish a usable concentration-to-endpoint relation without publishing the pharmacokinetics that would reach it from a dose. `antihypertensiveEndpoint()` evaluates those four against a concentration you supply, and returns the **change** the drug term predicts, never an absolute pressure, because none of them publishes the baseline model an absolute pressure would need. They are the paediatric fenoldopam mean-pressure maximum-effect model, terazosin's acute systolic and diastolic models, and amlodipine's erect systolic slope. Every result carries its population and its warnings, so a fit in anaesthetised children or in healthy volunteers cannot quietly become an adult prediction.

### Signs, and the trap in them

A paper that reports a maximum effect of -27 mmHg is reporting a fall. Store that sign and subtract it and you get a rise. Every effect in the audit is therefore normalised to a positive magnitude with an explicit direction, so that pressure is baseline minus magnitude times the fraction of maximum, and a concentration at the EC50 gives exactly half the fall. For paediatric fenoldopam that is -13.5 mmHg, which the test suite checks.

### What would change this page

For a drug to get a pressure, one source would have to supply the whole chain: dose to concentration, concentration to pressure, a baseline, and a stated measurement context, with the population and the formulation matching. Then the chain would have to reproduce that study's own published trajectory, and only afterwards would an independent validation in another population be worth discussing. Several records are two steps away rather than ten — but notice that the steps taken so far have all been on the *pharmacokinetic* side. Four drugs have joined this page since it was written, and each was blocked by something mechanical rather than evidential: carvedilol needed a zero-order oral release the engine did not have, hydrochlorothiazide needed its Weibull absorption discretised into pulses, and azilsartan and chlorthalidone needed a copy of a paywalled paper. All four now plot a concentration, and **not one of them brought a blood pressure with it.** The azilsartan and chlorthalidone paper does contain a fitted systolic model, and it is driven by eight-week exposure rather than by a concentration, which is exactly the pattern described above.

That is the honest state of this field: the pharmacokinetic obstacles yield to work, and the pharmacodynamic one has not moved at all.

One record has since acquired an **external validation**, the registry's first. Clevidipine's individual volumes are not printed in its source and had to be derived from the four quantities that are; a second study, in a different journal and different subjects, turned out to report the derived quantities as measurements, and the derivation recovers its measured central volume to about a percent. The source tree records that in full.

See also [the drug pages](help:drugs/index) for the six implemented models, and `docs/antihypertensives.md` in the source tree for the full audit, including where the specification this work came from had to be corrected.
