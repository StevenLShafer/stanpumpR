### The model

Enalaprilat is the active moiety: it is what inhibits converting enzyme, while [enalapril](help:drugs/enalapril) itself is an inactive prodrug. It is **not dosed directly here** — it has no dose units and does not appear in the dose table's drug list or the startup menu. Its whole curve comes from the enalapril you give, folded onto a row of its own, the same arrangement [salicylate](help:drugs/salicylate) has with aspirin.

Parameters come from Faisal, Cawello, Burckhardt, de Hoon and Laer (*Front Pediatr* 2019;7:281), the same simultaneous serum-and-urine model as the parent. Two compartments:

- central volume 46.1 L;
- elimination rate constant 0.184 /h, so a clearance of 8.48 L/h;
- distribution rate constants 0.060 and 0.054 /h, giving an intercompartmental clearance of 2.77 L/h and a peripheral volume of **51.2 L** — not fitted, but implied by the two rate constants;
- a delay of two transit compartments before the metabolite appears, with a mean of 0.910 h.

The implied half-lives are **2.7 and 18.1 hours**. That second one is the point of the two-compartment structure: the source tried one compartment and reports that it could not describe the long terminal phase at all.

### These numbers are absolute, which is rare for a metabolite

Because the source measured both moieties in urine as well as serum, and both are cleared renally and by nothing else, the mass balance closes and the parent's bioavailability became a parameter of its own (0.606). So this volume and this clearance are real litres and real litres per hour, not quantities divided by an unmeasured fraction converted. A metabolite's disposition is usually only knowable up to that fraction.

### Why there is no intravenous enalaprilat

Intravenous enalaprilat is a real product, and because these parameters are absolute the arithmetic for an injected dose would actually run. It is still not offered, for a reason the library applies generally: **a parameter set is used for what its source measured.** These were fitted to enalaprilat formed from an oral dose of enalapril and never to an injection.

There is a specific trap behind that caution. The enalaprilat label quotes an effective accumulation half-life of about 11 hours, and that number comes from repeated **oral enalapril** — it is a property of the formation-limited profile, not an intravenous elimination half-life. Reading it as one would give a badly wrong curve. The antihypertensive registry carries the intravenous record separately, with that warning, and says what a usable model would need.

### What the curve shows

After a single 10 mg enalapril tablet, enalaprilat peaks near 56 ng/mL at about 3.3 hours — the source reports its own peak "in around 3 to 4 h" — and declines biphasically with that 18-hour terminal phase. About 69% of the absorbed enalapril becomes enalaprilat, which is simply the ratio of the two parent elimination paths the source fitted.

Because the terminal half-life is long and the parent's is under an hour, twice-daily dosing accumulates enalaprilat while enalapril stays a transient spike. The metabolite is the exposure that matters.

### Two places where the paper disagrees with itself

Worth knowing for anyone checking these numbers against the text. The Discussion quotes the elimination rate constant as 0.175 /h where Table 1 estimates **0.184**, and quotes the parent's total elimination rate constant as 0.93 /h where its own two components sum to **0.993**. Table 1 is used in both cases, being the parameter table that carries the relative standard errors and both confidence intervals. Neither discrepancy changes anything material, but a reader who finds them should know which was taken.

### No covariates, no effect site, no band

The source tested age, weight, height, sex and total body water stepwise and retained weight on the **parent's** central volume only. Nothing was retained for this moiety, so with the [fat-free-mass switch](help:models/fat-free-mass) off these published values stand at every body size, and with it on the library's own scaling applies.

In particular there is **no renal term**, although enalaprilat is cleared renally and by nothing else. The population was 24 healthy young adults; a serum creatinine entered in the [Patient Profile](help:patient-profile) does not reach this drug.

Converting-enzyme inhibition is a genuinely measurable endpoint for this moiety, and it is still not plotted: [no antihypertensive in this library plots a blood pressure or a pharmacodynamic effect](help:models/antihypertensives). No band is drawn, there being no established therapeutic plasma range. There is no time until threshold either, but not for the reason given on the parent drug's page: the transit-chain engine can and does find a time until threshold directly from its matrix exponential (no exponential amplitudes are needed), but it only does so for the parent, enalapril; the metabolite's own columns are returned without a recovery calculation.
