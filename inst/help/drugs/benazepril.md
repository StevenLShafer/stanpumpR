### What is plotted is not the drug you dose

The curve is **benazeprilat**, the active diacid, not benazepril. The source is unusually direct about why: unchanged benazepril "has virtually no inhibitory activity on the converting enzyme at concentrations present in plasma", and the authors conclude that the parent's kinetics "are of minor importance for the pharmacokinetic characterization of the drug". So the dose you enter is milligrams of benazepril hydrochloride exactly as prescribed, and the concentration you see is its active metabolite.

Parameters come from Kaiser, Ackermann, Brechbühler and Dieterle (*Biopharm Drug Dispos* 1989;10:365–76), 62 healthy men given a single 10 mg oral dose. Two compartments, with apparent central volume 44.8 L, apparent peripheral volume 65.5 L, apparent clearance 7.92 L/h and apparent intercompartmental clearance 2.96 L/h.

### The parameters are derived, and the arithmetic is worth seeing

Kaiser analysed the data "by model independent methods" — trapezoidal areas, half-lives by regression — and fitted **no compartmental model at all**. The vector above is therefore *derived*, as [clevidipine's](help:drugs/clevidipine) is.

Normally that would be impossible. Two compartments need four independent numbers, and a non-compartmental paper usually prints three: a clearance, a volume and a terminal half-life. That is exactly why [esmolol and losartan](help:models/antihypertensives) could not be built from their sources. Benazeprilat escapes because Kaiser's Table 2 prints **four**:

| | |
|---|---|
| t½ α phase | 2.7 h |
| t½ β phase | 22.3 h |
| AUC(0–24 h) | 2649 (pmol/g)·h |
| AUC(0–∞) | 3185 (pmol/g)·h |

**Two areas, not one.** Their difference is the area under the terminal phase alone, and because that phase is a single exponential, multiplying it by β recovers the concentration at 24 hours — and with it the terminal intercept. That is the fourth number, and with it the whole vector follows.

Two checks say the arithmetic is right. The apparent clearance it implies equals dose over area exactly. And the renal fraction it implies is 20.0%, which is the paper's own separate statement that "about 20 per cent of the dose were excreted as benazeprilat" — a number the derivation was never given.

### What is firm and what is soft

The **area** under this curve is firm: it is dose over apparent clearance, and the clearance does not depend on how the area was split between the two phases.

The **shape** of the distribution phase is softer, and honesty requires saying by how much. The terminal area that closes the system is a difference of two measured numbers, 3185 − 2649, and their standard errors give that difference roughly ±25%. Propagated, the central volume carries about ±13%, the peripheral volume ±22% and the intercompartmental clearance ±37%. The derivation itself is as stable as clevidipine's per unit of input error; it is the input that is noisier.

Read those as a **floor** rather than a bound. The two areas rest on different subject counts — the 24-hour area on 60 subjects, the total on the 45 who still had samples at 48 and 72 hours — so they are not paired measurements on one group, and their difference carries a selection component that nothing printed lets you quantify.

### Why the peak is the least reliable part

The model predicts a peak of 156 ng/mL for a 10 mg dose against Kaiser's reported 195 ng/mL — **20% low**. The likely reason is not the model. Kaiser's figure is a *mean of individual peak concentrations* across 60 subjects whose times to peak ranged from 1 to 6 hours, and averaging peaks that happen at different times always exceeds the peak of the averaged curve. A 20% bias is unremarkable for that spread. It cannot be checked from the printed data, so it is stated rather than corrected.

The absorption rate constant is likewise an **effective** absorption-plus-conversion rate rather than a measured one: the real input to benazeprilat is benazepril's absorption convolved with its conversion, which no paper fits. It is set to put the peak at the measured 1.5 hours. The reassuring part is that the value this requires, 1.43 per hour, lands within a quarter of benazepril's *own* elimination rate constant of 1.16 per hour — the thing that should rate-limit the formation, and a number the fit was never told.

### An independent study agrees on the shape and not the scale

Sun and colleagues (*Eur J Clin Pharmacol* 1994;47:285–9) measured the same 10 mg dose in mass units: peak 257 ng/mL and area to 24 hours 1410 ng·h/mL, against Kaiser's 195 and 1050. The ratios are **1.32 and 1.34 — the same factor on both.** So two independent studies agree on the shape of this curve and disagree on its height by about a third.

That is recorded rather than reconciled, because rescaling a primary fit by another study's number would be fitting to a result instead of to data. The practical consequence: if you compare this curve with a published benazeprilat concentration, expect it to sit at the low end of the literature.

### The long tail is enzyme binding, linearised

Benazeprilat's 22.3-hour terminal phase carries only 17% of the area and runs at 1 to 3 per cent of peak concentrations. Physically it is binding to converting enzyme, not distribution into a tissue; a linear peripheral compartment is its linearisation at low concentration, the same reduction [lisinopril's page](help:drugs/lisinopril) describes and the reason captopril was not attempted at all.

One caution for anyone revising this. Kaiser's accumulation ratios **fall** with dose — 1.30 at 5 mg, 1.25 at 10 mg, 1.12 at 20 mg — which is weak evidence that the binding is already saturating across the therapeutic range. The model does not represent that, and should not be read as a claim of dose proportionality beyond 20 mg.

### No covariates at all

Kaiser fitted none: 62 healthy young men at a single dose level. In particular **there is no renal term**, even though benazeprilat is substantially cleared by the kidney, so a serum creatinine entered in the [Patient Profile](help:patient-profile) does not reach this drug. That is the source's limit rather than a statement about the drug: the study simply contains no renal-impairment data. Kaiser's companion papers report that clearance is reduced below a creatinine clearance of 30 mL/min, and nothing here represents that.

### Effect site and blood pressure

Neither is modelled, and [no antihypertensive in this library plots a blood pressure](help:models/antihypertensives). No band is drawn: there is no established therapeutic plasma range for benazeprilat.

Benazepril is also a **pregnancy contraindication** rather than merely an unstudied drug — the ACE inhibitors carry a boxed warning for fetal toxicity — so no pregnancy pharmacokinetic study exists and none should be expected.
