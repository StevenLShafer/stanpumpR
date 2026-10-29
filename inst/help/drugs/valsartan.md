### The model

Valsartan's parameters come from Heo, Holford, Kim, Son and Park (*Br J Clin Pharmacol* 2016;82:1557–67), a population analysis of 624 concentrations from 48 healthy subjects given a single fixed-dose combination tablet of valsartan 160 mg with amlodipine 10 mg, sampled to 48 hours. Two compartments with first-order elimination: apparent clearance 6.18 L/h, apparent central volume 25.9 L, apparent intercompartmental clearance 2.01 L/h and apparent peripheral volume 17.4 L, all at a 70 kg standard individual.

Every parameter is **apparent**, divided by the oral bioavailability, because the analysis had oral data only. So no intravenous unit is offered.

### Absorption is zero order, and the authors tested that

The input is a constant rate over 4.39 hours rather than a first-order absorption, and that was a finding rather than a convention: the objective function was 7712 for the zero-order model against 7875 for the first-order one. The library has had a [zero-order oral release](help:models/antihypertensives) since carvedilol, whose source rejected first-order absorption for the same kind of reason, so each oral dose here is rewritten into the constant-rate rows that deliver it over those 4.39 hours. There is no absorption rate constant and no separate bioavailability — both are inside the apparent scale.

One consequence is worth knowing before reading the curve. **The peak is at the end of the release**, at exactly 4.39 hours, because a constant-rate input into a linear system peaks when the input stops. The label puts the peak at 2 to 4 hours, a little earlier, and the authors say themselves that the model "somewhat underpredicted" the median peak concentration. So this curve is a fair description of valsartan's exposure and a slightly blunt one of its peak.

### Body weight is the only covariate, and the authors looked for others

The paper screened age, serum creatinine, smoking and drinking status by stepwise covariate modelling and reports that apart from body weight, none was significant. Size is theory-based allometry at a 70 kg standard — weight to the power 0.75 on the clearances, weight itself on the volumes — which is also this library's own scaling, so the published model is reproduced exactly. The release duration is not scaled: the paper's size equations name clearance and volume only.

**There is therefore no renal term here**, and that is worth saying plainly for a drug given to patients whose kidneys are often the reason they are taking it. The population was 48 healthy young adults with a mean age of 29 and a mean serum creatinine of 0.81 mg/dL. It contains no information about renal impairment at all, so a creatinine entered in the [Patient Profile](help:patient-profile) does not reach this drug — not because renal function is thought irrelevant, but because this source cannot say anything about it.

### How the curve compares with the label

At the 70 kg standard a 160 mg dose gives a peak of 3.4 µg/mL and an area under the curve of 25.9 µg·h/mL, with an apparent terminal half-life of about 8 hours. Published single-dose values for 160 mg in healthy volunteers sit in the region of 3 to 4 µg/mL and 20 to 30 µg·h/mL, with a terminal half-life of 6 to 9 hours — so the exposure is in the right place even though the peak arrives late.

### Why not the newer paper

Yang and colleagues (*Clin Pharmacol Drug Dev* 2022;11:1181) fitted valsartan in healthy Chinese subjects more recently and in a larger population, and this library does **not** use it. The reason is specific. Simulating the published model gives a time to peak of 7.5 hours and a peak of 0.99 µg/mL against roughly 2 to 2.5 hours and 2.8 µg/mL in the paper's own visual predictive check; the area under the curve does match, so the clearance is sound and the shape is not. Moving the decimal point in the absorption rate constant fixes the time to peak exactly and still leaves the peak 1.6-fold low, so at least two printed parameters are irreconcilable and nothing in the paper says which. That was checked with the closed-form solution and again with an independent numerical integration. The paper also studied sacubitril/valsartan rather than conventional valsartan, prints a peripheral volume of 0.51 L with a 26-fold confidence interval on the distribution clearance, and describes a body-mass-index allometry that its parameter table does not contain. The record is in the registry as requiring a parameter fit.

### Effect site and blood pressure

The source *did* fit a blood-pressure model for valsartan — an effect compartment with a maximum inhibition of 0.164 — together with an interaction term for the combination with amlodipine. [This library still does not plot a blood pressure](help:models/antihypertensives), for reasons that are about the class rather than about this paper. The relation is recorded in the antihypertensive registry as an endpoint record, where it can be evaluated against a concentration you supply and returns a *change* rather than an absolute pressure. No band is drawn here: there is no established therapeutic plasma range for valsartan.

### Where to be careful

This is a **single-dose study in healthy young volunteers**, so nothing in it constrains the accumulation on chronic daily dosing, and the between-subject variability on the central volume was large (0.519 on the scale the paper reports, the square root of the variance, against 0.373 for clearance).

Valsartan is also a **pregnancy contraindication**, not merely an unstudied drug: the angiotensin receptor blockers carry a boxed warning for fetal toxicity. The registry records that there are no pregnancy pharmacokinetic studies and none should be expected.
