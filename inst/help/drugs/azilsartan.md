## Azilsartan

Azilsartan is the active moiety of **azilsartan medoxomil** (Edarbi), an
angiotensin II type 1 receptor blocker. The prodrug is hydrolysed during
absorption, so what circulates — and what this curve shows — is azilsartan.

The model is from **Tsai 2016**, a population analysis of 1481 subjects in an
eight-week phase 3 factorial trial of azilsartan medoxomil and chlorthalidone
in moderate to severe essential hypertension. Two compartments with
first-order absorption, fitted simultaneously with a one-compartment model of
the inactive metabolite M-II.

### Enter the dose as it is prescribed

A dose here is **milligrams of azilsartan medoxomil**, the way a prescription
is written — 40 mg or 80 mg. The model converts it to azilsartan by molecular
weight, 456.45 over 568.52, which is a 20% reduction.

That conversion is not stated anywhere in the paper. It was recovered from
the paper's own simulated exposures: a 20 mg dose is reported to give a
steady-state area under the curve of 10 919 ng·h/mL, and since area is dose
over clearance, the mass that entered the model must have been 16.05 mg —
which is 20 mg of the medoxomil converted to azilsartan. The agreement is
0.04%. The same check rules out the marketed potassium salt, whose ratio
would be 0.752 rather than 0.803.

Getting this wrong in either direction moves the whole curve by a fifth, so
it is worth knowing which mass you are typing.

### What the covariates do

Clearance falls with age and rises with weight:

> CL/F = (1.47 L/h − 0.0149 × [age − 58 years]) × (weight / 86 kg)^0.267

Across the 40 to 75 year range the source reports this as about a 20% change
in exposure either way, which it calls not clinically meaningful. Age was the
largest covariate effect found. Liver function tests and estimated glomerular
filtration rate were screened and neither survived once age and weight were
in the model.

### Trust the dosing interval, not the tail

Checked against the label rather than against the paper, this model puts the
peak at 3.0 hours — the top of the labelled 1.5 to 3 hours — but gives a
terminal half-life of about 26 hours where the label says 11 to 12.

That difference is the shape of the data, not an error. Sampling was sparse
and confined to a single dosing interval: troughs at weeks 4 and 6, one sample
1 to 4 hours after a dose and two more 4 to 12 hours after. Nothing observes
the drug beyond 24 hours, which is why the authors *fixed* the
intercompartmental clearance and peripheral volume instead of estimating
them — and those two fixed numbers are precisely what set the terminal slope.

So the curve is on firm ground across a dosing interval, which is where it
reproduces every exposure the paper simulated. Its tail is an extrapolation
from fixed parameters and is not evidence about azilsartan's terminal phase.

### What is deliberately not here

**No blood pressure.** The same paper fits a saturable model of ambulatory
systolic pressure, but it is driven by steady-state area under the curve
rather than by a concentration, its maximum effect differs between black and
non-black subjects, and its half-maximal exposure could not be estimated with
precision — the published interval runs from 558 to 6570 ng·h/mL. No
antihypertensive in this library plots a pressure; see
[the antihypertensives page](help:models/antihypertensives).

**No metabolite curve.** M-II is fitted in the same model, and its parameters
are recorded in the package's antihypertensive registry, but the paper calls
it virtually inactive and then pharmacologically inactive. Plotting an
inactive metabolite would suggest it means something at the effect site.

**No effect site and no therapeutic band.** There is no equilibration
constant in the source and no concentration target for an angiotensin
receptor blocker, so the plot is plasma only.

### Reference

Tsai MC, Wu J, Kupfer S, Vakilynejad M. Population pharmacokinetics and
exposure-response of a fixed-dose combination of azilsartan medoxomil and
chlorthalidone in patients with stage 2 hypertension. *J Clin Pharmacol*
2016;56:988–998.
