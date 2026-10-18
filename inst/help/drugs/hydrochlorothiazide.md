## Hydrochlorothiazide

Hydrochlorothiazide is the thiazide diuretic in most fixed-dose combination
antihypertensives. This curve is its **plasma concentration**: no blood
pressure, no sodium excretion and no urine output. The source fitted no
pharmacodynamic model of any kind, and no drug in this library plots a
pressure — see [the antihypertensives page](help:models/antihypertensives).

The model is from **Van Wart 2013**, a population meta-analysis of 26
published studies, spanning the whole range of renal function from normal
volunteers to patients in severe renal impairment, and oral doses from 10 to
500 mg. Two compartments, with every parameter apparent (`/F`): the published
model has no bioavailability term, so a dose is entered as prescribed and the
volumes and clearances absorb whatever fraction is absorbed.

Its unit of analysis is worth knowing, because it is unusual. Most of what it
pooled was **digitized mean curves** rather than individual data — 79% of the
profiles — and each such curve enters as a single "meta-individual" carrying
the number of people behind it. So the fit rests on 823 plasma concentrations
from 43 individuals or meta-individuals, not on 43 people: the between-subject
variability it reports is between *studies* as much as between patients, and
the mean curve it predicts is better determined than that variability
suggests.

### Renal function is the whole covariate model

Clearance splits into a renal part that depends on creatinine clearance and a
non-renal part that depends on nothing:

> CL/F = 18.3 × (CL<sub>CR</sub> / 120 mL/min)<sup>1.38</sup> + 2.44 L/h

Creatinine clearance is Cockcroft-Gault, the estimator the paper used, taken
from the **Serum creatinine** field in the Patient Profile. Across the
paper's own ladder the renal term falls almost sevenfold — and *total*
clearance, which is what sets the half-life, falls about fourfold, because
the non-renal 2.44 L/h does not move:

| Creatinine clearance | Renal CL/F | Terminal half-life |
|---|---|---|
| 120 mL/min | 18.3 L/h | 14.8 h |
| 80 mL/min | 10.5 L/h | — |
| 50 mL/min | 5.47 L/h | 29 h |
| 30 mL/min | 2.70 L/h | 42 h |

Two things follow. **Leaving the creatinine field blank means a normal
value** for the patient's age and sex, so it models a patient with no renal
impairment — and for this drug that is the single most consequential field on
the page. And **the non-renal clearance is a floor**: even at an anuric
creatinine clearance the drug still clears at 2.44 L/h, so the half-life
lengthens a long way but never without limit.

No body-size covariate was fitted — not weight, not body surface area — so
weight reaches the model only through Cockcroft-Gault, which contains it.

What that does to a daily dose is worth seeing rather than reading. On 25 mg
once daily, a patient with a normal creatinine reaches a peak about 16% above
the first dose's by the end of a week, with a trough near 27 ng/mL. Give the
same regimen at a serum creatinine of 2.5 mg/dL and the peak rises about 68%,
with a trough near 122 ng/mL — four and a half times higher. The peak moves
much less than the trough, which is the usual signature of impaired
elimination rather than of altered absorption.

### Why the absorption is drawn in pulses

Open the debug log or read the model's parameters and you will find each oral
dose given as several fractions a few minutes apart rather than as one. That
needs explaining, because hydrochlorothiazide is an ordinary
immediate-release tablet and the pulses describe nothing about how it is
made.

The source's oral input is a **Weibull** function:

> dA/dt = −(1 − e<sup>−(1.34·t)<sup>2.84</sup></sup>) · A

The shape parameter is 2.84, not 1. That means the depot's rate constant is
not a constant at all: it starts at zero and rises as the dose sits in the
gut, reaching about 1 per hour after an hour and a half. The paper says
plainly that this described the data substantially better than fixing the
shape at 1 or than adding a lag time.

This package's extravascular path has a constant `ka`, so it cannot
represent that shape. The library's rule for a published input shape it
lacks — set when MDMA's zero-order depot arrived — is to **discretise the
published shape and measure what the discretisation costs**, never to
substitute a different shape and hope. So the dose is given as **eight**
lagged fractions, all absorbed at **1.0 per hour**, which is not a fitted
number: it is the exact limit the Weibull's own rate constant approaches.

The result tracks a numerical integration of the exact system to **a third of
a percent** of the peak concentration at its worst point, and to six
thousandths of a percent at the peak itself, equally well at every renal
function the paper tabulates. The measured figures are in
`docs/antihypertensives.md`, and the package's tests check the curve against
that integration rather than against the fractions themselves.

Why eight and not fewer: the approximation deviates most **at the pulse
times**, where each fraction's absorption begins abruptly, and six fractions
cannot keep that deviation inside the accuracy the library requires of such a
stand-in. Seven only just manages it. Eight leaves real margin.

### Dosing, and a check against the label

Doses are entered in milligrams as prescribed, as a single dose or once
daily. The usual antihypertensive dose is 12.5 to 25 mg daily; 50 mg is used
for oedema.

A single 25 mg dose in a 70 kg 50-year-old man with a normal creatinine peaks
at about 147 ng/mL at 2.3 hours. The time to peak is worth noting because it
is an **independent** check: the labelled range is 1 to 5 hours, and nothing
in fitting this model used it. Reproducing a source's own numbers tests the
transcription; agreeing with a number from somewhere else is the first
evidence that the curve is right.

### What is deliberately not here

**No blood pressure, and no diuresis.** The paper is purely
pharmacokinetic.

**No urinary amount**, although the source does model one. Van Wart fits
plasma and urine together, and the paper's own conclusion is that the model
predicts the rate of urinary excretion of the drug from dosing history and
creatinine clearance. That is the drug appearing in urine, not urine volume,
and this program has nowhere to draw an excreted amount. Leaving it out moves
no concentration: the urine compartment accumulates what renal clearance has
already removed from plasma, so it is a sub-partition of the elimination in
the curve rather than a pathway competing with it.

**No effect site and no therapeutic band.** There is no equilibration
constant in the source and no therapeutic plasma range for a thiazide.

**No chlorthalidone comparison on this page.** The library has
[chlorthalidone](help:drugs/chlorthalidone) as its own drug from a different
paper, and the two models share no population, no assay and no analysis, so
their curves are not a head-to-head comparison of the two drugs.

### Reference

Van Wart SA, Shoaf SE, Mallikaarjun S, Mager DE. Population-based
meta-analysis of hydrochlorothiazide pharmacokinetics. *Biopharm Drug Dispos*
2013;34:527–539.
