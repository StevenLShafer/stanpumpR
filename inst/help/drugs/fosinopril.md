### What is plotted is not the drug you dose

The curve is **fosinoprilat**, the active diacid. Fosinopril is an ester prodrug with no meaningful converting-enzyme activity of its own, and only the prodrug is marketed — so what you prescribe and what the patient swallows is fosinopril, and what this page draws is what it becomes.

There is deliberately only **one drug here**, not two. That is the opposite of the [enalapril](help:drugs/enalapril) arrangement, where the library carries `enalaprilat` as a drug of its own because Faisal 2019 fitted parent and metabolite together from one dataset. Fosinoprilat does not exist as a medicine at all — only as a research dose, which is exactly how Hui gave it — so there is nothing to dose separately.

The model takes the conversion to be **complete and instantaneous, in the gut, before absorption**. That is not a convenience: fosinopril is "rapidly converted to its active diacid, fosinoprilat, by esterases in the gastrointestinal mucosa and liver", its primary site of absorption is the proximal small intestine, and less than 1% of plasma radioactivity after an oral dose is intact prodrug. Enter the dose as prescribed, in milligrams of fosinopril sodium; the model converts it to its molar equivalent of fosinoprilat internally.

### The dose basis is read, not calculated

Hui's Methods state the intravenous dose as 7.5 mg of fosinoprilat, "equimolar to 10 mg fosinopril sodium". So **0.75 mg of fosinoprilat per mg of fosinopril sodium**, taken from the source itself rather than computed from molecular weights.

That choice matters a little. The disposition below was fitted to that 7.5 mg dose, and the paper's oral bioavailability percentages are quoted against the 10 mg fosinopril dose, so 0.75 keeps the whole chain internally consistent. Exact molar arithmetic differs by under 1%, and the label prints a molecular weight for the sodium salt (585.65) but none for the diacid, so there would be nothing here to check it against.

### Six printed numbers, and why they are enough

This drug was in the [antihypertensive registry](help:models/antihypertensives) as *not pursued*, partly on an identifiability failure. That reasoning was about a different paper — Gehr 1993, which prints a clearance, a steady-state volume, a mean residence time and a terminal half-life, where the residence time is the volume over the clearance, so three independent numbers and not the four two compartments need. It was never applied to Hui, which is a different kind of paper altogether.

Hui's Table IV prints **six** numbers per group for a three-compartment fit, which is exactly the six a three-compartment model has:

| | Normal renal function |
|---|---|
| Initial volume of distribution | 3.1 L |
| Total body clearance | 26.5 mL/min |
| Steady-state volume of distribution | 10.2 L |
| t½ α, β, γ | 0.2, 2.1, 12.4 h |

And the solve is **linear**, because the Methods define the two quantities that make it so: the area is A/α + B/β + C/γ, and the initial volume is dose over A + B + C. With the exponents fixed by the half-lives, the three intercepts satisfy three linear equations — their sum, their first moment (the area, which is dose over clearance) and their second moment (the residence time times the area, and the residence time is the steady-state volume over the clearance). The result is

* A, B, C = **1299.8, 1055.6, 63.9 ng/mL** at the 7500 mcg dose, all positive, and
* **V₁ 3.10, V₂ 2.43, V₃ 4.67 L; CL 26.5, Q₂ 71.7, Q₃ 5.47 mL/min**, every micro rate constant positive.

### The long tail is enzyme binding, linearised

The third phase is thin but very long. Its intercept is only 2.6% of the three added together, yet because it decays so slowly it carries about **a quarter of the whole area** — which is why a drug with a 12.4-hour half-life is given once a day.

Physically that phase is binding to converting enzyme rather than distribution into a tissue, and a linear third compartment is its linearisation at low concentration — the same reduction [benazepril's](help:drugs/benazepril) and [lisinopril's](help:drugs/lisinopril) pages describe, and the reason captopril was never attempted.

### How firm is it? All 729 corners hold

Esmolol's decomposition was rejected for *conditioning* rather than identifiability, so this one was stress-tested the same way: the solve was repeated at every one of the 3⁶ = 729 corners of Hui's printed ± SEM box, including a distribution half-life at 0.1 and 0.3 hours where its standard error is half the estimate itself.

**Every one of the 729 gives a physically admissible model.** None is refused. Across that box:

| | Range | Span |
|---|---|---|
| V₁ | 2.70–3.50 L | 1.30× |
| CL | 23.5–29.5 mL/min | 1.26× |
| V₂ | 1.04–4.18 L | 4.03× |
| Q₂ | 29.0–193.6 mL/min | 6.66× |
| V₃ | 2.78–6.57 L | 2.36× |
| Q₃ | 2.74–10.45 mL/min | 3.81× |

V₁ and the clearance move only as much as their own standard errors, because they are *inputs*. The **derived** distribution parameters are the soft ones, Q₂ worst because it is driven almost entirely by that distribution half-life. V₂ + V₃ is pinned, being the steady-state volume minus V₁. So the area under this curve and the shape of its first minutes are firm, the split of the middle phase is not — and for a drug absorbed over three hours, almost nothing you will look at depends on that split.

### Renal function: a plateau, not a proportion

Hui's four groups, by creatinine clearance in mL/min/1.73 m² against total clearance in mL/min:

| Creatinine clearance | 98.8 | 59.6 | 31.8 | 16.3 |
|---|---|---|---|---|
| Fosinoprilat clearance | 26.5 | 14.6 | 13.7 | 18.2 |

Clearance **falls 42% from normal to impaired** — Hui's own analysis of variance calls that significant at p < 0.001 — and is then **flat across impairment**. Within his impaired patients there was "no relationship between baseline creatinine clearance (11 to 72 ml/min/1.73 m²) and body clearance", and the *severe* group's 18.2 is the highest of the three.

The reason is in the same paper's Figure 4: hepatic clearance accounts for 63%, 79% and 85% of body clearance in the mild, moderate and severe groups. As filtration falls, the hepatobiliary route carries progressively more of a smaller total. So the covariate must level off, and clearance must **not** be scaled in proportion to creatinine clearance.

What the model does is a two-plateau ramp on Cockcroft-Gault creatinine clearance indexed to 1.73 m², the unit Hui reports:

* **26.5 mL/min** at a creatinine clearance of **80 and above** — the US label's own boundary for normal renal function, which defines mild to severe renal insufficiency as 10 to 80 mL/min/1.73 m²;
* **15.5 mL/min** at **59.6 and below** — the unweighted mean of Hui's three impaired groups, at his mild group's own creatinine clearance;
* linear across the interval between, which is the one interval neither source measures.

Every number in that is read from a source. The mean is unweighted because Table IV prints no per-cell subject count, only "n = 4 to 6 per group".

### The covariate moves nothing but elimination — and that buys a check

Hui's initial volumes (3.1, 3.3, 3.4, 4.2 L) and steady-state volumes (10.2, 8.8, 8.9, 13.0 L) show no trend worth modelling, so the volumes and both intercompartmental clearances are held at the normal group's and only the elimination clearance carries the covariate.

The model is therefore **never told the impaired half-lives**. At the impaired plateau it *predicts* a terminal half-life of **14.7 hours** against Hui's measured 14.0, 14.1 and 16.9 — a mean of 15.0, so within 2% — and a second phase of 2.9 hours against his 3.1, 3.9, 3.1. A clearance-only covariate on a fixed structure reproduces half-lives it was not fitted to.

The two are worth different amounts, and Hui's own statistics are what separate them. He found the distribution and elimination (β) half-lives **significantly** lower in normal renal function, so the second-phase match is the model reproducing an established difference — within 15%, on the phase that carries 68% of the area. He did **not** report the terminal half-life as significantly different: Table IV marks it with no significance symbol and the text's list of what differed leaves it out. So the 14.7 against 15.0 is a match to a trend rather than to a demonstrated effect. Both are checks the model was not given; only one is a check against something the source itself calls real.

### Where the label disagrees, and how far

You should know this before trusting the renal covariate. The US label says, in words, the opposite:

> In patients with mild to severe renal insufficiency (creatinine clearance 10 to 80 mL/min/1.73 m²), the clearance of fosinoprilat does not differ appreciably from normal, because of the large contribution of hepatobiliary elimination

and that only below a creatinine clearance of 10 is total clearance "approximately one-half of that in patients with normal renal function". Hui is the study that sentence rests on.

The two can be reconciled, and the label's own arithmetic is what does it: it puts healthy body clearance at "between 26 and 39 mL/min", and half of that is about Hui's 15.5. Read the sentence as flatness *across* 10 to 80 rather than equality *with* normal, and the label and Hui agree — which is also how the label's two statements become consistent with each other. That reading is an inference, and neither document states it. Steven L. Shafer settled the question on 2026-10-29: **keep Hui's halving**, which is what this model does. Where a label's prose and the study it rests on disagree, the study's *numbers* are what a simulation can be built on, and the prose is what you have to be warned about — "does not differ appreciably from normal" is a clinical judgment written to guide dosing, where Hui's 42% at p < 0.001 is a measurement.

There is a more general reason too. A label is published on the best science available when the drug is approved, and after that the science advances and the label does not: asking to change one invites fresh regulatory scrutiny of matters a company believed settled, so there is a standing disincentive to revise. **A revision date is not a review date.** This label reads "Revised: 10/2021", and its renal-impairment paragraph is still the 1991-era summary of the very paper this model is built from.

So if you compare this curve with the label, expect the label to predict no change down to a creatinine clearance of 10, and expect this curve to follow the measurements instead.

### A blank serum creatinine is not a normal clearance

Leaving the [Patient Profile's](help:patient-profile) Serum creatinine field empty means the *assumed normal value for the patient's age and sex* — 0.9 mg/dL for a man, 0.8 for a woman, from 18 years. It does **not** mean a normal creatinine clearance, because Cockcroft-Gault contains an age term: a 70-year-old with a blank field lands at an indexed clearance near 72, on the ramp, and so about 16% below the normal-function clearance.

Whether that is right is precisely what Hui cannot resolve and the label denies — and the label separately reports no pharmacokinetic difference in men aged 65 to 74 with normal renal and hepatic function. Enter a measured creatinine if you have one.

### Absorption comes from a second study, because Hui's normal group got no capsule

Hui gave the oral dose only to his *impaired* patients, so the absorption of a patient with normal renal function has to come from elsewhere. It comes from **Singhvi and colleagues** (*Br J Clin Pharmacol* 1988;25:9–15): nine healthy men aged 20 to 34, in a three-way crossover of intravenous fosinoprilat 7.5 mg and two oral 10 mg doses of fosinopril sodium, one a capsule and one a solution. Oral absorption averaged 32% for the solution and 36% for the capsule, and the **bioavailability of fosinoprilat averaged 25% and 29%**. Food slows the rate and not the extent. No lag was fitted anywhere, and none is used.

An absorption rate of **0.2987 per hour** puts the model's peak at 2.95 hours, the midpoint of fosinoprilat's times to peak in Singhvi's study, 2.8 hours after the solution and 3.1 hours after the capsule. Bioavailability is the capsule's **0.29**, read — the capsule because the marketed product is a tablet, a solid dosage form, and because Hui dosed a capsule too. For a patient with Hui's clearance the model then gives a peak of 131 ng/mL and a 24-hour area of 1257 ng·h/mL.

### Why that number is read and not fitted

An earlier version of this model *derived* a bioavailability of 0.26, by making the curve reproduce two figures quoted for Singhvi's oral dose. That was wrong in two ways, and both are worth knowing if you ever check this page against the literature.

**The two figures are not fosinoprilat.** A peak of 99 to 140 mcg/L and a 24-hour area of 1035 to 1132 mcg/L·h are quoted in the Murdoch review as "plasma fosinoprilat", but Singhvi's own table reports each figure for both total radioactivity and fosinoprilat, and it shows that the **area is total radioactivity** and the **peak range spans the two**: 99 is fosinoprilat after the solution, 140 total radioactivity after the capsule. Only about **75%** of the plasma radioactivity after an oral dose is the active diacid — the rest is an inactive β-glucuronide (15 to 20%) and a monohydroxy analogue (about 5%). Singhvi's numbers agree: 75% of that area is 776 to 849 ng·h/mL, which falls inside the **762 to 884** that his own bioavailability (25 to 29%) and his own clearance (39 mL/min) imply for the diacid over the same 24 hours. (His bioavailability and clearance give 801 to 929 for the *total* area — his table prints the diacid's as 788 and 920 — of which this model's oral curve, run at his clearance, leaves 95.1% inside 24 hours; an earlier version of this page truncated with Hui's 92% share instead, which belongs to a slower patient.) For a radiolabel study this is the mislabelling to expect, because the paper reports both quantities.

**And bioavailability cannot be fitted that way at all.** It is clearance-independent by construction: Singhvi's is a within-subject crossover, so his subjects' clearance cancels out of it. Making the model reproduce *his* oral exposure while it runs on *Hui's* clearance therefore counts the difference between the two populations twice — his area is small because he cleared the drug quickly, and charging that to a smaller bioavailability would make every other patient's curve wrong. The quantity to read is the bioavailability; the quantity to let the model compute is the exposure.

**Hui's own patients are the corroboration, and they run a little lower.** His crossover gave 22.5, 22.0 and 27.9% in the mild, moderate and severe groups, measured against the same patients' own intravenous doses with the same assay as the disposition. Those sit below the healthy capsule's 29% — the mild group by 2.4 of its own standard errors — and he reports no relationship between bioavailability and renal function. So the fair reading is that impairment leaves the *extent* of absorption broadly alone, which is what you would expect if the ester is hydrolysed in the gut before absorption begins, while impaired patients may run somewhat below the value this model uses.

### This curve is an older patient's, and runs high

Hui's "normal renal function" group were hypertensive patients averaging 61 years, and their clearance of 26.5 mL/min sits at the **bottom** of the label's healthy range of 26 to 39 mL/min. Singhvi's nine young men were at the **top** of it: he reports clearance 39 mL/min, renal clearance 17, a steady-state volume of 10 L and protein binding about 95%.

So the two sources differ **1.47-fold** on the one parameter that measures a physiological process, and agree on the volume. That factor is the whole of the difference between them, and it shows up where it should: this model predicts an oral area of 1368 ng·h/mL against the 929 that Singhvi's own bioavailability and clearance give for his capsule.

Neither is wrong — both clearances are inside the range the label itself quotes for healthy subjects — and the model is deliberately the older patient, because an oral antihypertensive is prescribed to people more like Hui's hypertensive 61-year-olds than like Singhvi's volunteers.

But **1.47 is the ratio of the areas, not of the curve**, and an earlier version of this page said the whole curve would sit 1.5-fold high. A clearance change does not scale a concentration-time curve. Running this model at Singhvi's clearance instead of Hui's gives:

| | ratio, Hui to Singhvi |
|---|---|
| AUC(0–∞) | ×1.47 |
| AUC(0–24 h) | ×1.42 |
| Peak concentration | ×1.21 |
| Time to peak | 2.95 h → 2.43 h |

The peak is reached before much drug has been eliminated, so it feels the clearance far less than the area does. **Expect about 1.5-fold on an area and about 1.2-fold on a peak**, and do not read either as an error in either study.

The terminal half-life needs no such caveat. The model's 12.4 hours matches the label's "approximately 12 hours" in healthy subjects and its effective 11.5 hours for accumulation in hypertensive patients.

### An independent check on the volume

Singhvi reports a steady-state volume of 10 L after the same 7.5 mg intravenous dose (9.8 ± 0.7 L in his table; the Murdoch review's "9.8 to 10.6" has an upper figure his paper does not print). Hui's 10.2 L — which this model reproduces exactly, being an input — is the same number. A different laboratory and a different population agree on the volume while differing 1.47-fold on the clearance, which is the **reverse** of the usual pattern: here it is the mathematical construct that agrees and the physiological parameter that does not.

### What the source is, and what it is not

Four to six patients per group, aged 54 to 68, mostly hypertensive, weighing 77 to 81 kg. Dispersion is printed as standard **error**, not standard deviation. The assay is radiolabel — thin-layer radiochromatography, sensitivity 2 ng/mL. One patient was withdrawn and four excluded from some calculations, one of them for a poor fit.

Table IV's parameters are **means of individual fits**, so they need not form one consistent model, and they do not quite: the area the derived vector implies is 4717 ng·h/mL against Table IV's own 4971, about 5% apart. The right repair would be to refit the individual data, which is not published, so the inconsistency is stated rather than removed.

Protein binding is reported as 97.4 to 98.1% (Hui), 95% (Singhvi) or "approximately 99.4%" (the label), and the curve is **total** fosinoprilat, not free.

### What is not modelled

* **The absorption rate in renal impairment — for which Hui turns out to contain no evidence.** An earlier version of this page said his impaired patients absorbed more slowly, citing a peak at 4.0 to 4.2 hours against the healthy 2.8 to 3.1. That was wrong twice.

  A later peak is not a slower input: **anything that lowers elimination moves the peak right.** Holding the absorption rate at the healthy value and lowering only the clearance to the impaired plateau, this model *predicts* a peak at 3.7 to 3.9 hours — essentially all of the observed shift. Closing the last fraction of an hour would need a rate 85 to 92% of healthy, well inside the scatter of a time to peak reported as 4.0 ± 0.0 to 4.2 ± 0.5 hours in groups of four to six.

  And slower *than what?* Hui's normal-renal-function group received the intravenous dose only, so his paper has no healthy oral arm. The comparison was with a different study, across a 1.47-fold clearance difference, a different population and a different laboratory.

  The one quantity that *would* be evidence disagrees with his own peak times. Mean absorption time is the oral residence time minus the intravenous one, taken within each group from its own intravenous arm, so it is clearance-independent — for a first-order input it is exactly the reciprocal of the absorption rate. His values of 7.1, 11.1 and 12.3 hours imply peaks at 5.7 to 7.6 hours, and he observed 4.0 to 4.2. The two descriptors disagree by nearly two-fold, and the mean absorption time is the weaker, because **a difference between two numbers is a fraught measurement**: the variance of a difference is the sum of the variances, so the difference between two means is easily dwarfed by their scatter.

  Worked through, that finishes the question. Hui did the arithmetic *correctly* — his printed standard errors (1.3, 3.0, 4.4 hours) are smaller than independence would give (3.05, 3.59, 7.32), the signature of computing the difference per subject rather than from the two group means. But even so, **the trend his text claims is not in his numbers**: "longer in patients with moderate or severe impairment" tests out at 1.2, 0.2 and 1.1 standard errors for the three comparisons. And the model's own mean absorption time is not a measurement at all — it is the reciprocal of a rate set to a midpoint quoted in a review, so it carries no standard error and belongs in no significance test.

  So one absorption rate serves every patient here not as a simplification but because **there is no supported absorption-rate finding in the paper at all** — not from the peak times, not from the mean absorption times against healthy, and not from them across degrees of impairment.

* **p-hydroxyfosinoprilat.** After an oral dose the label puts active fosinoprilat at 75% of plasma radioactivity, its inactive glucuronide at 20 to 30% and this p-hydroxy metabolite at 1 to 5% — and "in rats, the p-hydroxy metabolite of fosinoprilat is as potent an inhibitor of ACE as fosinoprilat". A minor, equipotent, unmodelled active metabolite.
* **Hepatic impairment**, where the label puts apparent total clearance at about half of normal in cirrhosis. The Patient Profile has no hepatic covariate.
* **Dialysis.** Fosinopril is poorly dialysed (2% and 7% of urea clearance by haemodialysis and peritoneal dialysis), and dialysis patients had clearance halved. The plateau happens to land near the right place; nothing here models dialysis.
* **Children**, beyond ordinary size scaling — which is what the label supports: 20 patients aged 6 to 16 with a glomerular filtration rate of at least 25 had areas and peaks matching adults at the same dose per kilogram, with a terminal half-life of 11 to 13 hours.

### Effect site, band and blood pressure

None of the three. There is no published equilibration constant, so the drug is plasma only; there is no established therapeutic plasma range for fosinoprilat, which is dosed to blood pressure and is not listed in Hart 2026's Table 5, so **no band is drawn**; and [no antihypertensive in this library plots a blood pressure](help:models/antihypertensives).
