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

Hui gave the oral dose only to his *impaired* patients, so the absorption of a patient with normal renal function has to come from elsewhere. It comes from Singhvi 1988 in healthy volunteers, through the Murdoch and McTavish review (*Drugs* 1992;43:123–40): about a third of a 10 mg dose absorbed (32 to 36%), peak 99 to 140 ng/mL, time to peak 2.8 to 3.1 hours, area to 24 hours 1035 to 1132 ng·h/mL, and a fosinoprilat bioavailability of 25 to 29%. Over 10 to 640 mg the time to peak ran 2.4 to 4.2 hours; the label says it is dose-independent at "approximately 3 hours". Food slows the rate and not the extent. No lag was fitted anywhere, and none is used.

An absorption rate of **0.2987 per hour** puts the model's peak at 2.95 hours, the midpoint of that range. A bioavailability of **0.26** of the fosinoprilat-equivalent dose then gives a peak of 117.8 ng/mL and a 24-hour area of 1126.9 ng·h/mL — both inside the measured ranges, and inside the reported 25 to 29% as well.

Two things are worth knowing about that 0.26.

**Hui's own patients corroborate it, and they are the reason one absorption model serves everyone.** His crossover gave fosinoprilat bioavailability as 22.5, 22.0 and 27.9% in the mild, moderate and severe groups — a mean of 24.1%, 8% below the value used, from the same patients, assay and analysis as the disposition. That those bracket the healthy 25 to 29% is the evidence that renal impairment leaves the *extent* of absorption alone, which is what you would expect if the ester is hydrolysed in the gut before absorption even begins.

**The agreement with the reported percentage is partly a coincidence of two offsetting discrepancies**, and saying so is more useful than claiming a match. Singhvi's own intravenous and oral areas imply an availability of 33 to 36% against *his* clearance, not 25 to 29% — and his clearance was about 40% above Hui's. See the next section.

### This curve is an older patient's, and runs high

Hui's "normal renal function" group were hypertensive patients averaging 61 years, and their clearance of 26.5 mL/min sits at the **bottom** of the label's healthy range of 26 to 39 mL/min. Singhvi's young healthy volunteers were at the top: his intravenous area implies about 37 mL/min — derived here, by extrapolating his truncated 24-hour area with this model's own 94% share.

So for the same intravenous dose this model gives about 40% more exposure than Singhvi measured — a 5-minute concentration of 2064 ng/mL against his 1556. Neither is wrong; both are inside the label's quoted healthy range, and only clearances are being compared here, which differ by exactly the factor the exposures do. Nobody can give that intravenous dose anyway, since only the prodrug is marketed, and orally the derived bioavailability absorbs the difference.

The terminal half-life needs no such caveat. The model's 12.4 hours matches the label's "approximately 12 hours" in healthy subjects and its effective 11.5 hours for accumulation in hypertensive patients.

### An independent check on the volume

Murdoch reports Singhvi's steady-state volume of distribution as 9.8 to 10.6 L after the same 7.5 mg intravenous dose. Hui's 10.2 L — which this model reproduces exactly, being an input — sits in the middle of it. A different laboratory and a different population agree on the volume while disagreeing on the clearance.

### What the source is, and what it is not

Four to six patients per group, aged 54 to 68, mostly hypertensive, weighing 77 to 81 kg. Dispersion is printed as standard **error**, not standard deviation. The assay is radiolabel — thin-layer radiochromatography, sensitivity 2 ng/mL. One patient was withdrawn and four excluded from some calculations, one of them for a poor fit.

Table IV's parameters are **means of individual fits**, so they need not form one consistent model, and they do not quite: the area the derived vector implies is 4717 ng·h/mL against Table IV's own 4971, about 5% apart. The right repair would be to refit the individual data, which is not published, so the inconsistency is stated rather than removed.

Protein binding is reported as 97.4 to 98.1% (Hui), 95% (Singhvi) or "approximately 99.4%" (the label), and the curve is **total** fosinoprilat, not free.

### What is not modelled

* **The absorption rate in renal impairment.** Hui's impaired patients peaked at 4.0 to 4.2 hours with mean absorption times of 7.1, 11.1 and 12.3 hours, against the healthy 2.8 to 3.1. One absorption model serves every patient here: the *extent* is what Hui shows unchanged, and the rate is not the same claim. The mean absorption time is also confounded, since the residence time after the intravenous dose rose in the same patients.
* **p-hydroxyfosinoprilat.** After an oral dose the label puts active fosinoprilat at 75% of plasma radioactivity, its inactive glucuronide at 20 to 30% and this p-hydroxy metabolite at 1 to 5% — and "in rats, the p-hydroxy metabolite of fosinoprilat is as potent an inhibitor of ACE as fosinoprilat". A minor, equipotent, unmodelled active metabolite.
* **Hepatic impairment**, where the label puts apparent total clearance at about half of normal in cirrhosis. The Patient Profile has no hepatic covariate.
* **Dialysis.** Fosinopril is poorly dialysed (2% and 7% of urea clearance by haemodialysis and peritoneal dialysis), and dialysis patients had clearance halved. The plateau happens to land near the right place; nothing here models dialysis.
* **Children**, beyond ordinary size scaling — which is what the label supports: 20 patients aged 6 to 16 with a glomerular filtration rate of at least 25 had areas and peaks matching adults at the same dose per kilogram, with a terminal half-life of 11 to 13 hours.

### Effect site, band and blood pressure

None of the three. There is no published equilibration constant, so the drug is plasma only; there is no established therapeutic plasma range for fosinoprilat, which is dosed to blood pressure and is not listed in Hart 2026's Table 5, so **no band is drawn**; and [no antihypertensive in this library plots a blood pressure](help:models/antihypertensives).

Fosinopril is also a **pregnancy contraindication** rather than merely an unstudied drug — the ACE inhibitors carry a boxed warning for fetal toxicity, and fosinoprilat crosses the placenta in animals — so no pregnancy pharmacokinetic study exists and none should be expected.
