### Two curves, and the second one is the drug that works

Enalapril is an inactive prodrug. The moiety that inhibits converting enzyme is [enalaprilat](help:drugs/enalaprilat), formed from it in the liver, and this library plots **both**: enalapril on its own row and the enalaprilat it produces on another. So dosing enalapril adds two panels, and the one that matters clinically is the second.

That is unusual among the ACE inhibitors here. [Benazepril](help:drugs/benazepril) is plotted as its diacid alone, because its source states the parent has "virtually no inhibitory activity" and calls the parent's kinetics "of minor importance". Enalapril's source took the opposite view and measured both moieties deliberately, in serum **and** in urine, so both curves rest on data rather than on an assumption.

Parameters come from Faisal, Cawello, Burckhardt, de Hoon and Laer (*Front Pediatr* 2019;7:281), 2208 serum and urine concentrations from 24 healthy adults given a single 10 mg oral dose of enalapril maleate, modelled in NONMEM and validated by visual predictive check, bootstrap and sampling-importance-resampling.

### Fitting urine with serum is what makes this model usable

Almost every oral antihypertensive model in this library is **apparent**: its volumes and clearances are divided by a bioavailability nobody measured, because oral data alone cannot separate the two. [Valsartan's](help:drugs/valsartan) are, [lisinopril's](help:drugs/lisinopril) are, and so are the whole class's.

This one is not. Enalapril and enalaprilat are both cleared renally and by nothing else, so measuring what appears in the urine closes the mass balance and the bioavailability becomes a parameter of its own. It came out at **0.606**, against the 60% the literature had long reported from other designs — a number the fit was not told. Everything else is therefore absolute: a real central volume of 51.1 L, a real clearance.

### The absorption needed a new engine

Enalapril's absorption is not first order. A lag-time model failed to describe it, and what worked was a chain of **eight identical transit compartments** feeding a single central compartment. Enalaprilat's formation needed **two more**.

A chain like that is why this drug took so long to arrive. Every other curve in this library is built from sums of exponentials, and the coefficients of those sums all divide by the difference between two rate constants. Eight identical compartments share one rate constant eight times over, so those differences are zero, and nudging them apart — which the library does for accidental ties — would mean dividing by a nudge seven times and then subtracting enormous nearly equal numbers from each other. That is the same arithmetic hazard that kept [esmolol out of a decomposition](help:models/antihypertensives) of its own.

The system is still perfectly linear, so it needs no approximation — only a method that never forms those differences. That method is the matrix exponential, and this drug is why the library now has one. The curve you see is exact to rounding, in the same sense the closed-form curves are, and not accurate merely to some step size.

### Where the two absorption constants each act, and the one inconsistency

The source prints a mean transit time of 0.558 h with eight compartments, and separately an absorption rate constant of 6.010 per hour. It is explicit about what each does: the rate **through** the transits comes from its formula (8+1)/0.558 = 16.13 per hour, and the 6.010 is "the transfer rate of the drug from the **last** transit into enalapril central compartment". Both are used exactly as defined.

One derived number cannot survive both statements. The formula assumes nine steps all at the same rate, while the structure has seven at 16.13 and one at 6.01, so the model's true mean input time is 0.60 h rather than the printed 0.558 h. That is an inconsistency in the source's shorthand, not in its parameters.

It was worth checking rather than assuming, so three other readings were built and integrated numerically. **All four reproduce the source's own observations** — it reports the parent peaking in "almost 1 h" and the metabolite "in around 3 to 4 h", and the four readings give 0.82 to 0.95 h and 3.20 to 3.32 h, with peak concentrations varying under 6%. The reading implemented is the one the text states, and that is the only reason for it. It is **not** the closest of the four to the reported peak — the nine-compartment reading is nearer, 0.95 h against 0.88 h — which is worth knowing, because it means this curve was not chosen by fitting it to the figure the paper reports. An ambiguous parameterisation is only a blocker when the readings disagree with the paper's own figures, which is what separates this case from the [valsartan model this library rejected](help:drugs/valsartan).

### What the curve looks like

At the source's reference individual a 10 mg tablet gives enalapril a peak near 76 ng/mL at about 0.9 hours, falling below the study's assay limit well within 10 hours — the source says the drug "is eliminated in almost 10 h from the serum". Enalaprilat peaks near 56 ng/mL at about 3.3 hours and leaves far more slowly, with a terminal half-life around 18 hours. About **69%** of what is absorbed becomes enalaprilat, which is the ratio of the two elimination paths the source fitted, 0.688 against 0.305 per hour.

The practical reading: the parent is a transient, and the metabolite is the exposure. On twice-daily dosing enalaprilat accumulates and enalapril does not.

### An independent study agrees on the metabolite peak, and that settles the dose basis

Arafat and colleagues (*J Clin Pharm Ther* 2005;30:319-28) gave two brands of 10 mg enalapril maleate to healthy volunteers and measured both moieties with **specific enzyme immunoassays**, rather than the ACE-inhibition assay most earlier work used. Their enalaprilat peaks were 54.8 and 57.2 ng/mL. This model predicts **55.9** — between the two, within 3% of each, in a study that was no part of Faisal's fit.

That agreement does more than reassure. It answers the question the next section has to leave open: whether the fitted bioavailability of 0.606 already contains the maleate-to-base mass fraction of 0.764. Getting that wrong in either direction would move the whole curve by a factor of 1.308, and **both alternatives land outside Arafat's measured range.** So an ambiguity the source itself cannot settle is settled by a study that never entered it.

The two studies do **disagree about the tail.** Arafat's metabolite area was 267 and 256 ng·h/mL against this model's 495 — a factor of about 1.9. They report the reason as a finding of their own: enalaprilat concentrations at 12 to 24 hours were "lower than those reported in the literature". So it is their terminal phase that departs from the published picture, not Faisal's. It is recorded rather than reconciled, for the same reason [benazepril](help:drugs/benazepril) keeps its own scale against an independent study that read a third higher: rescaling a primary fit to another study's number is fitting to a result instead of to data.

The **parent** cannot be compared at all, and again the authors are the ones who say so — their enalapril peaks are "about 10 times those published in the literature". Their 314 ng/mL against this model's 75 is an assay-method disagreement they identify themselves, not a target to hit.

### The dose is entered as prescribed

The source gave two 5 mg tablets of enalapril **maleate**, so its doses are the labelled strength of the salt, which is what a prescription says too. Whether the fitted bioavailability of 0.606 also absorbs the maleate-to-base mass fraction is not resolvable from the text, and does not need to be: the model is used on exactly the basis it was fitted on, so a 10 mg tablet entered as 10 mg reproduces the paper. There is no salt factor and no molar conversion.

### One covariate, and it is not the one you want

Age, weight, height, sex and total body water were all available and tested. Only **body weight on enalapril's central volume** survived, to the first power, normalised to the study's mean of 69.76 kg. Nothing was retained on any clearance, and nothing at all on enalaprilat. The authors add that their base and full models differ by under 25% and call the covariates clinically unimportant.

So **there is no renal term**, and for this drug that deserves saying twice. Enalapril and enalaprilat are cleared renally and by nothing else — the source's own assumption, and the thing that made its mass balance close — yet a serum creatinine entered in the [Patient Profile](help:patient-profile) does not reach either moiety. The population was 24 healthy young adults, so the study contains no information about renal impairment. It is not that renal function is irrelevant here; it is that renal function is the single most relevant thing and this source cannot speak to it. Enalapril is dose-reduced in renal impairment in practice, and nothing in these curves shows that.

The source's remaining term, a factor of 0.730, describes its child-appropriate orodispersible mini-tablet rather than the conventional tablet. The reference formulation is the one modelled here, for which that factor is exactly 1, so the printed mean transit time already is the right one.

### This is an adult model from a paediatric programme

Worth being explicit about, because the paper invites the opposite conclusion. It was published in a paediatric journal, its title is about child-appropriate mini-tablets, and it belongs to a project whose name is "labelling of enalapril from neonate to adolescence". **Its data are 24 healthy adults.** Not one child was studied.

So the child and infant rows of the table above are the library's own allometric extrapolation from a population of 51.8 to 95.6 kg, not anything this source measured. Enalapril's conversion to enalaprilat is by carboxylesterase 1, whose activity changes with age, and nothing here represents that. Treat the paediatric columns as arithmetic rather than as evidence.

### Effect site and blood pressure

Neither moiety has an effect site, and [no antihypertensive in this library plots a blood pressure](help:models/antihypertensives). No band is drawn: there is no established therapeutic plasma range for either. A **time until threshold** is not offered either, and that is structural rather than pending — the library solves it from the exponential amplitudes its closed-form engines leave behind, and a transit chain leaves none.
