### A research model, behind an opt-in

Methamphetamine is one of stanpumpR's **illicit-drug research models**. These are hidden until you turn on **Show illicit drugs** above the dose table (off by default), and their names appear in red in the dose table. The curve is a *modelled plasma concentration* offered for research and teaching. It is not dosing advice, a safety threshold, or a psychosis, impairment or toxicity threshold, and it is not a prediction for any individual. It describes the d-enantiomer insufflated by experienced adult users under study conditions; street material differs in purity, enantiomer content and adulterants.

### Which methamphetamine

This is **d-methamphetamine**, the S-(+)-enantiomer that Harris and Cook studied. Mendelson and colleagues found the enantiomers' kinetics broadly similar but their elimination half-lives significantly different (l 13.3-15.0 h against d 10.2-10.7 h), and the d-enantiomer's exposure 30% smaller after the racemate than after d alone. l-methamphetamine, the nasal decongestant, and the racemate are therefore **not** this model and are not in the library.

It is also a different drug from the library's **mixed amphetamine salts**, which is an oral prescription amphetamine product.

### The model

Harris and colleagues gave deuterium-labelled intravenous methamphetamine at the same time as an intranasal or smoked dose in eight experienced users (Harris DS, Boxenbaum H, Everhart ET, et al. *Clin Pharmacol Ther* 2003;74(5):475-486). They reported a clearance of 272 mL/h/kg, a steady-state volume of 4.2 L/kg, a mean residence time of 16 hours and an intravenous elimination half-life of 11.4 hours.

Those three summaries are mutually consistent with **one compartment**, which is why the reduction here is a mild one: clearance × mean residence time gives 4.35 L/kg against the reported 4.2 L/kg, and the resulting half-life is 10.7 hours against the reported 10.7-11.4 hours. Cook's oral study reached the same structure independently, reporting that a one-compartment model with a lag time fitted the data best, with a mean half-life of 10.1 hours.

### Absorption: the rate constant is derived, not published

Harris measured intranasal bioavailability as **79%**, with the maximum plasma concentration at **2.7 hours**. He reported no absorption rate constant. In a one-compartment model whose elimination rate is already fixed by his own clearance and volume, the time of the peak determines the absorption rate constant uniquely, so stanpumpR *derives* it: 1.12 per hour, an absorption half-time of 37 minutes. It reproduces Harris's observed peak time exactly and claims nothing beyond that. (The same approach sets hydrocodone's oral absorption rate in this library.)

### Why only the intranasal route is offered

- **Intravenous** is not offered. Harris's intravenous parameters are a non-compartmental summary, and a steady-state volume used as a one-compartment volume misstates the first minutes after a bolus, where distribution dominates.
- **Oral** is not offered. The absolute oral bioavailability, 67.2 ± 3.1%, is a calculation Cook reported across his 1993 smoking study and his 1992 oral study, and it is solid. What is missing is an absorption rate for an *immediate-release* dose: the published oral time course (Schepers and colleagues, *Clin Chem* 2003) is a **sustained-release** tablet whose peak spanned 2 to 12 hours, which must not be used as an immediate-release input.
- **Smoked** is not offered. Bioavailability has been reported three ways with three denominators: 67% of the estimated *delivered* dose and 37.4% of the nominal *pipe load* (Harris), and 90.3 ± 10.4% of the *inhaled* dose (Cook). They must never be applied together, and stanpumpR has no pulmonary route. Smoke is not aliased to the intranasal route.

See [Research models of drugs of abuse](help:models/illicit-drugs) for the route-by-route record.

### What is not modelled

There is **no effect site**, no MEAC and no concentration band. Heart rate, blood pressure and subjective effects peak within minutes of an intranasal dose while plasma peaks near 2.7 hours, so effect and plasma are in clear hysteresis and no published model resolves that into an equilibration constant this engine could carry. The amphetamine metabolite is not modelled either: an exposure ratio does not identify a formation fraction without the metabolite's own disposition.

### Covariates

Harris reports clearance and volume per kilogram, so the published scaling is linear in weight: with *Adjust weight to fat-free mass* off, 272 mL/h/kg and 4.2 L/kg are reproduced exactly at any weight. With it on, the volume scales with the Al-Sallami fat-free mass ratio and clearance with its 0.75 power. The absorption rate constant is not scaled.

### Where to be careful

This is an intranasal plasma-exposure estimate in experienced adult users. Blood concentration does not identify an individual's stimulant effect, cardiovascular risk, psychosis risk or toxicity threshold, and tolerance — which Cook showed is not explained by changed kinetics — is not represented.
