## What is here

The **Antidepressants** in the startup menu are a library of named, source-specific population pharmacokinetic models. Each one reproduces a single published analysis in the population it was fitted in. None is a pooled "antidepressant model", and none predicts whether a patient will respond.

| Drug | Source | Population | What the curve is |
|---|---|---|---|
| [escitalopram](help:drugs/escitalopram) | Liu and colleagues, 2022 | 106 Chinese psychiatric patients, mostly troughs | One compartment, apparent oral; CYP2C19 on clearance |
| [citalopram](help:drugs/citalopram) | Akil and colleagues, 2016 | 81 older patients with Alzheimer's agitation (CitAD) | R plus S citalopram, each one compartment, reduced exactly to two compartments; CYP2C19, age, sex and weight |
| [sertraline](help:drugs/sertraline) | Alhadab and Brundage, 2020 | Mean profiles from 27 studies in healthy adults, intravenous and oral | Two compartments, absolute parameters; absorption and dose-dependent bioavailability reduced to the engine |
| [paroxetine](help:drugs/paroxetine) | Kim and colleagues, 2015 | Korean therapeutic drug monitoring | One compartment, apparent oral; age, and an empirical power of the dose |
| [duloxetine](help:drugs/duloxetine) | Zhong and colleagues, 2026 | 325 Chinese depressed inpatients | One compartment, apparent oral; sex |
| [mirtazapine](help:drugs/mirtazapine) | Yan and colleagues, 2026 | 105 Chinese depressed inpatients | One compartment, apparent oral; BMI of 28 or more. The abstract and Table 2 disagree; Table 2 is used |
| [fluoxetine](help:drugs/fluoxetine) | Han and colleagues, 2025 | 198 Chinese psychiatric patients, steady-state troughs only | One compartment, apparent oral, with [norfluoxetine](help:drugs/norfluoxetine) formed from it; sex. **Only the trough at steady state is valid**: the half-lives are hours, not days |
| [venlafaxine](help:drugs/venlafaxine) | Wang and colleagues, 2022 | 24 healthy volunteers (rich profiles) and 127 psychiatric patients (troughs) | One compartment, apparent oral, with [desvenlafaxine](help:drugs/desvenlafaxine) formed from it and by first pass; the patients' clearance |
| [bupropion](help:drugs/bupropion) | Ghimire and colleagues, 2026 | 19 adults, one 150 mg sustained-release dose | Two compartments for bupropion and two for [hydroxybupropion](help:drugs/hydroxybupropion), formed from it |

All are given by mouth only (`mg PO`, with once- and twice-daily schedules). Most are **apparent** models, fitted to oral data alone, which predict oral concentrations correctly and intravenous ones not at all. See [Oral, intramuscular and intranasal doses](help:models/absorption).

## What a curve can and cannot tell you

**Population, not patient.** Every curve is the typical patient of its source population. No model is offered as transferable beyond that population without calibration. When ten published escitalopram models were tested against an independent cohort, none gave reliable population predictions without it (Liu and colleagues, 2025).

**Trough data shape the half-lives.** Most of these models were fitted to one or two concentrations per patient, drawn just before a dose at steady state. Such data pin down the average steady-state concentration (dose rate divided by clearance) well and the shape within a dosing interval poorly. Several apparent half-lives therefore differ from the product label's: mirtazapine's is about 7 hours against the label's 20 to 40, paroxetine's about 54 hours at 71 years against the label's 21, and fluoxetine's about 6 hours against the label's 4 to 6 days, with norfluoxetine's 20 minutes against 4 to 16 days. Fluoxetine's model was held back until its units were checked: its source's own simulated troughs confirm that the printed parameters are what the authors used, so the troughs are right and the time course is not. Each drug's page says where this applies.

**The shaded band** is the therapeutic reference range of the AGNP consensus guideline (Hiemke and colleagues, 2018) for trough concentrations at steady state. For bupropion the guideline's range is for bupropion plus hydroxybupropion, and is drawn on the hydroxybupropion row, which carries most of the sum.

**No effect site.** Antidepressant response develops over weeks and does not follow plasma concentration with a delay that a ke0 can describe. There is no effect-site curve, and *Time until threshold* is not computed.

**Transporter occupancy is not plotted.** PET and SPECT studies relate plasma concentration to serotonin-transporter occupancy for several of these drugs: an EC50 of 11.7 ng/mL of racemic citalopram (Meyer and colleagues, 2004), 1.9 ng/mL of sertraline (Parsey and colleagues, 2006), 3.7 ng/mL of duloxetine (Takano and colleagues, 2006), 3.4 ng/mL of venlafaxine. These are biomarkers measured in small groups, each with its own tracer, brain region and concentration definition. Each drug's page quotes them as context. None is converted into a probability of remission, because no supported equation does that.

## What is deliberately absent

These were assessed and left out until their sources are reconciled. They are not forgotten:

- **Paroxetine, Feng and colleagues' Michaelis-Menten model** (2006). Its sole elimination route, at its printed Vmax of 454 to 474 µg/h, can remove at most about 11 mg a day, below the up to 40 mg a day the study gave. The model as printed cannot reach a steady state, so the source's input convention must be recovered first. Kim's model is used instead.
- **Symptom-score models.** Shigetome and colleagues' paroxetine MADRS model needs the patient's measured week-1 response, so it cannot forecast from a baseline. No other model links concentration to remission.
- **Metabolites** other than hydroxybupropion, norfluoxetine and desvenlafaxine: desmethylcitalopram, desmethylsertraline and the others. Their sources' conversion conventions were not recovered.
- **Co-medication.** Mirtazapine's interactions with paroxetine and fluvoxamine are in its source, but the app has no field for co-medication.

## References

Hiemke C, Bergemann N, Clement HW, et al. Consensus guidelines for therapeutic drug monitoring in neuropsychopharmacology: update 2017. *Pharmacopsychiatry* 2018;51:9-62. https://doi.org/10.1055/s-0043-116492

Liu and colleagues, external evaluation of ten escitalopram population models, *Drug Des Devel Ther* 2025, https://doi.org/10.2147/DDDT.S546904.

Meyer JH, Wilson AA, Sagrati S, et al. Serotonin transporter occupancy of five selective serotonin reuptake inhibitors at different doses: an [11C]DASB positron emission tomography study. *Am J Psychiatry* 2004;161:826-835. https://doi.org/10.1176/appi.ajp.161.5.826

Feng and colleagues, *Br J Clin Pharmacol* 2006, https://doi.org/10.1111/j.1365-2125.2006.02629.x; Shigetome and colleagues, *CPT Pharmacometrics Syst Pharmacol* 2025, https://doi.org/10.1002/psp4.70032.
