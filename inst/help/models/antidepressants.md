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

All are given by mouth only (`mg PO`, with once-daily schedules, twice-daily ones for all but fluoxetine, and three times daily for venlafaxine). Most are **apparent** models, fitted to oral data alone, which predict oral concentrations correctly and intravenous ones not at all. See [Oral, intramuscular, intranasal and regional anesthesia doses](help:models/absorption).

## What a curve can and cannot tell you

**Population, not patient.** Every curve is the typical patient of its source population. No model is offered as transferable beyond that population without calibration. When ten published escitalopram models were tested against an independent cohort, none gave reliable population predictions without it (Yan and colleagues, 2025).

**Trough data shape the half-lives.** Most of these models were fitted to one or two concentrations per patient, drawn just before a dose at steady state. Such data pin down the average steady-state concentration (dose rate divided by clearance) well and the shape within a dosing interval poorly. Several apparent half-lives therefore differ from the product label's: mirtazapine's is about 7 hours against the label's 20 to 40, paroxetine's about 54 hours at 71 years against the label's 21, and fluoxetine's about 6 hours against the label's 4 to 6 days, with norfluoxetine's 20 minutes against 4 to 16 days. Fluoxetine's model was held back until its units were checked: its source's own simulated troughs confirm that the printed parameters are what the authors used, so the troughs are right and the time course is not. Each drug's page says where this applies.

**The shaded band** is the therapeutic reference range for trough concentrations at steady state of the AGNP consensus guideline's 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5), which revised 88 ranges. Against the 2017 update (Hiemke and colleagues, 2018) it narrows sertraline to 10 to 50 ng/mL (from 10 to 150), moves paroxetine to 30 to 80 (from 20 to 65), and adopts the task force's systematic reviews for escitalopram, 20 to 40 (Eichentopf and colleagues, 2022), duloxetine, 20 to 120 (Amann and colleagues, 2024), and venlafaxine, 140 to 600 for the active moiety with 85 to 380 for ODV alone (Lense and colleagues, 2024); citalopram (50 to 110) and mirtazapine (30 to 80) are unchanged. Bupropion's range, 850 to 1500 ng/mL, is hydroxybupropion's and is drawn on its row. Fluoxetine's, 150 to 300 ng/mL (120 to 500 in 2017), and venlafaxine's are for a parent plus its metabolite, which share the sum too evenly for either row to carry it, so no band is drawn and the drug's page gives the range. Every range, its alert level, its source and how far the source was read are in `inst/extdata/therapeuticRanges.csv`.

**No effect site.** Antidepressant response develops over weeks and does not follow plasma concentration with a delay that a ke0 can describe. There is no effect-site curve, and *Time until threshold* is not computed.

**Transporter occupancy is not plotted.** PET and SPECT studies relate plasma concentration to serotonin-transporter occupancy for several of these drugs: an EC50 of 11.7 ng/mL of racemic citalopram (Meyer and colleagues, 2004), 1.9 ng/mL of sertraline (Parsey and colleagues, 2006), 3.7 ng/mL of duloxetine (Takano and colleagues, 2006), 3.4 ng/mL of venlafaxine. These are biomarkers measured in small groups, each with its own tracer, brain region and concentration definition. Each drug's page quotes them as context. None is converted into a probability of remission, because no supported equation does that.

## What is deliberately absent

These were assessed and left out. They are not forgotten:

- **Paroxetine, Feng and colleagues' Michaelis-Menten model** (2006). Left out after reading the full paper, which does not resolve it. Its structure is printed (two compartments, saturable elimination from the central one, NONMEM's general nonlinear model) and so is its covariate code, but no bioavailability or dose-scaling term. Its own population predictions (Figure 2) stay below about 300 ng/mL over nearly three years on 10 to 40 mg a day, so its elimination capacity must exceed the dose. No reading of the printed Vmax (454 to 474) and Km (205) does that and lands near the concentrations seen in practice: as µg/h, elimination stops at about 11 mg a day and the model accumulates without limit; as a rate per litre of central volume or per minute, 20 mg a day gives under 7 ng/mL; as a near-linear clearance of 2.2 L/h, it gives about 380 ng/mL. An elderly patient on 20 mg a day typically has 50 to 100. With all of the dose absorbed, matching that would need a Vmax of about 3,300 to 4,550 µg/h, seven to ten times the printed value. The paper also places its weight exponent on V2 in Table 5 and on Vmax in its code. Kim's model is used instead.
- **Symptom-score models.** Shigetome and colleagues' paroxetine MADRS model needs the patient's measured week-1 response, so it cannot forecast from a baseline. No other model links concentration to remission.
- **Metabolites** other than hydroxybupropion, norfluoxetine and desvenlafaxine: desmethylcitalopram, desmethylsertraline and the others. Their sources' conversion conventions were not recovered.
- **Co-medication.** Mirtazapine's interactions with paroxetine and fluvoxamine are in its source, but the app has no field for co-medication.

## References

Hiemke C, Bergemann N, Clement HW, et al. Consensus guidelines for therapeutic drug monitoring in neuropsychopharmacology: update 2017. *Pharmacopsychiatry* 2018;51:9-62. https://doi.org/10.1055/s-0043-116492

Hart XM, Gründer G, Hiemke C, et al. Consensus guidelines for therapeutic drug monitoring in neuropsychopharmacology: update 2026. *Pharmacopsychiatry* 2026;59:166-295. https://doi.org/10.1055/a-2860-7861

Eichentopf L, Hiemke C, Conca A, et al. Systematic review and meta-analysis on the therapeutic reference range for escitalopram. *Front Psychiatry* 2022;13:972141. https://doi.org/10.3389/fpsyt.2022.972141

Amann F, Kochtyrew M, Zernig G, Gründer G, Hart XM. Therapeutic reference range for duloxetine in the treatment of depression revised. *Neurosci Appl* 2024;3:104077. https://doi.org/10.1016/j.nsa.2024.104077

Lense XM, Hiemke C, Funk CSM, et al. Venlafaxine's therapeutic reference range in the treatment of depression revised. *Psychopharmacology* 2024;241:275-289. https://doi.org/10.1007/s00213-023-06484-7

Yan K, Xie X, Shao Q, Wang N, Yang M. A step toward precision dosing of escitalopram in Chinese patients: an external evaluation of published population pharmacokinetic models. *Drug Des Devel Ther* 2025;19:10937-10951. https://doi.org/10.2147/DDDT.S546904.

Meyer JH, Wilson AA, Sagrati S, et al. Serotonin transporter occupancy of five selective serotonin reuptake inhibitors at different doses: an [11C]DASB positron emission tomography study. *Am J Psychiatry* 2004;161:826-835. https://doi.org/10.1176/appi.ajp.161.5.826

Feng and colleagues, *Br J Clin Pharmacol* 2006, https://doi.org/10.1111/j.1365-2125.2006.02629.x; Shigetome and colleagues, *CPT Pharmacometrics Syst Pharmacol* 2025, https://doi.org/10.1002/psp4.70032.
