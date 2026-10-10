### The model

Sertraline's parameters are from Alhadab and Brundage (*AAPS J* 2020;22:73), a model-based meta-analysis of 60 mean concentration-time profiles (748 concentrations) from 27 published studies in healthy adults, after intravenous sertraline and single and repeated oral doses of 5 to 400 mg. Because intravenous profiles were included, the parameters are absolute rather than divided by an unknown bioavailability: two compartments, clearance 59.7 L/h, central volume 1200 L, intercompartmental clearance 161 L/h, and a peripheral volume of 928 L after a single dose and 1350 L with repeated doses. Absorption was a rate that rises with time since the dose, and the bioavailability of a single dose rises with its size, as 0.639 &times; D / (15.5 + D). The variability between studies in a meta-analysis is not the variability between patients, and none is used.

Four things are reduced to fit the simulator's engine, and each is recorded in the drug file:

- **The repeated-dose peripheral volume, 1350 L.** The simulator has one set of parameters per drug, and sertraline is taken daily. A single dose is then a little off: for 100 mg the peak is 2 per cent lower, the concentration at 24 hours 13 per cent lower and at 72 hours 10 per cent higher than with the single-dose volume. The terminal half-life is 33 hours (27 hours with the single-dose volume, close to the label's 26).
- **First-order absorption after a lag.** The source's time-varying absorption rate was replaced by the first-order rate and lag that best fit its cumulative fraction absorbed over 48 hours: 0.409/h after 1.43 hours. The fraction absorbed differs by 0.016 on average and by at most 0.11, around the end of the lag. A 100 mg dose peaks at 25 ng/mL 5.5 hours after it is taken (5.9 hours with the source's absorption; 4.5 to 8.4 hours in the literature).
- **The single-dose bioavailability is applied to every dose.** Each oral dose is scaled by its own bioavailability: 0.39 at 25 mg, 0.49 at 50, 0.55 at 100, 0.59 at 200. How the source handled bioavailability with repeated doses could not be recovered, and its abstract says that with repeated dosing bioavailability did not change with the dose. Applying the single-dose curve to each dose of a daily regimen is therefore an assumption, and chronic doses below 50 mg are probably underpredicted. Enter each dose as one row. See [Oral, intramuscular and intranasal doses](help:models/absorption).
- **Oral only.** Absolute parameters would predict intravenous doses, but there is no clinical intravenous sertraline product.

### Covariates

None. The parameters are those of healthy adults, taken to be the 70 kg reference man's, and scaled to the patient's [fat-free mass](help:models/fat-free-mass) with the switch on; with it off everyone receives the published values. Age, sex, hepatic impairment, CYP2C19 and CYP2D6 genotype and interacting drugs are not represented, and the model was not fitted in patients with depression.

### Effect site

None; only the plasma concentration is plotted. The antidepressant effect develops over weeks, which an effect site that equilibrates with plasma in minutes or hours does not describe. Serotonin transporter occupancy measured by PET is a biomarker rather than an effect: in Parsey and colleagues' study of 17 healthy volunteers after 4 to 6 days of 25 to 100 mg daily (*Biol Psychiatry* 2006;59:821-828), occupancy saturated at low plasma concentrations (half-maximal at 1.9 ng/mL), so it is near its maximum across the whole therapeutic range. The metabolite N-desmethylsertraline, which is weakly active and accumulates, is not modelled.

### Typical concentrations

The shaded band, 10 to 150 ng/mL, is the therapeutic reference range of the AGNP consensus guidelines for therapeutic drug monitoring (Hiemke and colleagues, *Pharmacopsychiatry* 2018;51:9-62). It describes trough concentrations at steady state and is for orientation only.

### Other models

Zhang and colleagues (*Heliyon* 2024;10:e25231) fitted trough concentrations from 140 hospitalised Chinese patients aged 11 to 79, with apparent clearance falling with age; its apparent half-life, about 7 hours, is inconsistent with sertraline's known half-life and reflects what trough data can identify rather than a difference in the patients. Models of the effect of CYP2C19 and CYP2D6 genotype (Castillo 2024), of pregnancy (Monfort 2024) and González de la Cruz (2026) are not combined with this one.

