### The model

Sertraline's parameters are from Alhadab and Brundage (*AAPS J* 2020;22:73), a model-based meta-analysis of 60 mean concentration-time profiles (748 concentrations) from 27 published studies in healthy adults, after intravenous sertraline and single and repeated oral doses of 5 to 400 mg. Because intravenous profiles were included, the parameters are absolute rather than divided by an unknown bioavailability: two compartments, clearance 59.7 L/h, central volume 1200 L, intercompartmental clearance 161 L/h, and a peripheral volume of 928 L after a single dose and 1350 L with repeated doses. Absorption was a rate that rises with time since the dose, and the bioavailability of a single dose rises with its size, as 0.639 &times; D / (15.5 + D). The variability between studies in a meta-analysis is not the variability between patients, and none is used.

Four things are reduced to fit the simulator's engine, and each is recorded in the drug file:

- **The repeated-dose peripheral volume, 1350 L.** The simulator has one set of parameters per drug, and sertraline is taken daily. A single dose is then a little off: for 100 mg the peak is 2 per cent lower, the concentration at 24 hours 13 per cent lower and at 72 hours 10 per cent higher than with the single-dose volume. The terminal half-life is 33 hours (27 hours with the single-dose volume, close to the label's 26).
- **First-order absorption after a lag.** The source's time-varying absorption rate was replaced by the first-order rate and lag that best fit its cumulative fraction absorbed over 48 hours: 0.409/h after 1.43 hours. The fraction absorbed differs by 0.016 on average and by at most 0.11, around the end of the lag. A 100 mg dose peaks at 25 ng/mL 5.5 hours after it is taken (5.9 hours with the source's absorption; 4.5 to 8.4 hours in the literature).
- **The single-dose bioavailability is applied to every dose.** Each oral dose is scaled by its own bioavailability: 0.39 at 25 mg, 0.49 at 50, 0.55 at 100, 0.59 at 200. How the source handled bioavailability with repeated doses could not be recovered, and its abstract says that with repeated dosing bioavailability did not change with the dose. Applying the single-dose curve to each dose of a daily regimen is therefore an assumption, and chronic doses below 50 mg are probably underpredicted. Enter each dose as one row. See [Oral, intramuscular, intranasal and regional anesthesia doses](help:models/absorption).
- **Oral only.** Absolute parameters would predict intravenous doses, but there is no clinical intravenous sertraline product.

### Covariates

None. The parameters are those of healthy adults, taken to be the 70 kg reference man's, and scaled to the patient's [fat-free mass](help:models/fat-free-mass) with the switch on; with it off everyone receives the published values. Age, sex, hepatic impairment, CYP2C19 and CYP2D6 genotype (the Patient Profile's **CYP 2C19** and **CYP 2D6** fields do not change this model) and interacting drugs are not represented, and the model was not fitted in patients with depression.

### Effect site

None; only the plasma concentration is plotted. The antidepressant effect develops over weeks, which an effect site that equilibrates with plasma in minutes or hours does not describe. Serotonin transporter occupancy measured by PET is a biomarker rather than an effect: in Parsey and colleagues' study of 17 healthy volunteers after 4 to 6 days of 25 to 100 mg daily (*Biol Psychiatry* 2006;59:821-828), occupancy saturated at low plasma concentrations (half-maximal at 1.9 ng/mL), so it is near its maximum across the whole therapeutic range. The metabolite N-desmethylsertraline, which is weakly active and accumulates, is not modelled.

### Typical concentrations

The shaded band, 10 to 50 ng/mL with 30 as the typical value (its midpoint, not a target), is the therapeutic reference range of the AGNP consensus guidelines' 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5, https://doi.org/10.1055/a-2860-7861), for troughs 24 hours after a dose. It narrows the 2017 update's 10 to 150 ng/mL (Hiemke and colleagues, *Pharmacopsychiatry* 2018;51:9-62): 80% of serotonin transporters are occupied at 10 ng/mL, and above 50 ng/mL efficacy may fall and adverse reactions rise. The laboratory alert level is 150 ng/mL. It is for orientation only.

### Other models

Zhang and colleagues (*Heliyon* 2024;10:e25231) fitted trough concentrations from 140 hospitalised Chinese patients aged 11 to 79, with apparent clearance falling with age; its apparent half-life, about 7 hours, is inconsistent with sertraline's known half-life and reflects what trough data can identify rather than a difference in the patients. Models of the effect of CYP2C19 and CYP2D6 genotype (Castillo 2024), of pregnancy (Monfort 2024) and González de la Cruz (2026) are not combined with this one.

### Pregnancy

With **Pregnant** ticked, the elimination clearance is multiplied by 0.60: sertraline is the one drug in the library whose clearance **falls** in pregnancy. Westin and colleagues (*PLoS One* 2017;12:e0181082) followed 37 sertraline pregnancies through routine drug monitoring and found the dose-adjusted serum concentration 68% higher in the third trimester than the same women's own baseline (95% CI 37 to 106%), and 1/1.68 is 0.60; a meta-analysis of 18 patients (Schoretsanitis 2020) gives a smaller rise, 38%, and Freeman and colleagues (2008) found wide differences between women. The volumes, the absorption and the dose-dependent bioavailability are unchanged. Clinically the change mainly argues against reducing the dose reflexively. See [Pregnancy](help:models/pregnancy).
