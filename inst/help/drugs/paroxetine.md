### The model

Paroxetine's parameters are from Kim and colleagues (*Drug Des Devel Ther* 2015;9:5247-5254), who fitted 271 therapeutic drug monitoring concentrations from 127 Korean psychiatric outpatients taking paroxetine at steady state: one compartment, absorption 0.908/h and apparent volume 1020 L (both fixed), and an apparent clearance of 13.1 L/h at a daily dose of 25 mg and age 71, which falls with age (power &minus;0.702) and with the daily dose (power &minus;0.363). Paroxetine inhibits its own metabolism by CYP2D6, so exposure rises more than in proportion to the dose. The parameters are apparent, divided by an unknown bioavailability, so paroxetine is offered by mouth only.

### The dose term

A clearance that depends on the dose cannot be used directly by the simulator, whose parameters are fixed before the doses are seen. Instead, clearance is Kim's value at 25 mg a day, and each oral dose D is scaled by (D / 25)<sup>0.363</sup>. At steady state on D mg once a day the exposure over a day is then D &times; (D / 25)<sup>0.363</sup> / CL<sub>25</sub>, which is exactly Kim's D / CL(D). So for once-daily dosing the average steady-state concentration is Kim's at every dose: at age 50, 46 ng/mL on 20 mg and 160 ng/mL on 50 mg. The scale exceeds 1 above 25 mg because it is an exposure scale on apparent parameters, not a bioavailability. See [Oral, intramuscular and intranasal doses](help:models/absorption).

What this does not reproduce:

- **Divided doses.** Each administration is read as the whole day's dose. Taken twice daily, the same daily dose gives 22 per cent less exposure than Kim's model (2<sup>&minus;0.363</sup> = 0.78), and three times daily 33 per cent less. The model is exact for once-daily dosing only.
- **The half-life's dependence on the dose.** The half-life is the 25 mg/day value: 54 hours at 71, 42 hours at 50.
- **The first dose.** Kim's patients were at steady state; the dose term is applied to the first dose as well.

### The half-life

The label's half-life is about 21 hours; this model's is 54 hours at 71. Kim's data were trough concentrations, from which the volume could not be estimated, and the long half-life is an artefact of the data rather than a property of the patients. The model predicts the average steady-state concentration, which depends only on clearance, best; the swing within a day is too small, and steady state is approached too slowly (7.5 days to 90 per cent at 71, against about 3 by the label).

### Covariates

Age, as published, with 71 years as the reference: at 20, clearance is 2.4 times the reference value. The power of age has no lower bound, and children are far outside Kim's data; the model should not be used for them. Kim retained no size covariate, so the parameters are taken to be the 70 kg reference man's and scaled to the patient's [fat-free mass](help:models/fat-free-mass) with the switch on; with it off everyone receives the published values. CYP2D6 genotype and interacting drugs are not represented.

### Effect site

None; only the plasma concentration is plotted. The antidepressant effect develops over weeks. Serotonin transporter occupancy is a biomarker rather than an effect: in Catafau and colleagues' SPECT study of 10 patients after 4 to 6 weeks of 20 mg daily (*Psychopharmacology* 2006;189:145-153), occupancy reached a maximum of 70.5 per cent, half-maximal at 2.7 ng/mL.

### Typical concentrations

The shaded band, 20 to 65 ng/mL, is the therapeutic reference range of the AGNP consensus guidelines for therapeutic drug monitoring (Hiemke and colleagues, *Pharmacopsychiatry* 2018;51:9-62); an observational study (Yuan 2025) also associated response with this range. It describes trough concentrations at steady state and is for orientation only.

### Other models

Feng and colleagues (*Br J Clin Pharmacol* 2006;61:558-569) fitted 1970 concentrations from 171 depressed patients aged 69 to 95 with two compartments and saturable (Michaelis-Menten) elimination depending on CYP2D6 genotype. It is not used: its maximal elimination rate in extensive metabolisers, about 450 &micro;g/h, is about 11 mg a day, less than the doses of up to 40 mg a day the patients took without their concentrations rising without limit, and how its doses and bioavailability were defined to reconcile the two could not be resolved. Shigetome and colleagues (*CPT Pharmacometrics Syst Pharmacol* 2025;14:1119-1127) fitted 179 Japanese patients with major depression and linked the first week's exposure to the depression rating over six weeks; that model needs the patient's measured rating after one week, and is not implemented.

