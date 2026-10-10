### The model

Meloxicam kinetics are from Aoyama and colleagues (*CPT Pharmacometrics Syst Pharmacol* 2017;6:823-832). They fitted a two-compartment NONMEM model to 119 healthy men aged 21 to 35 (30 Japanese, 30 Chinese, 29 Korean and 30 white), each given one 7.5 mg oral dose and sampled to 72 hours. No ethnic difference was found. The parameters are **apparent** (divided by an unmeasured bioavailability): CL/F 0.391 L/h, Vc/F 7.79 L, Q/F 1.24 L/h and Vp/F 2.73 L. Half-times at the reference man are about 1.1 and 19 hours.

### Covariates

Apparent clearance falls with each **CYP2C9** variant allele: by 14.7% per \*2 and 40% per \*3, so \*1/\*3 is 60% and \*3/\*3 20% of the \*1/\*1 value. The app has no CYP2C9 input and always simulates \*1/\*1. A script can pass `cyp2c9` to `meloxicam()` directly. Vc/F follows James lean body mass, (LBM / 55)^1.05, in either position of the [fat-free-mass switch](help:models/fat-free-mass). The source found no size effect on the other three parameters. They take the library's fat-free-mass scaling with the switch on and are left as published with it off. There is no CYP2D6 term, because none was fitted.

### Route and absorption

Because the parameters are apparent, the Aoyama model takes the **oral** doses only, with a bioavailability of 1: the apparent scale already contains it. Do not apply the label's absolute bioavailability (about 0.89) on top. Intravenous doses go to a separate model (below). Absorption has two parallel paths. In the source, 42.5% of the dose enters at a constant rate over 1.91 hours from the dose, and 57.5% enters a first-order depot (2.00 per hour) after a lag of the same 1.91 hours. The engine absorbs only first-order, so the constant-rate path is **approximated** by a first-order depot with the same mean absorption time (rate 2 / 1.91 per hour, no lag). The lagged path is exact. For 7.5 mg in the reference man the approximation peaks at 0.72 mcg/mL at 3.2 hours, against 0.73 mcg/mL at 3.1 hours for the published structure. It is 22% high at 1 hour and 13% low at 2 hours, and within 0.5% from 4 hours on.

### Intravenous meloxicam (ANJESO)

Intravenous doses use the population model for ANJESO, the intravenous nanocrystal formulation, from the FDA clinical pharmacology review of NDA 210583 (2020, Table 3). It was fitted to 316 subjects and 3496 concentrations, with three compartments and systemic parameters: CL 0.416 L/h × (weight / 70)^0.761 × (eGFR / 91)^0.554; Vc 4.16, V2 2.06 and V3 3.28 L × (weight / 70)^0.776; Q2 6.171 and Q3 0.835 L/h × (weight / 70)^0.761. eGFR is the CKD-EPI 2009 estimate from the creatinine in the Patient Profile. With the field blank it is an assumed normal creatinine for age and sex, so renal impairment is then not shown. The weight terms are used as published in both positions of the fat-free-mass switch. There is no CYP2C9 term.

The two models come from different studies, and no study fitted oral and intravenous meloxicam together. So each dose is simulated by the model fitted to its route, and the two curves are added: the help page's *Parallel systems* table shows which route goes where. No bioavailability links them. The oral CL/F (0.391 L/h) is lower than the intravenous CL (0.416 L/h), which would imply a bioavailability above 1: that is a difference between the studies.

For 30 mg in the reference man, the model gives about 7 mcg/mL at 1 minute, 3.9 mcg/mL at 1 hour and 1.0 mcg/mL at 24 hours. Its terminal half-life is 17 hours, shorter than the label's "approximately 24 hours". The FDA reviewer noted that the model fits late times poorly. The parameters were taken from a literature summary and have not yet been checked against the review's Table 3.

### Where to be careful

The oral source studied single 7.5 mg doses in young healthy men. Steady-state, older, female and hepatically or renally impaired patients are extrapolations. The intravenous model had eight subjects with moderate renal impairment and none with severe, and it describes the ANJESO nanocrystal only, not other intravenous formulations such as QP001. There is no effect site and no shaded band.
