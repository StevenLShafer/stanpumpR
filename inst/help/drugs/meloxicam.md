### The model

Meloxicam kinetics are from Aoyama and colleagues (*CPT Pharmacometrics Syst Pharmacol* 2017;6:823-832). They fitted a two-compartment NONMEM model to 119 healthy men aged 21 to 35 (30 Japanese, 30 Chinese, 29 Korean and 30 white), each given one 7.5 mg oral dose and sampled to 72 hours. No ethnic difference was found. The parameters are **apparent** (divided by an unmeasured bioavailability): CL/F 0.391 L/h, Vc/F 7.79 L, Q/F 1.24 L/h and Vp/F 2.73 L. Half-times at the reference man are about 1.1 and 19 hours.

### Covariates

Apparent clearance falls with each **CYP2C9** variant allele: by 14.7% per \*2 and 40% per \*3, so \*1/\*3 is 60% and \*3/\*3 20% of the \*1/\*1 value. The app has no CYP2C9 input and always simulates \*1/\*1. A script can pass `cyp2c9` to `meloxicam()` directly. Vc/F follows James lean body mass, (LBM / 55)^1.05, in either position of the [fat-free-mass switch](help:models/fat-free-mass). The source found no size effect on the other three parameters. They take the library's fat-free-mass scaling with the switch on and are left as published with it off. There is no CYP2D6 term, because none was fitted.

### Route and absorption

Because the parameters are apparent, meloxicam is offered **by mouth only**, with a bioavailability of 1: the apparent scale already contains it. Do not apply the label's absolute bioavailability (about 0.89) on top. Absorption has two parallel paths. In the source, 42.5% of the dose enters at a constant rate over 1.91 hours from the dose, and 57.5% enters a first-order depot (2.00 per hour) after a lag of the same 1.91 hours. The engine absorbs only first-order, so the constant-rate path is **approximated** by a first-order depot with the same mean absorption time (rate 2 / 1.91 per hour, no lag). The lagged path is exact. For 7.5 mg in the reference man the approximation peaks at 0.72 mcg/mL at 3.2 hours, against 0.73 mcg/mL at 3.1 hours for the published structure. It is 22% high at 1 hour and 13% low at 2 hours, and within 0.5% from 4 hours on.

### Where to be careful

The source studied single 7.5 mg doses in young healthy men. Steady-state, older, female and hepatically or renally impaired patients are extrapolations. There is no effect site and no shaded band.
