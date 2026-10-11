### The model

Escitalopram's kinetics are from Liu and colleagues (*Front Pharmacol* 2022;13:964758, [doi:10.3389/fphar.2022.964758](https://doi.org/10.3389/fphar.2022.964758)), a population analysis of therapeutic drug monitoring in 106 Chinese psychiatric patients taking oral escitalopram, sampled mostly at trough. One compartment with first-order absorption described the data:

| Parameter | Typical value |
|---|---|
| CL/F | 16.3 L/h (CYP2C19 normal metabolisers) |
| V/F | 815 L |
| ka | 0.6 /h, fixed (trough samples cannot identify absorption) |

The elimination half-life is ln 2 × 815 / 16.3 = 34.7 hours, a little longer than the 27 to 32 hours on the product label. Steady state is the daily dose over the clearance: 10 mg once daily gives an average of 10 / 24 / 16.3 mg/L = 25.6 ng/mL in a normal metaboliser. The curve is the typical patient's; between-patient and residual variability are not used.

### Oral only

No patient received intravenous escitalopram, so the clearance and volume are **apparent**, divided by the unmeasured bioavailability. They predict oral concentrations correctly, because the bioavailability cancels, and would predict intravenous ones wrong, so only oral units are offered (**mg PO**, and the scheduled **mg PO qd** and **mg PO bid**), and the bioavailability is carried as 1. See [Absorption](help:models/absorption).

### CYP2C19

The **CYP 2C19** field in the Patient Profile scales clearance by the multipliers Liu estimated relative to normal ("extensive") metabolisers: 0.847 for intermediate and 0.479 for poor metabolisers, so a poor metaboliser's steady state is about twice a normal one's. **Rapid and ultrarapid metabolisers were not estimated** and are given the normal value: the source's normal group was the reference, and carriers of CYP2C19\*17, uncommon in Chinese cohorts, were presumably counted with it. That probably overstates their concentrations.

### What is gated

- **Age.** Liu reports an age coefficient (0.0077) on clearance, but the exact form of the equation was not recovered, so **no age effect is applied**: the same parameters are used at every age. A guessed form could move an older patient's clearance by tens of percent in either direction.
- **Body size.** The source has no size covariate, so its values are taken as the 70 kg reference man's and scaled to [fat-free mass](help:models/fat-free-mass) (volume by the fat-free-mass ratio, clearance by that ratio to the 0.75 power); with the switch off they are used exactly as published at any size. The cohort's weights were not available to re-anchor the reference, and a Chinese psychiatric cohort was probably lighter than 70 kg.
- **The desmethyl metabolite** is not modelled.

### No effect site

The antidepressant effect develops over weeks, not minutes, and no equilibration rate constant exists, so there is no effect site: the plot shows plasma escitalopram. The shaded band is the AGNP consensus therapeutic reference range, 15 to 80 ng/mL (Hiemke and colleagues, *Pharmacopsychiatry* 2018;51:9-62), with 40 ng/mL as the typical line. Serotonin-transporter occupancy from PET (Kim and colleagues 2017) is not plotted, because the units of the published EC50 values could not be verified.

### Where to be careful

- Liu and colleagues' own external validation (*Drug Des Devel Ther* 2025, [doi:10.2147/DDDT.S546904](https://doi.org/10.2147/DDDT.S546904)) found that population predictions, without a measured concentration to calibrate them, were unreliable for individual patients. Treat the curve as the typical patient's, not a prediction for the one in front of you.
- Other published models were not mixed with this one. Jin and colleagues (2010) fitted adults with major depression (CL/F 23.5 L/h, V/F 884 L; their covariates were not verified). Poweleit and colleagues (2023) fitted children and adolescents (CL/F 14.2 L/h × body surface area / 1.73 m² × a CYP2C19 factor, V/F 428 L, ka 0.8 /h); for a child that is the better source, and the child and infant in the table above are extrapolation of an adult model.
- For racemic citalopram see [citalopram](help:drugs/citalopram).

The model was implemented by Claude Code at the request of Steven L. Shafer, from the published parameters.
