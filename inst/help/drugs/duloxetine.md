### The model

Duloxetine's kinetics are from Zhong and colleagues (*Chinese Journal of Clinical Pharmacology* 2026, [doi 10.13699/j.cnki.1001-6821.2026.10.009](https://doi.org/10.13699/j.cnki.1001-6821.2026.10.009)), a population analysis of therapeutic drug monitoring in 325 depressed inpatients with 450 plasma concentrations. One compartment described the data:

| Parameter | Typical value | Variability (CV) |
|---|---|---|
| CL/F | 64.6 L/h in men, 25% lower in women (48.45 L/h) | 42.2% |
| V/F | 1530 L | |
| ka | 0.168 /h, fixed | |

The proportional residual error was 27.1%. The curve is the typical patient's; the variability is not plotted. The drug file converts these to litres per minute and per minute, the library's units.

The half-life is 16.4 hours in a man and 21.9 hours in a woman, longer than the label's mean of about 12 hours. The data were monitoring samples, mostly troughs, which pin down the steady-state average (the daily dose over the clearance) better than the shape of the curve between doses. In the reference man, 60 mg once daily averages 38.7 ng/mL at steady state, swinging between about 29 and 45 ng/mL over the day; in a woman of the same size the average is 51.6 ng/mL.

### Oral only

Every patient took duloxetine by mouth, and there is no intravenous product, so the clearance and volume are **apparent**: divided by the unmeasured bioavailability (about 50%). Apparent parameters predict oral concentrations correctly, because the unknown bioavailability cancels, and would predict intravenous concentrations wrong by one over it. Duloxetine is therefore offered only as `mg PO`, `mg PO qd` and `mg PO bid`, with no lag.

### Covariates

Sex is the only covariate: women's clearance is 25% lower, as published. The model has no size covariate, so its values are read as the 70 kg reference man's and take the library's [fat-free-mass scaling](help:models/fat-free-mass) for fixed published parameters; with the switch off, everyone receives Zhong's values unscaled. The cohort's body size was not available, so the values could not be re-anchored to the cohort's own fat-free mass. With the switch on a woman's clearance is lowered twice, by her smaller fat-free mass and by the published 25%, part of which may itself reflect body size.

### No effect site

Only the plasma concentration is plotted. The antidepressant response follows the concentration by weeks, not minutes, and no validated equation relates a duloxetine concentration to remission, so there is no effect site and no MEAC.

Positron emission tomography measures transporter occupancy, a biomarker rather than a clinical effect, and is given here for context only. Takano and colleagues (2006) found serotonin-transporter occupancy half-maximal at 3.7 ng/mL, so the serotonin transporter is nearly saturated throughout the therapeutic range. Norepinephrine-transporter occupancy needs far higher concentrations: half-maximal at 58.0 ng/mL in healthy men (Moriguchi and colleagues, 2017) and 71.21 ng/mL in major depressive disorder (Moriguchi and colleagues, 2025). Neither curve is plotted: an occupancy curve would need a maximum occupancy, which setting to 100% would be an assumption. Yuen and colleagues' (2013) pharmacodynamic model was built for diabetic neuropathic pain and does not predict response in depression.

### Typical concentrations

The shaded band, 30 to 120 ng/mL, with 60 as the typical value, is the AGNP consensus therapeutic reference range (Hiemke and colleagues, *Pharmacopsychiatry* 2018;51:9-62).

### Not used

Lobo and colleagues' pooled adult model (2009), with sex, smoking, age and dose effects, was not used because its fixed-effect table could not be verified. Shibata and colleagues (2023) described Japanese children and adolescents with major depressive disorder (CL/F 81.4 L/h, V/F 1170 L, ka 0.168 /h); that is a separate population, and its values are not mixed with Zhong's.

### Where to be careful

- The cohort was Chinese adult inpatients; the child and infant in the table above are extrapolation.
- Smoking induces duloxetine's metabolism (CYP1A2), and fluvoxamine and ciprofloxacin inhibit it; none is modelled.
- Between-patient variability in clearance is 42%, so a measured concentration may differ widely from the curve.
