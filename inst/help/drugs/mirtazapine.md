### The model

Mirtazapine's kinetics are from Yan and colleagues (*Drug Des Devel Ther* 2026, [doi 10.2147/DDDT.S601238](https://doi.org/10.2147/DDDT.S601238)), a population analysis of therapeutic drug monitoring in 105 Chinese inpatients with depression, with 210 plasma concentrations. One compartment described the data. From the paper's Table 2:

| Parameter | Typical value |
|---|---|
| CL/F | 28.9 L/h, 29.4% lower at a BMI of 28 kg/m² or more |
| V/F | 310 L |
| ka | 1.2 /h, fixed |

**Source discrepancy.** The paper's abstract gives CL/F as 29.3 L/h and V/F as 348 L, which do not match Table 2. Table 2, the final model's parameter table, is used. The difference is small for the steady-state average (1.4%). Table 2 also prints the unit of ka as L/h, a typo for /h.

The variability (variances 0.0613 on clearance and 0.131 on volume; residual 0.0747) is not plotted.

### Read the steady-state average, not the shape

The half-life implied by these values is 7.4 hours, against the 20 to 40 hours of the label. Two concentrations per patient, mostly troughs at steady state, identify the clearance (the steady-state average is the daily dose over the clearance) but not the volume. The small volume, and so the short half-life, is an artefact of that design. **The shape of the curve within a dosing interval, the peak-to-trough swing and the time to steady state are not reliable**; the steady-state average is what the data identify. In the reference man, 30 mg once daily averages 43.3 ng/mL, inside the therapeutic range, but the curve swings from about 13 to 88 ng/mL over the day, far more than mirtazapine does.

### Oral only

Every patient took mirtazapine by mouth, and there is no intravenous product, so the clearance and volume are **apparent**: divided by the unmeasured bioavailability (about 50%). They predict oral concentrations correctly and would predict intravenous ones wrong by one over the bioavailability, so mirtazapine is offered only as `mg PO`, `mg PO qd` and `mg PO bid`, with no lag.

### Covariates

**Body mass index.** At a BMI of 28 kg/m² or more (the Chinese threshold for obesity), clearance is 29.4% lower. BMI is computed from the patient's actual weight and height, as published, and the effect is a step at 28, not a gradient. For the reference man, 30 mg daily averages 43.3 ng/mL below the threshold and 61.3 ng/mL above it.

**Co-medication, not modelled.** Yan found clearance 26.9% lower with paroxetine and 51.1% lower with fluvoxamine. The program has no co-medication input, and how the two combine in one patient was not verified, so neither is applied: the curve is that of a patient taking neither. A patient taking either will have higher concentrations than plotted.

**Body size.** Yan's values are read as the 70 kg reference man's and take the library's [fat-free-mass scaling](help:models/fat-free-mass) for fixed published parameters; with the switch off, Yan's model, BMI step included, is reproduced exactly. The cohort's body size was not available, so no re-anchoring was possible. The two overlap: with the switch on, an obese patient's larger fat-free mass raises clearance while the BMI step lowers it. For a 120 kg, 170 cm, 50-year-old man (BMI 41.5) clearance is 28.9 × 1.22 × 0.706 = 24.9 L/h with the switch on and 20.4 L/h with it off. The BMI effect is the source's finding and is kept.

### No effect site

Only the plasma concentration is plotted. The antidepressant response follows the concentration by weeks, and no validated equation relates a mirtazapine concentration to remission, so there is no effect site and no MEAC. For context only: Sato and colleagues (2013) found 80 to 90% histamine H1 receptor occupancy by positron emission tomography after 15 mg, which accounts for the drug's sedation, but reported no half-maximal concentration.

### Typical concentrations

The shaded band, 30 to 80 ng/mL, with 50 as the typical value, is the AGNP consensus therapeutic reference range (Hiemke and colleagues, *Pharmacopsychiatry* 2018;51:9-62).

### Not used

Grasmäder and colleagues (2004), who found clearance about 26% lower in CYP2D6 intermediate metabolisers, and Brockmöller and colleagues (2007), who described the two enantiomers separately, are separate analyses; neither is mixed with Yan's estimates.

### Where to be careful

- The cohort was 105 Chinese adult inpatients; the child and infant in the table above are extrapolation.
- Fluvoxamine (CYP1A2) and paroxetine (CYP2D6) raise concentrations, and CYP3A4 inducers such as carbamazepine lower them; none is modelled.
