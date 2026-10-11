### Read this first: only the steady-state trough is valid

This model reproduces one thing: the **trough concentration at steady state**, 24 hours after a once-daily dose, of fluoxetine and of [norfluoxetine](help:drugs/norfluoxetine). Everything else on the curve is not physiological:

- **Steady state comes within a day.** Real fluoxetine takes about a month, and norfluoxetine longer.
- **After the last dose both curves fall to near zero within a day or two.** Real fluoxetine and norfluoxetine persist for weeks. Do not use this curve to judge a washout, for example the five weeks recommended before starting a monoamine oxidase inhibitor.
- **The swing between peak and trough** within a day is several-fold here; in patients it is small.

The reason is the data. Every concentration in the source was a trough drawn before the next dose, at steady state, and such data say how the trough scales with dose and sex but nothing about the time course. The fitted parameters give a fluoxetine half-life of 5.9 hours (5.1 in men) and a norfluoxetine half-life of 20 minutes, against 4 to 6 days and 4 to 16 days on the label.

### The model

Han and colleagues (*Pharmaceutics* 2025;17:1516, [doi:10.3390/pharmaceutics17121516](https://doi.org/10.3390/pharmaceutics17121516)) fitted 241 fluoxetine and 241 norfluoxetine concentrations from 198 Chinese psychiatric patients at Hunan Brain Hospital: median age 17 (12 to 56), median weight 59 kg, three quarters women, on 20 to 60 mg a day. Two connected one-compartment models, apparent oral parameters:

| Parameter | Typical value |
|---|---|
| Fluoxetine CL/F | 2.91 L/h in women; 16.5% higher in men |
| Fluoxetine V/F | 24.9 L |
| Norfluoxetine CL/F | 3.24 L/h |
| Norfluoxetine V/F | 1.52 L |
| ka | 0.3 /h, fixed from the literature |

All of fluoxetine's clearance forms norfluoxetine, mass for mass.

### The units are as printed

The short half-lives first looked like a units error, and fluoxetine was held back until that was settled. The paper's supplementary Table S4 lists the median troughs of its own simulations, and the printed parameters, read as litres, litres per hour and hours, reproduce them to within 3%: at 20 mg once daily, fluoxetine 83.8 ng/mL in women (Table S4: 86.0) and 57.1 in men (58.8); norfluoxetine 79.5 (78.9) and 63.8 (63.7). The same comparison settles that women are the reference group and that norfluoxetine is formed mass for mass.

### Dosing and the range

Fluoxetine is offered as **mg PO** and **mg PO qd** only. The AGNP consensus therapeutic reference range (Hiemke and colleagues, 2018), 120 to 500 ng/mL, is for **fluoxetine plus norfluoxetine** at trough; the two contribute about equally, so no band is drawn on either row. Add the two troughs to compare with it: at 20 mg a day about 163 ng/mL in women and 121 in men, at 40 mg about 326 and 242. The source's simulations put 30 mg a day in women and 40 mg in men as the doses most likely to reach the range.

### Covariates

Sex, as published. Age and weight were tested and not retained, within a narrow range of weights. The parameters take the library's [fat-free-mass scaling](help:models/fat-free-mass) for fixed published values, as the 70 kg reference man's, and are used exactly as published with the switch off. CYP2D6 and CYP2C19 genotype, which shift the balance between fluoxetine and norfluoxetine, were not studied. The cohort was adolescents and young adults; the infant and child in the table above are extrapolation.

### Alternatives not used

Panchaud and colleagues (2011, perinatal women; CL/F 8.42 L/h, V/F 690 L) and Wilens and colleagues (2002, children; CL/F 0.181 L/h/kg, V/F 37.4 L/kg) modelled fluoxetine alone and fitted their own populations; Han and colleagues found that both underpredicted the Chinese troughs. Panchaud's half-life, 57 hours, is closer to the label's, but the population is narrow and there is no norfluoxetine. See [Antidepressant models and their limits](help:models/antidepressants).
