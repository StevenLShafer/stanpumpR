### The model

Citalopram's kinetics are from Akil and colleagues (*J Pharmacokinet Pharmacodyn* 2016;43:99-109, [doi:10.1007/s10928-015-9457-6](https://doi.org/10.1007/s10928-015-9457-6)), a population analysis of the CitAD trial: 81 older patients with Alzheimer's disease and agitation, taking oral racemic citalopram. The two enantiomers were modelled separately, each with one compartment and a common absorption rate constant fixed at 1 /h, and each racemic dose delivers half of each:

| Enantiomer | CL/F | V/F |
|---|---|---|
| R-citalopram | 13 L/h (men) or 9.05 L/h (women) × (age/60)<sup>−0.822</sup> | 1830 L |
| S-citalopram (escitalopram) | 22.1 L/h (CYP2C19 normal, rapid, ultrarapid) or 16.3 L/h (intermediate, poor) × (age/60)<sup>−1.33</sup> × (weight/70)<sup>0.75</sup> | 1390 L |

In a 60-year-old, 70 kg man with normal CYP2C19 the half-lives are 97.6 hours for R- and 43.6 hours for S-citalopram, and 20 mg once daily gives an average total concentration of 10 / 24 / 13 + 10 / 24 / 22.1 mg/L = 50.9 ng/mL.

### What is plotted, and the reduction

The plot shows **total citalopram**, R plus S, which is what clinical assays report and what the therapeutic range refers to. The program carries one mammillary model per drug, so the two enantiomers are combined into one **two-compartment model of the racemate**. This is exact, not an approximation: the sum of two one-compartment responses with positive weights is a biexponential, and every such biexponential is the central-compartment response of a two-compartment model. With A = 0.5/V<sub>R</sub>, B = 0.5/V<sub>S</sub> and the elimination rate constants k<sub>R</sub> = CL<sub>R</sub>/V<sub>R</sub> and k<sub>S</sub> = CL<sub>S</sub>/V<sub>S</sub>:

```
V1  = 1 / (A + B)
k21 = (A kS + B kR) / (A + B)
k10 = kR kS / k21
k12 = kR + kS - k21 - k10
```

Because both enantiomers share the same absorption rate, the oral curve is exact as well. The model's two half-lives are the enantiomers' own, and its total clearance is the harmonic mean of the two, so the racemic exposure is D/2 ÷ CL<sub>R</sub> + D/2 ÷ CL<sub>S</sub>. The second compartment is therefore not a tissue: it is the slower enantiomer. When the two rate constants coincide the model is a single compartment. Enantiomer-specific concentrations are not shown.

### Oral only

No patient received intravenous citalopram, so every parameter is **apparent**, divided by the unmeasured bioavailability; only oral units are offered (**mg PO**, **mg PO qd**, **mg PO bid**), and the bioavailability is carried as 1.

### Covariates

- **CYP2C19** changes S-citalopram's clearance only. The source estimated two groups: normal and rapid metabolisers (22.1 L/h) and intermediate and poor metabolisers (16.3 L/h). Normal, rapid and ultrarapid take the first value; intermediate and poor take the second, which probably overstates a true poor metaboliser's clearance. The CYP2C19 table above shows the clearance of the combined model, CL1, which is the total racemic clearance: 16.3 against 22.1 L/h for S-citalopram moves it only from 16.4 to 14.5 L/h, because R-citalopram is unaffected.
- **Age and sex** are used as published, centred on 60 years. The cohort was older adults; the power functions of age are extrapolated in younger adults and meaningless in children, where they raise clearance enormously. The child and infant in the table above are shown only because every model is tabulated there.
- **Body size.** S-citalopram's clearance has its own weight term. With the [fat-free-mass](help:models/fat-free-mass) switch on it is evaluated at the pharmacokinetic weight (70 kg × the patient's fat-free mass over the reference man's), and the volumes and R-citalopram's clearance are scaled by the library factors; with it off, total body weight enters the weight term and the other parameters are exactly as published.

### Not modelled

- **The desmethyl metabolites** (R- and S-desmethylcitalopram), which the source modelled under assumptions of complete conversion and equal volumes.
- **Variability.** The scale of the source's between-patient variability table could not be settled, so the curve is the typical patient's.
- **Effect.** The antidepressant effect develops over weeks and has no equilibration rate constant, so there is no effect site. For context, Meyer and colleagues (*Am J Psychiatry* 2004) put the concentration of total citalopram giving half-maximal serotonin-transporter occupancy at 11.7 ng/mL; occupancy is not plotted. Friberg and colleagues' QT model (2006) was fitted to overdoses and is not used. CitAD's relationship between exposure and agitation does not predict remission of depression.

The shaded band is the AGNP consensus therapeutic reference range for citalopram, 50 to 110 ng/mL (Hiemke and colleagues, *Pharmacopsychiatry* 2018;51:9-62), with 80 ng/mL as the typical line. For the S-enantiomer given alone, see [escitalopram](help:drugs/escitalopram), whose model comes from a different population and is not the S-component of this one.

The model was implemented by Claude Code at the request of Steven L. Shafer, from the published parameters, and the reduction is checked in the test suite against the two enantiomer curves.
