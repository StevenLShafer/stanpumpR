### The model

Retout and colleagues' population analysis (*Clin Pharmacokinet* 2020) pooled 389 persons with haemophilia A, with or without factor VIII inhibitors, from the HAVEN 1 to 4 trials and a Japanese phase I/II study: aged 1.2 to 77 years (median 30), weighing 9.5 to 156 kg (median 69), given 0.3 to 6 mg/kg subcutaneously every week, every two weeks or every four weeks. The model has one compartment with first-order absorption, on the apparent scale:

- apparent clearance 0.272 L/day × (weight/70)^0.911 × (1 − 0.0157 × (albumin − 45 g/L));
- apparent volume 10.4 L × weight/70;
- absorption rate 0.536/day (a half-time of 1.3 days);
- a relative bioavailability of 1 up to 30 years, falling by 0.651% a year after that.

The distribution phase an IgG antibody shows after an intravenous dose is hidden under the slow subcutaneous absorption, so the model has one compartment, and only the subcutaneous route is offered. The elimination half-life is 26.5 days. The label's absolute bioavailability (80 to 93% at 1 mg/kg) is inside the apparent parameters and is not applied again.

For the paper's reference person (70 kg, albumin 45 g/L, 30 years) on 1.5 mg/kg weekly, the model gives an average steady-state concentration of 55.1 mcg/mL, the paper's value. The label's mean steady-state troughs are 51.2, 46.9 and 38.5 mcg/mL on 1.5 mg/kg weekly, 3 mg/kg every two weeks and 6 mg/kg every four weeks.

### Covariates

- **Weight**, on clearance and volume, as fitted. With the fat-free-mass switch on, the weight in these terms is the pharmacokinetic weight, as for every model with its own weight covariate. Because both scale almost in proportion to weight, dosing in mg/kg gives nearly the same concentrations at every size.
- **Serum albumin**, from the Patient Profile, on clearance: emicizumab and albumin are both recycled by the neonatal Fc receptor. Blank is the reference 45 g/L (4.5 g/dL); an entry is held within the 16.8 to 56.6 g/L the study included. Albumin 33 g/L lowers the steady state 16%, 57 g/L raises it 23%.
- **Age**, on bioavailability above 30 years: a 77-year-old reaches 31% less than a 30-year-old. The effect is held at 77, the oldest person studied. The label says the pharmacokinetics are "not influenced by age"; this later population analysis found the effect, and the model applies it.

**Not applied**: black race lowered the volume 21.5% in the paper, changing the steady state by less than 2%; the Patient Profile has no race field. **Infants under 1 year** are outside the study: the label reports that concentrations predicted below 6 months of age are 19 to 33% lower, from a maturation model not yet published in a form this library can use, so the curve for an infant is likely too high.

### Product and doses

Hemlibra (emicizumab-kxwh), `mg/kg SC` and `mg/kg SC qwk`, and `mg SC` for a dose written as a vial amount. The label's loading dose is 3 mg/kg weekly for four weeks, then 1.5 mg/kg weekly, 3 mg/kg every two weeks or 6 mg/kg every four weeks. The library has no every-two-weeks or every-four-weeks schedule, so enter those doses one by one.

### What is not plotted

Concentration only, in mcg/mL. Emicizumab bridges activated factor IX and factor X in place of activated factor VIII, but there is no established relation between its concentration and a factor VIII-equivalent activity, and none is drawn. The paper's bleeding rate was nearly flat across the concentrations the approved regimens reach, and no therapeutic range is established. Emicizumab interferes with clotting-based assays (the aPTT, one-stage factor VIII activity and the clotting Bethesda assay); the label says the effect may persist for up to 6 months after the last dose, and a chromogenic assay with bovine factors measures factor VIII activity without it. Its boxed warning concerns thrombotic microangiopathy and thrombosis when activated prothrombin complex concentrate is given on top of it.
