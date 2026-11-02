## Warfarin: the Hamberg genotype model beside Xue

Warfarin is plotted by the library's own model, Xue and colleagues' cardiac-surgery model (2017), which turns the S- and R-warfarin concentrations into the INR through a prothrombin-complex turnover (see [warfarin](help:drugs/warfarin)). A **Graph Options** checkbox, **Hamberg**, draws a second INR line beside it: the model of Hamberg and colleagues (*Clin Pharmacol Ther* 2007;81:529-538), the model built to show how **age**, **CYP2C9** and **VKORC1** genotype shape the dose a patient needs. The box is offered whenever warfarin is in the dose table and is off by default; tick it to draw the second line.

## Why two models

The two models answer different questions. Xue's was fitted to cardiac-surgery patients and carries the covariates that matter there. Hamberg's was built from 150 patients specifically to separate the three predictors that drive the roughly twenty-fold range in warfarin dose requirement, and it predicts the whole time course from the first dose to steady state — so it shows how patients who look alike in the first week can diverge by steady state. Seeing both lines move as the Patient Profile changes is the point.

## The pharmacokinetics

Hamberg fitted S- and R-warfarin separately: S-warfarin as a two-compartment model and R-warfarin as a one-compartment model, each with first-order oral absorption (the absorption rate constant fixed at 2 /h, as the paper could not estimate it from its sampling). The anticoagulant effect is driven by **S-warfarin only** — R-warfarin was not a significant predictor of the INR; it is carried as the second enantiomer system the INR machinery expects, and is not drawn.

- **CYP2C9** genotype and **age** act on S-warfarin clearance. Relative to a 71-year-old with the wild-type `*1/*1` genotype (clearance 0.314 L/h), clearance falls by 31% for `*1/*2`, 45% for `*1/*3`, 72% for `*2/*2`, 69% for `*2/*3` and 85% for `*3/*3`, and by about 0.9% per year of age. A `*3/*3` patient clears S-warfarin about seven times more slowly.
- Age also lowers R-warfarin clearance by about 1% per year.
- **Body size.** Hamberg found neither weight nor sex significant and published no size term. As for every library model without one, the volumes and clearances are scaled to the patient's fat-free mass relative to the 70 kg, 170 cm reference man while *Adjust weight to fat-free mass* is ticked, and used as published when it is not. The library's own warfarin model carries its own fat-free-mass covariate, so with the switch on the two INR lines treat body size alike and the gap between them is the genotype.

## The pharmacodynamics

The delay between S-warfarin exposure and the INR — the INR does not fall until the vitamin-K-dependent clotting factors already in the blood are cleared — is described by **two parallel chains of transit compartments**, a chain of six compartments with a mean transit time of 11.6 hours and a single compartment with a mean transit time of 120 hours. These stand in for the different half-lives of the clotting factors. The transit rate constant of each chain is the reciprocal of its mean transit time (the paper's own convention, whatever the number of compartments). S-warfarin inhibits the turnover of both chains through one inhibitory Emax relationship (maximum effect fixed at 1, a shape factor of 0.424), and the INR is built from the product of the two chain outputs (A₆ from the six-compartment chain, A₇ from the single compartment):

> INR = baseline + 20 × (1 − A₆·A₇)^3.61

with the maximum INR rise fixed at 20 (the data could not estimate it) and the exponent 3.61 making the INR change faster when inhibition is already deep.

## VKORC1 — warfarin sensitivity

**VKORC1** genotype sets the sensitivity, through the half-maximal S-warfarin concentration: 4.61 mg/L for the GG genotype, 3.02 for GA and 2.20 for AA. The AA patient is about twice as sensitive as the GG patient. Set the VKORC1 genotype in the [Patient Profile](help:patient-profile). The library's Xue model reads the same field, but only for the AA genotype, where it lowers the S-warfarin C50 (GG and GA are the same to it); so the AA setting moves both INR lines, while GA moves only the Hamberg line. **AA** is the field's default, the most sensitive genotype in both models, so that an unentered genotype shows the larger INR rise; see [VKORC1 and ancestry](help:patient-profile) for why, and for which genotype to expect in which patient.

## Research model — not for dosing

Like the library's own warfarin model, this is a research and teaching model. The paper's authors caution that it does not implement the full underlying coagulation system and that detailed mechanistic interpretation should be made with care; it is **not** validated clinical decision support, and a simulated INR is not a dose recommendation or a bleeding risk.
