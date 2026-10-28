### Research model — not for clinical dosing

This is a research and teaching model. The plotted INR is **not** validated clinical decision support: it is not a warfarin dose, a reversal plan, or a safe time to operate. Warfarin is dosed by measured INR, and the recovery of the INR after a dose is held or vitamin K is given depends on the patient in ways no simulation captures.

### The model

Warfarin is modelled by Xue and colleagues (*Br J Clin Pharmacol* 2017;83:823-835, with its [erratum](https://doi.org/10.1111/bcp.13325), 2017;83:1602, which corrects the R-warfarin term of Equation 2 to the form used here), who fitted the two enantiomers separately in 264 cardiac-surgery patients. A racemic dose is split equally between **S-** and **R-warfarin**, each a one-compartment model with first-order absorption:

- S-warfarin: clearance 0.224 L/h, volume 24.0 L,
- R-warfarin: clearance 0.124 L/h, volume 16.2 L,
- absorption rate constant 1.80 /h for both.

The enantiomer concentrations are not plotted or added. They drive the synthesis of the **prothrombin complex** (activity PCA, a fraction of normal): S-warfarin inhibits synthesis through a sigmoid model (half-maximal at 0.378 mg/L, Hill 2.72), R-warfarin competes with it (raising the apparent half-maximal concentration, with a competition constant of 2.387 mg/L), and PCA turns over with a half-life of 11.4 hours from a baseline of 0.979. The plotted **INR is 1 / PCA**, so it starts near 1.02 and rises as warfarin accumulates. This is the paper's theory-based relationship, which its authors found as good in vivo as an empirical calibration.

### Why the INR, not a concentration

The INR is a delayed, non-linear function of two concentrations, so it is computed from the two enantiomer curves after the engines have run rather than as an effect site. There is no MEAC and no "time until threshold": the INR falls after warfarin is stopped at the pace of clotting-factor synthesis, which the effect-site recovery calculation does not describe. The shaded band is the INR target, 2.0 to 3.0 with 2.5 typical, the Coumadin label's target for venous thromboembolism and atrial fibrillation (section 2.2; 2.5 to 3.5 for some mechanical valves) and the CHEST guideline's (Holbrook and colleagues, *Chest* 2012;141(2 Suppl):e152S-e184S, https://doi.org/10.1378/chest.11-2295); it is a reference for reading the curve, not a dose recommendation.

### Covariates

Clearance scales with fat-free mass (as FFM^0.75) and volume with fat-free mass directly — the model's own covariate, used in both positions of the [fat-free-mass switch](help:models/fat-free-mass). The source used the Janmahasatian fat-free mass, which this model keeps, referenced to the 56.1 kg of its standard 70 kg, 176 cm man. Women have larger enantiomer volumes (×1.33 S, ×1.22 R), applied by sex. The source's other covariates — amiodarone and fluconazole (enzyme inhibition), CYP2C9 \*1/\*3, a bioprosthetic valve, and the VKORC1 and CYP4F2 genotypes that lower the S-warfarin potency — have no field in the app, so the app simulates a CYP2C9 \*1/\*1, VKORC1 non-AA, CYP4F2 TT patient on neither drug with a mechanical valve. They are arguments to the model function for scripting.

### Where to be careful

This is a cardiac-surgery population with an INR target of 1.5–2.5, and the R-warfarin competition term is specific to this study (the authors call the R effect minor). The model is not a dosing algorithm, and it does not represent vitamin K, bridging heparin, or the between-patient variability that makes warfarin dosing individual.
