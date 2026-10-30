### Research model — not for clinical dosing

This is a research and teaching model. The plotted INR is **not** validated clinical decision support: it is not a fluindione dose, a reversal plan, or a safe time to operate. Fluindione is dosed by measured INR, and the response to a dose depends on the patient in ways no simulation captures.

### The model

Fluindione is an **indane-dione vitamin-K antagonist**, the most prescribed oral anticoagulant in France. It is modelled by Verstuyft and colleagues (*Clin Pharmacokinet* 2012;51(1):41-53), who studied a single 20 mg dose in 24 healthy White subjects and fitted a combined pharmacokinetic–pharmacodynamic model in Monolix.

The disposition is **apparent** (/F) two-compartment with first-order oral input and no lag:

- clearance 0.17 L/h,
- central volume 5.19 L, peripheral volume 2.77 L, intercompartmental clearance 0.173 L/h,
- absorption rate constant 1.83 /h.

The concentration is not plotted. It drives an **indirect-response model** of the INR (R/inr.R, type `"indirect"`). Fluindione inhibits the production of 1/INR:

- the inhibition is sigmoid, `E = C^γ / (C^γ + C50^γ)`, half-maximal at **3.63 mg/L** with Hill coefficient **2.32**;
- `d(1/INR)/dt = kin·(1 − E) − kout·(1/INR)`, with `kin` 0.0385 /h and `kout` 0.0391 /h;
- the baseline 1/INR is `kin/kout`, so the plotted INR starts near 1.0 and rises as fluindione accumulates.

### Why the INR, not a concentration

The INR is a delayed, non-linear turnover endpoint, so it is computed from the fluindione curve after the engine has run rather than as an effect site. There is no MEAC and no "time until threshold": the INR falls after fluindione is stopped at the pace of clotting-factor turnover, which the effect-site recovery calculation does not describe. The shaded band is the INR target, 2.0 to 3.0 with 2.5 typical, the CHEST guideline's target for every vitamin K antagonist (Holbrook and colleagues, *Chest* 2012;141(2 Suppl):e152S-e184S, https://doi.org/10.1378/chest.11-2295); its own product information has not been read; it is a reference for reading the curve, not a dose recommendation.

### Covariates

- **CYP2C9**: a **\*3** allele carrier (any of \*1/\*3, \*2/\*3, \*3/\*3) has clearance × 0.637, so a \*3 carrier reaches a higher INR. \*1/\*2 and \*2/\*2 were not predictors in the source and behave as the wild type.
- **VKORC1**: the Patient Profile's −1639G>A field drives the C50 through its linkage with the source's 1173C>T genotype. **AA** is the sensitive reference (base C50, higher INR) and the field's default, so that an unentered genotype shows the larger INR rise; **GG** or **GA** raises the C50 × 1.30 (less sensitive, lower INR). See [VKORC1 and ancestry](help:patient-profile) for the reasoning and for the genotype to expect in a patient who has not been genotyped.
- **Body weight** scales clearance (as weight^0.75) and central volume — the model's own covariate, used in both positions of the [fat-free-mass switch](help:models/fat-free-mass).
- The source's **CYP1A2** (paraxanthine/caffeine ratio) and **smoking** effects have no field in the app, so the ratio is fixed at the population median and those terms drop out.

### Where to be careful

The model was fitted to a single dose in healthy volunteers; the steady-state INR shown here comes from repeated dosing of that single-dose model, not from a maintenance-therapy fit. Fluindione's low clearance lets it accumulate, so its simulated steady-state INR is close to the paper's Table IV values, but the model is not a dosing algorithm and does not represent vitamin K, drug interactions, or the between-patient variability that makes vitamin-K-antagonist dosing individual.

### Further reading

These are not implemented here — the plotted curve is the Verstuyft 2012 healthy-volunteer model — but they inform how the drug behaves in patients and in the elderly, where it is actually used:

- Comets E et al., *Clin Pharmacol Ther* 2012;91(5):777-786 ([doi:10.1038/clpt.2011.309](https://doi.org/10.1038/clpt.2011.309)): the PREPA study of 131 octogenarian inpatients, where clearance falls with age and exposure rises substantially, arguing for a lower dose — the age effect the single-dose healthy-volunteer source does not carry.
- Mentré F et al., *Clin Pharmacol Ther* 1998;63(1):64-78 ([doi:10.1016/S0009-9236(98)90122-9](https://doi.org/10.1016/S0009-9236(98)90122-9)): the original patient-based, multiple-dose indirect-response PK/PD model, a check of the structure in real patients rather than volunteers.
