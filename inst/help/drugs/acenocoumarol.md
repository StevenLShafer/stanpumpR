### Research model — not for clinical dosing

This is a research and teaching model. The plotted INR is **not** validated clinical decision support: it is not an acenocoumarol dose, a reversal plan, or a safe time to operate. Acenocoumarol is dosed by measured INR, and the response to a dose depends on the patient in ways no simulation captures.

### The model

Acenocoumarol is the **4'-nitro coumarin analogue of warfarin**, a vitamin-K antagonist widely used in Europe. It is modelled by Verstuyft and colleagues (*Clin Pharmacokinet* 2012;51(1):41-53), who studied a single 4 mg dose in 24 healthy White subjects and fitted a combined pharmacokinetic–pharmacodynamic model in Monolix.

Acenocoumarol is given as the racemate, but the INR is driven by the **S-enantiomer** alone (R-acenocoumarol was not a predictor of the response). So only S-acenocoumarol is modelled, taking **half of each racemic dose**. Its **apparent** (/F) disposition is two-compartment with first-order oral input and a lag:

- clearance 15.3 L/h,
- central volume 24.7 L, peripheral volume 52.5 L, intercompartmental clearance 4.85 L/h,
- absorption rate constant 0.936 /h, lag time 0.269 h.

The concentration is not plotted. It drives an **indirect-response model** of the INR (R/inr.R, type `"indirect"`). S-acenocoumarol inhibits the production of 1/INR:

- the inhibition is sigmoid, `E = C^γ / (C^γ + C50^γ)`, half-maximal at **0.0023 mg/L** with Hill coefficient **1.2**;
- `d(1/INR)/dt = kin·(1 − E) − kout·(1/INR)`, with `kin` 0.0269 /h and `kout` 0.0268 /h;
- the baseline 1/INR is `kin/kout`, so the plotted INR starts near 1.0 and rises as acenocoumarol accumulates.

### Why the INR, not a concentration

The INR is a delayed, non-linear turnover endpoint, so it is computed from the S-acenocoumarol curve after the engine has run rather than as an effect site. There is no MEAC, no band and no "time until threshold": the INR falls after acenocoumarol is stopped at the pace of clotting-factor turnover, which the effect-site recovery calculation does not describe.

### Covariates

- **CYP2C9**: a **\*3** allele carrier (any of \*1/\*3, \*2/\*3, \*3/\*3) has clearance × 0.7, so a \*3 carrier reaches a higher INR. \*1/\*2 and \*2/\*2 were not predictors in the source and behave as the wild type.
- **VKORC1**: the Patient Profile's −1639G>A field drives the C50 through its linkage with the source's 1173C>T genotype. **AA** is the sensitive reference (base C50, higher INR); **GG** or **GA** raises the C50 × 2.29 (less sensitive, lower INR).
- **Body weight** scales central volume — the model's own covariate, used in both positions of the [fat-free-mass switch](help:models/fat-free-mass).

### Where to be careful

The model was fitted to a single dose in healthy volunteers; the steady-state INR shown here comes from repeated dosing of that single-dose model, not from a maintenance-therapy fit. Acenocoumarol clears quickly, so its daily-dose INR swings through the day and its simulated steady-state peak is more sensitive to the dosing schedule than fluindione's; the paper does not state the maintenance dose or the definition behind its steady-state table, so the simulated value here is within roughly 20–30% of the paper's Table IV INRmax rather than an exact match. The model is not a dosing algorithm and does not represent vitamin K, drug interactions, or the between-patient variability that makes vitamin-K-antagonist dosing individual.

### Further reading

Not implemented here, but relevant to populations the single-dose adult source does not cover:

- Maagdenberg HM et al., *J Thromb Haemost* 2018;16(9):1732-1742 ([doi:10.1111/jth.14211](https://doi.org/10.1111/jth.14211)): the pediatric dosing algorithm from the Children Anticoagulation and Pharmacogenetics Study (175 Dutch children), where body surface area plus VKORC1, CYP2C9 and CYP2C18 explain about 62% of the dose requirement — the only substantial pediatric acenocoumarol evidence, and a pointer to body-size scaling.
- Delavenne X et al., *Fundam Clin Pharmacol* 2009;23(1):127-135 ([doi:10.1111/j.1472-8206.2008.00642.x](https://doi.org/10.1111/j.1472-8206.2008.00642.x)): an independent two-compartment, indirect-response model (with a body-weight covariate) that corroborates the structure used here.
