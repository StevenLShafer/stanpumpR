### A research model, behind an opt-in

MDMA is one of stanpumpR's **illicit-drug research models**. These are hidden until you turn on **Show illicit drugs** above the dose table (off by default), and their names appear in red in the dose table. The curve is a *modelled plasma concentration* offered for research and teaching. It is not dosing advice, a safety threshold, or a prediction of anyone's experience. It describes pharmaceutical MDMA hydrochloride swallowed as a single dose in controlled studies, not material of unknown identity or content.

### The model

Huestis and colleagues fitted a one-compartment model with linear clearance to 2,012 plasma MDMA observations from 65 healthy adults across two phase I studies (Huestis MA, Smith WB, Leonowens C, et al. *CPT Pharmacometrics Syst Pharmacol* 2025;14(2):376-388). stanpumpR carries its apparent clearance of 41.5 L/h and apparent volume of 462 L, which give a half-life of 7.7 hours.

Because those are *apparent* parameters, clearance and volume divided by an unmeasured bioavailability, they predict oral concentrations correctly and intravenous ones wrong by 1/F. MDMA is therefore offered as an **oral** unit only, with bioavailability carried as 1 because the apparent scale already contains it.

### The dose is the hydrochloride

The source doses MDMA hydrochloride throughout, so its apparent parameters sit on the **salt** mass basis and the dose you enter is milligrams of MDMA hydrochloride. Entering a free-base mass against these parameters would under-predict exposure by the salt-to-base ratio. This matters because specifications of this drug often quote one basis and dose the other.

### The input is a zero-order window, discretised

The published input is not a plain first-order absorption. The dose fills the depot at a *constant rate* over 0.304 hours after a lag of 0.322 hours, and the depot then empties first-order into the circulation with a rate constant of 1.17 per hour.

stanpumpR's engine has first-order depots only, so that zero-order filling is **discretised** rather than approximated away: half the dose enters at the first quarter point of the window and half at the third, both absorbed at the published rate. That is the two-point midpoint rule for the source's input window, and it reproduces the exact solution's peak to 0.01% and the whole curve to within 0.53% — far inside the model's own uncertainty, whose clearance confidence interval alone spans about ±10%. The drug's test measures that agreement against a numerical evaluation of the exact solution, so it cannot drift unnoticed.

One consequence is visible on the plot: the peak falls at 2.86 hours rather than the 2.77 hours a single lagged depot would give, because the width of the input window shows up in the peak time.

### Single doses only

MDMA is a strong mechanism-based inhibitor of CYP2D6, the enzyme that clears it, so **its kinetics are not linear across repeated or split doses**. That is precisely why the source paired this linear population model with a separate physiologically based model for its split-dose simulations.

This model is calibrated on single doses and is linear. A second dose entered a few hours after the first will under-predict the exposure that follows it, and the error grows with the second dose. stanpumpR cannot represent the auto-inhibition.

### What is not modelled

- **MDA**, the active metabolite, is not plotted. The source did fit it, as a two-compartment model, but driven by a formation fraction **fixed at 0.1** rather than estimated, and fitted sequentially from each subject's individual parent estimates. A fixed, unestimated formation fraction is not a measured formation clearance, so no MDA curve is offered.
- There is **no effect site**, no MEAC and no band. No source reviewed for this model relates a concentration to heart rate, blood pressure, euphoria or toxicity in a form this engine could carry.

### Covariates

The source fitted its own size covariates, so stanpumpR carries those rather than the package's: apparent clearance with an exponent of 1.03 on weight, apparent volume with 0.885, both normalised to 70 kg. With *Adjust weight to fat-free mass* on they are evaluated at the patient's pharmacokinetic weight, and with it off at total body weight, reproducing the published scaling.

Two covariates the source also fitted are deliberately **not** carried. One is an empirical study effect, 36% lower apparent clearance in one of the two datasets, which the authors call an idiosyncrasy of that comparison; the parameters here are those of its reference individual in the other study. The other is food, which the app has no field for. The published food effect is large but acts on *timing* rather than exposure: a high-fat meal roughly halves the absorption rate and moved the median peak from 2.1 to 4.0 hours, while area under the curve and peak concentration were unchanged.

### Where to be careful

Read this as single-dose oral exposure for pharmaceutical MDMA hydrochloride in healthy adults. It is not a guide to a session, not a prediction for a split dose, and not an effect.
