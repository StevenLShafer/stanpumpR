The **Bayesian Updating** panel in the left-hand sidebar adjusts a drug's pharmacokinetics to the patient from concentrations measured during care. It is meant for drugs that are assayed over the course of treatment, such as warfarin (by its INR), amiodarone, phenytoin, digoxin, vancomycin and the aminoglycosides, the antiseizure drugs and the immunosuppressants. It works for every drug in the dose table except the inhaled agents.

## Entering the measurements

Open the panel with its arrow. The table has three columns:

- **Drug**: one of the drugs in the dose table.
- **Time**: when the sample was drawn, typed as the dose table's times are, in the **Time units** and **Time Display** of the [Time](help:time-display) settings (hours, days, or a clock time in Actual time). When the time units change, the table's times are converted with the dose table's.
- **Conc.**: the measured value, in the units of the drug's own plot, which are listed under the table. For warfarin this is the **INR**, and for heparin on bypass the **activated clotting time** in seconds, because those are what their panels plot.

A row takes effect as soon as it is complete; there is no Apply button. Right-click a row (or press Shift+F10) to insert or remove rows. **Clear measurements** empties the table. A measurement at or before the first dose of the drug cannot be explained by it and is set aside, with a note under the table.

## What is drawn

Each updated drug is drawn with the patient's own pharmacokinetics. The model's population prediction stays in the same panel as a second, lighter or darker line, named in the legend as, for example, *warfarin (population)*. The measurements are drawn as points in the drug's colour. Everything that reads the drug's pharmacokinetics uses the updated drug: the curve, the time until threshold, the peripheral compartments, the TCI rates, the hover readout and [Suggest Dosing](help:models/suggest-algorithm). Suggest Dosing can therefore aim a new regimen at the updated patient.

Under the table, each updated drug's estimate is written out, for example *amiodarone: clearance x 0.616 (−38%), volume x 0.676 (−32%); 2 measurements*. A small table follows with each measured value beside the model's prediction and the updated one.

## What is estimated

Two numbers per drug: the patient's **clearance** and **volume**, each as a multiple of the model's. This is the update the original STANPUMP made (its BAYES.C, written by Pierre Maitre and Steven Shafer; see [History](help:history)), restated for every model in the library:

- The **volume** factor multiplies every volume and every intercompartmental clearance. The rate constants of distribution (k12, k21, k13, k31) are therefore the model's: the patient is the model's patient, larger or smaller, with the same shape of distribution. STANPUMP likewise estimated CL and V1 and held k12, k21, k13 and k31 fixed.
- The **clearance** factor multiplies the elimination clearance. For a drug with saturable elimination (phenytoin), it multiplies Vmax, with Km, a concentration, left as the model has it. This is the classic phenytoin update.
- A metabolite's rate of formation moves with the parent's elimination, so the share of the parent's elimination that forms the metabolite is the model's. The metabolite's own pharmacokinetics are not updated.
- Warfarin's two enantiomers take the same two factors, so their ratio is the model's.
- Absorption, bioavailability, lag times, the effect site (ke0 or the time to peak effect), and the links from concentration to INR or ACT are left as the model has them.

**Only clearance has a direct physiological meaning** (see [How stanpumpR computes](help:models/overview)). With steady-state troughs alone, the measurements say almost nothing about the volume, and the estimate keeps it near the model's. The time course after a change in dose, or a peak and a trough, is what moves the volume.

## The two variabilities

The estimate is the **maximum a posteriori** (MAP) one: the clearance and volume that best balance agreement with the measurements against agreement with the model. Two sliders set the balance. Both are coefficients of variation (CV) of **log-normal** distributions, because neither a pharmacokinetic parameter nor a concentration can be negative. On a log-normal scale, half and twice the expected value are equally far from it.

- **PK variability** (default 25%) is how far the patient's clearance and volume may plausibly be from the model's.
- **Residual variability** (default 5%) is how far a measurement may plausibly be from the patient's own predicted concentration. It includes the assay error, and also errors in the recorded dose and sample times and in the model itself. In population modelling it typically comes down to about 5%. Raise it when the sample times are uncertain.

With *eta* the logarithm of each factor, the estimate minimises

```
sum(eta^2) / omega^2  +  sum((log measured - log predicted)^2) / sigma^2
```

where omega and sigma are the two variabilities on the log scale: a CV of *c* is `sqrt(log(1 + c^2))`, 0.246 for 25% and 0.050 for 5%. This is minus twice the logarithm of the posterior probability, less a constant, and the function Vozeh used for lidocaine (Vozeh 1985), which BAYES.C minimised.

Only the **ratio** of the two variabilities changes the estimate: doubling both gives the same clearance and volume. At 25% and 5%, each measurement weighs about 25 times as much as the prior, so two or three measurements bring the curve close to the points. A larger residual variability or a smaller PK variability keeps the estimate closer to the model.

The search runs from the model's values, within 20 times the model's clearance and volume either way. A note under the table says when the estimate reached that limit, which usually means a value in the wrong units.

## How it is computed

The prediction compared with each measurement is exactly what the plot draws at that time. It is the INR for warfarin. For a drug that is also formed from a dosed parent, it is the drug given plus the drug formed. With the [antiseizure interactions](help:models/antiseizure-interactions) on, it is the drug as they change it. When several drugs have measurements, they are updated one after another, in the dose table's order, each with the drugs before it already updated. The search (L-BFGS-B, from R's `optim()`) repeats the simulation for each trial clearance and volume. That takes a second or two for most drugs and longer for phenytoin, whose saturable elimination is integrated numerically. An estimate is kept until the doses, the measurements, the patient or the variabilities change.

The calculation was checked against **mapbayr** (Le Louedec 2021), the R package for MAP Bayesian estimation on mrgsolve models. The test was vancomycin, a two-compartment model, given as 1 g over an hour every 12 hours, with three levels. Given the same model, priors and log-normal residual error, mapbayr 0.10.2 estimated clearance x 0.6111885 and volume x 0.7970002, and stanpumpR estimated 0.6111886 and 0.7970008. The test suite holds stanpumpR to those values.

## Cautions

- **A point estimate.** The update gives the single most probable clearance and volume. It does not show how uncertain they are, and with one or two measurements they can be quite uncertain.
- **Warfarin.** The INR depends on the patient's sensitivity to warfarin (VKORC1 genotype, vitamin K intake, liver function) as well as on how fast warfarin is cleared. Only clearance and volume are updated, so the update puts all of the difference between the patient's INR and the model's into clearance. The updated clearance is therefore an *effective* one. It predicts the INR of a changed dose well near the doses measured, and less well far from them.
- **The analyte.** Enter what the drug's panel plots. Amiodarone's panel plots amiodarone, not desethylamiodarone. Phenytoin's plots the total, not the unbound, concentration. An anticoagulant's plots the activity or INR its model gives.
- **The time of the sample.** A trough is drawn before the dose. Enter the time it was drawn, not the time of the dose that follows it.
- **The companion models** (the [Minto and Schnider](help:models/minto-schnider) and [Hamberg](help:models/hamberg) lines) are not updated. They remain the population predictions of their own models.
- **Saved links.** A [bookmark](help:sharing) keeps the measurements with the dose table.

stanpumpR is an advisory simulator intended to facilitate perioperative patient care, teaching and research. It does not control drug delivery and has not been reviewed or cleared by any regulatory agency. How its predictions apply to a patient is a matter of clinical judgement (see [Cautions](help:cautions)).

## References

- Vozeh S, Uematsu T, Hauf GF, Follath F. Performance of Bayesian feedback to forecast lidocaine serum concentration: evaluation of the prediction error and the prediction interval. *J Pharmacokinet Biopharm* 1985;13:203-212. [doi:10.1007/BF01059399](https://doi.org/10.1007/BF01059399)
- Maitre PO, Stanski DR. Bayesian forecasting improves the prediction of intraoperative plasma concentrations of alfentanil. *Anesthesiology* 1988;69:652-659. [doi:10.1097/00000542-198811000-00004](https://doi.org/10.1097/00000542-198811000-00004)
- Le Louedec F, Puisset F, Thomas F, Chatelut E, White-Koning M. Easy and reliable maximum a posteriori Bayesian estimation of pharmacokinetic parameters with the open-source R package mapbayr. *CPT Pharmacometrics Syst Pharmacol* 2021;10:1208-1220. [doi:10.1002/psp4.12689](https://doi.org/10.1002/psp4.12689)
