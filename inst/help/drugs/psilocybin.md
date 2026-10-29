### A research model, behind an opt-in

Psilocybin is one of stanpumpR's **illicit-drug research models**. These are hidden until you turn on **Show illicit drugs** above the dose table (off by default), and their names appear in red in the dose table. The curve is a *modelled plasma concentration* offered for research and teaching. It is not dosing advice, a safety threshold, or a prediction of anyone's experience.

### The dose is psilocybin; the curve is psilocin

Psilocybin is a prodrug. It is dephosphorylated to **psilocin**, and it is unconjugated (free) plasma psilocin that was measured and modelled. So the dose you enter is milligrams of psilocybin, and the concentration plotted is free psilocin in ng/mL.

They are different molecules, so their masses differ by their molecular weights: psilocin 204.27 g/mol ÷ psilocybin 284.25 g/mol = **0.718628**. One milligram of psilocybin can yield at most 0.72 mg of psilocin. That factor is stoichiometry. It is *not* a bioavailability, and it is separate from absorption and dephosphorylation, neither of which is identified here.

### The model

Holze and colleagues fitted a one-compartment model with first-order absorption and a lag time to oral psilocybin in healthy participants (Holze F, Becker AM, Kolaczynska KE, Duthaler U, Liechti ME. *Clin Pharmacol Ther* 2023;113(4):822-831), reporting apparent clearance and volume on a **psilocin-equivalent** dose basis, from the assayed psilocybin content of each capsule.

stanpumpR implements the **nominal 15 mg arm** (assayed content 13.8 mg psilocybin, 9.9 mg psilocin equivalent): absorption rate 0.89 per hour, lag 0.30 hours, apparent clearance 172 L/h and apparent volume 448 L. Those give a half-life of 1.81 hours and a peak at 1.96 hours, reproducing the paper's published half-life of 1.8 hours (95% CI 1.7-2.0) and its statement that maximal concentrations were reached after an average of 2 hours. For that arm's assayed content the model peaks at about 11.6 ng/mL, against the published 11 ng/mL (95% CI 10-13).

### How the dose basis is converted

Because this model takes its dose in milligrams of *psilocybin* rather than psilocin equivalents, the input amount is 1 ÷ 0.718628 times the source's, so **both** apparent parameters are rescaled by that same factor: the apparent volume becomes 623.4 L and the apparent clearance 239.3 L/h. Their ratio is unchanged, so every predicted concentration and the half-life are unchanged. This is a change of dose basis, not of the model.

Because the parameters are apparent — clearance and volume over an unmeasured bioavailability — psilocybin is offered as an **oral** unit only, with bioavailability carried as 1. Applying a separate bioavailability on top of the apparent scale would discount the exposure twice.

### One dose arm, applied to every dose

The 25 mg and 30 mg arms were fitted separately and differ modestly (apparent clearance 199 and 175 L/h, apparent volume 413 and 450 L, absorption 0.77 and 0.75 per hour). stanpumpR carries one parameter set per drug, so larger doses are simulated here with the 15 mg arm's parameters and are predicted dose-proportionally.

Do not mix these numbers with other reports. An independent oral study (Brown and colleagues, 2017) fitted different apparent parameters together with a glucuronide compartment that was *inferred from the shape of the psilocin curve* rather than fitted to measured glucuronide concentrations, and a separate trial reports different peak concentrations.

### Content, not material

These parameters belong to synthetic psilocybin of assayed content. Mushroom material and teas vary widely in psilocybin and psilocin content, so a weight of material is not a dose; without an assay there is no quantitative prediction to make.

### No effect site

There is no effect site, no MEAC and no band. Madsen and colleagues' positron emission tomography study in eight volunteers found that plasma psilocin and cerebral 5-HT2A receptor occupancy conformed to a single-site binding model, with occupancies up to 72% — that is a direct function of plasma concentration rather than an equilibration delay. Subjective intensity is not a pharmacokinetic end point this library can plot. Holze's own effect-site parameters are in a supplement that was not read for this implementation, so no equilibration constant, EC50 or Emax is coded.

Intravenous psilocybin is not offered: it would need a conversion compartment before psilocin rather than a one-for-one substitution of psilocybin mass, and apparent oral parameters cannot predict an intravenous dose in any case. See [Research models of drugs of abuse](help:models/illicit-drugs).

### Covariates

Holze published no size covariate, so the apparent parameters are a reference adult's: unscaled with *Adjust weight to fat-free mass* off, and scaled with the Al-Sallami fat-free mass ratio (volume) and its 0.75 power (clearance) with it on. That scaling is a stanpumpR convention, not a published fit.
