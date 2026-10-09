stanpumpR uses no numerical ODE solver. Every intravenous drug is a mammillary three-compartment model with an effect-site link, solved analytically once per patient and then evaluated at every time point as a sum of exponentials. The inhaled agents use a separate engine that advances the Gas Man model by matrix exponential in short steps: each step is exact with the uptake that couples the gases held at its value at the start of the step, and that coupling is updated from step to step, which leaves a small error that shrinks with the step (see [The inhaled-gas engine](help:models/gas-engine)). This page is the map; the pages that follow are the territory.

## From covariates to a curve

1. **Covariates to parameters.** The drug's model function (one R file per drug) turns age, weight, height and sex into volumes V1, V2, V3 and clearances CL1, CL2, CL3, plus the time to peak effect, tPeak. See [Covariates and body size](help:models/covariates).
2. **Parameters to rate constants.** k10 = CL1/V1, k12 = CL2/V1, k21 = CL2/V2, k13 = CL3/V1, k31 = CL3/V3.
3. **Rate constants to eigenvalues.** The characteristic cubic of the compartment system is solved for its three roots, the exponents α, β and γ that govern the three phases of the concentration curve. See [The three-compartment model](help:models/three-compartment).
4. **tPeak to ke0.** The effect-site rate constant is found by searching for the ke0 at which the effect-site concentration after a bolus peaks at the published time to peak effect. See [The effect site and ke0](help:models/effect-site).
5. **Coefficients.** For each route (bolus, infusion, oral, intramuscular, intranasal) the coefficients of the exponentials are precomputed.
6. **Doses to curves.** Each row of the dose table contributes its exponentials from its time onward; infusions are handled as rates with the state carried forward at every change. Oral, IM and IN doses given as amounts add a first-order absorption compartment; amiodarone's constant daily oral rate (mg/day PO) is handled as an infusion. See [Oral, intramuscular and intranasal doses](help:models/absorption).
7. **Events.** Where a model has different parameters during cardiopulmonary bypass, the state is converted at each event boundary and the simulation continues with the new parameters. See [Events that change the kinetics](help:models/pk-events).
8. **Output.** Plasma and effect-site concentrations on an even time grid, the peaks (for normalization), the percentage of MEAC, and the time until threshold.

## Active metabolites and targeting

- [Active metabolites](help:models/metabolites): a drug may form another drug, whose curve is convolved from the parent's and added to the metabolite's row. Codeine and tramadol are modelled as prodrugs with no effect site of their own.
- [Target-controlled infusion](help:tci): the *Plasma target* and *Effect site target* units run a simulated TCI pump, computing the infusion exactly from the same closed-form coefficients.
- [Scaling to fat-free mass](help:models/fat-free-mass): most models are scaled to the patient's fat-free mass by default, a switch away from the published total-weight scaling.

## Beyond concentrations

- [MEAC](help:models/meac) puts opioids on a common axis of analgesic effect.
- [Propofol-opioid interaction](help:models/interaction) turns two concentrations into a probability of response to laryngoscopy.
- [Time until threshold](help:models/recovery) answers, at every moment, "if I stopped now, how long until the concentration falls to the threshold?"
- [Normalization](help:models/normalization) rescales curves to their peaks for comparison.
- [Suggest Dosing](help:models/suggest-algorithm) searches for a regimen that reaches a target.

## The inhaled agents

The inhaled-gas engine is a separate model: four compartments (alveolar gas, vessel-rich group, muscle, fat) for each of several gases, coupled through the total uptake so that the concentration and second gas effects appear, with a breathing circuit in front. Its parameters and defaults are Gas Man®'s, and it has been validated against Gas Man. See [The inhaled-gas engine](help:models/gas-engine), [Where the gas engine differs from Gas Man](help:models/gas-differences) and [Opioid reduction of MAC](help:models/opioid-mac).

## Why closed form

The analytical solution is exact at every evaluated time, is fast enough to re-simulate every drug on every edit, and is the method STANPUMP used to drive infusion pumps in the 1990s. Its cost is a constraint: the model must be linear with piecewise-constant parameters. That rules out saturable kinetics and continuously varying physiology, which is why, for example, cardiac output in the gas model is held constant within a segment and the bypass events step the parameters rather than ramping them.
