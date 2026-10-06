stanpumpR predicts drug concentrations. You describe a patient and a dosing regimen, and it plots the plasma and effect-site concentrations those doses would be expected to produce, using pharmacokinetic models from the peer-reviewed literature. It is meant to make pharmacokinetics accessible: for patient care, for teaching, and for research.

**stanpumpR does not control drug delivery and does not measure anything.** Every curve is a model's prediction for a typical patient with the covariates you entered. Real patients vary, sometimes a great deal, around those predictions. Please read [Cautions](help:cautions) once.

## Where to start

- **New here?** Take [the five-minute tour](help:quick-start). It covers everything you need to produce your first simulation.
- **Teaching or learning?** The [teaching scenarios](help:scenarios/index) are complete simulations, each making one pharmacokinetic point. Open one and press *Load* to put it in the simulator.
- **Which model is this?** Every drug has a page under [Drug library](help:drugs/index) recording the model the program actually computes: its parameters at reference patients, its citation, its units, and notes on where it came from and where it should not be trusted.
- **How does it work?** [Models and methods](help:models/overview) explains the three-compartment model, the effect site, the interaction surface, the inhaled-gas engine, and the rest.

## What is on the screen

| Region | What it does | Help page |
|---|---|---|
| Left sidebar | Patient covariates, graph options, additional plots, email | [Patient profile](help:patient-profile), [Graph options](help:graph-options) |
| Plot | Predicted concentrations; hover, click and double-click act on it | [Reading the plot](help:reading-the-plot) |
| Dose table | Where you describe the regimen | [The dose table](help:dose-table) |
| Time card | Elapsed minutes or clock time | [Time display](help:time-display) |
| References | The citation for every drug being simulated | [Bibliography](help:references) |
| Settings menu | The editable drug library and recovery thresholds | [Drug Library and Drug Thresholds](help:drug-library) |

## A note on scope

The library holds twenty intravenous drugs and the inhaled anesthetics. Several features are visible but not yet active, and several more are in development; see [In development](help:in-development) for an honest list.

stanpumpR is a collaborative research project. If you would like to add a drug, a data set or an algorithm, see [Contributing](help:contributing).
