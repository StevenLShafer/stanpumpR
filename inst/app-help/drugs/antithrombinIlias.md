### The model

Ilias and colleagues (*Intensive Care Med* 2000) gave a plasma-derived antithrombin III concentrate (Kybernin P) to 33 adults with severe sepsis in eleven European intensive care units: 6000 IU, then either 1000 IU every 4 hours or 250 IU/hour for 4 days. The patients were 18 to 85 years old (median 64), 23 men and 10 women, most of them after surgery. Each patient's activity, less the activity before the first dose, was fitted to one compartment. Table 3 gives the medians of the 32 fits, standardised to 70 kg:

| | Median | Mean | Range |
|---|---|---|---|
| Volume | 4.5 L | 4.5 L | 2.4-6.5 L |
| Clearance | 2.7 mL/min | 3.4 mL/min | 1.4-10.1 mL/min |
| Half-life | 18.6 h | 18.9 h | 5.1-37.4 h |
| Rise per IU/kg | 1.70 IU/dL | 1.75 IU/dL | 1.14-2.80 IU/dL |

stanpumpR uses the medians, scaled in proportion to weight, as the paper standardised them. They agree with each other: they give a half-life of 19.3 hours. They also give a rise of 1.56 IU/dL for each IU/kg, against the paper's 1.70, which it measured as the largest rise within 2 hours of the first dose.

**stanpumpR uses this model for adults** (19 years and over), through the [antithrombin](help:drugs/antithrombin) group (Steven L. Shafer, 2026-11-26). Children get [Moffett 2017](help:drugs/antithrombinMoffett).

**This is a model of sepsis.** Antithrombin is consumed in sepsis, and the half-life, 19 hours, is a fraction of the 3.8 days that the Thrombate III label gives in hereditary deficiency (8 patients, functional assay). In a patient who is not septic, the activity will fall more slowly than drawn. The product, Kybernin P, is not sold in the US. Like Thrombate III it is plasma-derived human antithrombin III, dosed in the same international units.

### What is plotted

Antithrombin activity in **IU/dL** (% of normal): the patient's own plus the activity infused. The patient's own is the Patient Profile's **Antithrombin baseline**, shown while antithrombin is in the dose table. Left blank, it is 63 IU/dL, the source's median. The baseline is only plotted. The source subtracted it before fitting and has no covariate on it, so a lower baseline does not change the rise for a dose.

The band is the label's target, 80 to 120% of normal (IU/dL).

### Covariates

- **Weight**, on both parameters in proportion, as the source standardised them; with the fat-free-mass switch on, the pharmacokinetic weight.
- **Heparin**: eight patients took heparin, with no effect on the kinetics (the paper's Results).
- **Pregnancy**: not studied; nothing is applied.

### Doses

`IU/kg` or `IU` as a dose, and `IU/kg/hr` or `IU/hr` as an infusion.

### Not modelled

The patient's own antithrombin is held constant. Its production, and its consumption in sepsis, are not drawn; the authors saw the activity at 7 days sometimes above the fit, which they read as recovering synthesis.
