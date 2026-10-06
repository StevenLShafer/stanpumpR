### The model

Oxycodone's disposition is from Lamminsalo and colleagues (*Expert Opin Drug Deliv* 2019;16:649-656), a population model of intravenous oxycodone that also described its distribution into cerebrospinal fluid. The model uses the plasma part: a central volume of 90.2 L, a peripheral volume of 68.9 L, a clearance of 37.4 L/h (0.62 L/min) and an intercompartmental clearance of 206 L/h. There is no third compartment.

### Oral only

Although the disposition is intravenous, the dose table offers oxycodone only as **mg PO**. The absorption rate constant of 0.06/min (an absorption half-time of about 12 minutes) was chosen to put the plasma peak at 30 to 45 minutes, where the studies of Olesen and of Mandema place it, and the bioavailability of 0.5 is described in the code as the most consistent published value. See [the oral oxycodone scenario](scenario:oral-oxycodone) and [Oral, intramuscular and intranasal doses](help:models/absorption).

### Covariates

None in the published model. Under the default [fat-free-mass scaling](help:models/fat-free-mass) the fixed parameters are scaled to the patient's fat-free mass; unticking the box uses them as published, unscaled.

### Effect site

The time to peak effect is 60 minutes, taken from the time of peak cerebrospinal fluid concentration after an intravenous dose in Lamminsalo's study. This makes oxycodone's effect site, like morphine's, far slower than its plasma; an oral dose's effect peaks an hour or more after the plasma does.

### MEAC and typical concentrations

MEAC is 12 ng/mL, described in the code as a compromise between the lower values suggested by Mandema's analysis and the 45 to 50 ng/mL suggested by Kokki in 2012. The shaded band is 10 to 20 ng/mL.

### Active metabolite

Oxycodone forms **oxymorphone**, a potent metabolite, by CYP2D6, so a dose of oxycodone adds an [oxymorphone](help:drugs/oxymorphone) row. Formation is calibrated against the roughly 2 per cent plasma ratio Agema and colleagues observed, and the **CYP 2D6** field scales it. At present oxymorphone's own effect-site potency is provisional, so the metabolite's contribution to the opioid total is small; see [Active metabolites](help:models/metabolites).
