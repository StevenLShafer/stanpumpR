### The model

Midazolam's parameters are from Bührer and colleagues (*Clin Pharmacol Ther* 1990;48:544-554), who gave five healthy men 3.75 to 25 mg at 5 mg/min. This is the set STANPUMP originally used for midazolam target-controlled infusion. Zomorodi and colleagues (*Anesthesiology* 1998;89:1418-1429) drove their infusions after coronary bypass surgery with it and print it in their Table 3; later versions of STANPUMP carried the kinetics Zomorodi fitted in those patients instead. The model is a fixed three-compartment model with volumes of 3.3, 17.6 and 96.8 L and clearances of 0.535, 2.01 and 0.832 L/min. Earlier versions of this page credited Mould and colleagues (1995), whose paper reports only noncompartmental kinetics.

### Covariates

None. The parameters are for a typical adult. Midazolam's clearance is known to fall with age and with CYP3A4 inhibition, and its volume to rise with obesity; none of this is in the model.

### Effect site

The time to peak effect is 4 minutes. Its source is not recorded. With these kinetics, Bührer's own EEG equilibration half-time of 4.8 minutes (*Clin Pharmacol Ther* 1990;48:555-567) would put the peak at about 2.7 minutes, and Mould's 3.2 minutes at about 2.2 minutes.

### Typical concentrations

The shaded band, 0.04 to 0.12 mcg/mL, is a sedation range. Concentrations needed for hypnosis as a sole agent are several times higher. The default recovery threshold is 0.04 mcg/mL.

### Where to be careful

The active metabolite α-hydroxymidazolam, which matters mainly in renal failure and during prolonged infusion, is not modelled. Midazolam's context-sensitive half-time grows markedly with long infusions because of its large slow compartment; the model will show this, but the model's typical adult is not the intensive care patient in whom it is most often seen.
