### The model

Intravenous haloperidol, from Li and colleagues (*Pharmaceutics* 2022;14:549): 22 critically ill adults (median age 67) treated for delirium with 1 mg intravenously every 8 hours (0.5 mg from age 80, 2 mg when agitated). One compartment: clearance 51.7 L/h and volume 1490 L, **absolute** values because the data were intravenous. The half-life is 20 h. Between-patient variability in clearance was 30%.

### A separate drug from oral haloperidol

[Oral haloperidol](help:drugs/haloperidol) runs a different study's **apparent** parameters, so the two routes are separate entries, as with [amiodarone](help:drugs/amiodarone) and [amiodaroneIV](help:drugs/amiodaroneIV). This one offers intravenous units only.

### Covariates

Weight, age, sex and CYP2D6 genotype were tested and rejected, so the **CYP 2D6** field does not change this drug (unlike [oral haloperidol](help:drugs/haloperidol)). The final model lowered clearance with C-reactive protein, flattening above about 100 mg/L, but the exact function is in a supplement that was not available, so **the CRP effect is not applied**: this is the typical patient's clearance. The values take the library's [fat-free-mass scaling](help:models/fat-free-mass).

### Where to be careful

A one-compartment model fitted to samples hours apart does not describe the minutes after a bolus, when the true concentration is higher before the drug distributes. No relation between concentration and delirium, sedation or QTc was established.

### Effect site

The D2-occupancy effect site of Lim and colleagues (*Pharm Res* 2013;30:683-693) is supplied here as on [oral haloperidol](help:drugs/haloperidol): ke0 0.213 per hour, half-maximal putamen occupancy at 0.791 ng/mL. It was fitted after oral doses in healthy men, and its use after an intravenous bolus in critically ill patients is a transfer. Occupancy is a receptor biomarker, not a target. There is no band: the AGNP therapeutic reference range describes oral maintenance treatment of psychosis at steady state, not intravenous doses for delirium, just as [amiodaroneIV](help:drugs/amiodaroneIV) takes no chronic trough window.
