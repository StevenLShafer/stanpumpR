### The model

Oliceridine's parameters are from Dahan and colleagues (*Anesthesiology* 2020;133:559-568), a pharmacokinetic-pharmacodynamic analysis of oliceridine's analgesia and respiratory depression against morphine's. The model is **two-compartment**: a central volume of 28 L, a peripheral volume of 29.1 L, a clearance of 31.7 L/h (0.53 L/min) and an intercompartmental clearance of 37.5 L/h. The drug file notes that its times are in hours and converts.

### Covariates

None.

### Effect site

The time to peak effect is 4.4 minutes. Dahan found oliceridine's analgesia (cold-pressor tolerance) equilibrated too fast to estimate; the model carries that as an equilibration half-time of 1 minute, which on this disposition peaks a bolus at 4.4 minutes. That is a stated assumption, not a measurement. The 0.25 hours (15 minutes) in the paper is the equilibration half-time of the ventilatory response, which the model used as a time to peak until October 2026.

### MEAC and typical concentrations

MEAC is 27.9 ng/mL and the shaded band 18.3 to 37.5 ng/mL, both from the drug library rather than the drug file, which returns no values of its own for them. The MEAC is Dahan's C100 for analgesia, the concentration that doubled cold-pressor tolerance (morphine's was 34.3 ng/mL in the same study). Oliceridine is a biased μ-agonist developed for a wider margin between analgesia and respiratory depression; the model is pharmacokinetic and says nothing about that margin.

### Where to be careful

The model describes the volunteers in Dahan's study. Oliceridine is metabolised by CYP2D6 and CYP3A4, so poor metabolisers and patients on inhibitors have reduced clearance; this is not represented, and the CYP2D6 field in the Patient Profile does not act on it.
