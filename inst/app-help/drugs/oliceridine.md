### The model

Oliceridine's parameters are from Dahan and colleagues (*Anesthesiology* 2020;133:559-568), a pharmacokinetic-pharmacodynamic analysis of oliceridine's analgesia and respiratory depression against morphine's. The model is **two-compartment**: a central volume of 28 L, a peripheral volume of 29.1 L, a clearance of 31.7 L/h (0.53 L/min) and an intercompartmental clearance of 37.5 L/h. The drug file notes that its times are in hours and converts.

### Covariates

None in the source. The parameters take the default [fat-free-mass scaling](help:models/fat-free-mass) and are used as published with the switch off.

### Effect site

The time to peak effect is 4.4 minutes. Dahan found oliceridine's analgesia (cold-pressor tolerance) equilibrated too fast to estimate; the model carries that as an equilibration half-time of 1 minute, which on this disposition peaks a bolus at 4.4 minutes. That is a stated assumption, not a measurement. The 0.25 hours (15 minutes) in the paper is the equilibration half-time of the ventilatory response, which the model used as a time to peak until October 2026.

### MEAC and typical concentrations

MEAC is 27.9 ng/mL, Dahan's **analgesic** potency: the effect-site concentration at which the probability of a 50 per cent increase in cold-pressor hand-withdrawal latency is one half (morphine's was 34.3 ng/mL). The band's typical value, 27.4 ng/mL, is a different end point, the **respiratory** potency: the concentration for a 25 per cent fall in the hypercapnic ventilatory response (morphine's, for a 50 per cent fall, was 33.7 ng/mL). The two coincide numerically, which is the paper's point: over 0 to 35 ng/mL the probability of analgesia exceeds that of respiratory depression for oliceridine and not for morphine. The band, 18.3 to 37.5 ng/mL, and both values are in the drug library, not the drug file, which returns none of its own. Only one concentration curve is drawn; the two end points are labels on it, not separate effect curves. Both were measured in 30 healthy men, not in patients after surgery, so neither is a postoperative MEAC. The *Time until threshold* line uses 27.4 ng/mL too: it is the time until the effect-site concentration falls below the respiratory potency, not a prediction of when spontaneous breathing returns.

### Where to be careful

The model describes the volunteers in Dahan's study. Oliceridine is metabolised by CYP2D6 and CYP3A4, so poor metabolisers and patients on inhibitors have reduced clearance; this is not represented, and the CYP2D6 field in the Patient Profile does not act on it.
