### The model

Oliceridine's parameters are from Dahan and colleagues (*Anesthesiology* 2020;133:559-568), a pharmacokinetic-pharmacodynamic analysis of oliceridine's analgesia and respiratory depression against morphine's. The model is **two-compartment**: a central volume of 28 L, a peripheral volume of 29.1 L, a clearance of 31.7 L/h (0.53 L/min) and an intercompartmental clearance of 37.5 L/h. The drug file notes that its times are in hours and converts.

### Covariates

None in the source. The parameters take the default [fat-free-mass scaling](help:models/fat-free-mass) and are used as published with the switch off.

### Effect site

Dahan and colleagues estimated the equilibration between plasma and the effect site for the **ventilatory** effect only, with a half-life of 0.25 hours (15 minutes), and did not quantify one for analgesia. The drug file supplies that ke0 (0.046/min) and the time to peak effect after a bolus is solved from it, about 26 minutes with these kinetics. Until October 2026 the 15 minutes was read as the time to peak effect itself, which implied an equilibration half-life of about 6 minutes.

### MEAC and typical concentrations

MEAC is 27.9 ng/mL, Dahan's **analgesic** potency: the effect-site concentration at which the probability of a 50 per cent increase in cold-pressor hand-withdrawal latency is one half (morphine's was 34.3 ng/mL). The band's typical value, 27.4 ng/mL, is a different end point, the **respiratory** potency: the concentration for a 25 per cent fall in the hypercapnic ventilatory response (morphine's, for a 50 per cent fall, was 33.7 ng/mL). The two coincide numerically, which is the paper's point: over 0 to 35 ng/mL the probability of analgesia exceeds that of respiratory depression for oliceridine and not for morphine. The band, 18.3 to 37.5 ng/mL, and both values are in the drug library, not the drug file, which returns none of its own. Only one concentration curve is drawn; the two end points are labels on it, not separate effect curves.

### Where to be careful

The model describes the volunteers in Dahan's study. Oliceridine is metabolised by CYP2D6 and CYP3A4, so poor metabolisers and patients on inhibitors have reduced clearance; this is not represented, and the CYP2D6 field in the Patient Profile does not act on it.
