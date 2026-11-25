### The model

Honda and colleagues at Ono Pharmaceutical pooled the Japanese phase 1 studies of landiolol (*Drug Metab Pharmacokinet* 2008;23:447-455). They studied 47 healthy men aged 20 to 33, weighing 50.7 to 77.2 kg. The men were given 60-minute infusions of 5 to 80 mcg/kg/min, some after a 1-minute loading dose. In all, 420 venous whole-blood samples were assayed.

A population fit gave two compartments per kilogram:
- clearance 36.6 mL/min/kg
- central volume 101 mL/kg
- intercompartmental clearance 16.1 mL/min/kg
- peripheral volume 55.6 mL/kg

The fit also had a lag of 0.82 minutes, which stanpumpR does not draw.

### When stanpumpR uses it

This is one of [three landiolol models](help:drugs/landiolol). stanpumpR uses it for the young adults it studied. It is also the nearest model for children and for adults up to 35.

### Where to be careful

- These were healthy, awake young men, and covariates were not explored.
- Concentrations are in venous whole blood. The two Kunisawa studies measured arterial plasma during anaesthesia, and their central volumes are smaller.
- Four outlying late samples, from three subjects, were removed before the final fit.

See [the vasoactive and resuscitation drugs](help:models/vasoactive).
