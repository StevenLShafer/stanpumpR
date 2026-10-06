### The model

Naloxone's parameters are from Papathanasiou and colleagues (*Br J Anaesth* 2019;123:e204-e214), a population analysis of naloxone kinetics. The model is three-compartment with every volume and clearance proportional to weight: 0.408, 0.636 and 1.64 L/kg and 0.049, 0.046 and 0.026 L/min/kg.

### Covariates

Weight only, scaling every parameter linearly.

### Effect site

The time to peak effect is 1 minute, described in the code as based on clinical observation: naloxone works within a minute or two of an intravenous dose.

### Typical concentrations

Naloxone has no therapeutic range in the library (the band is 0 to 0), because the concentration needed depends entirely on the opioid it is opposing. Its recovery threshold is 1 ng/mL.

### Why it is here

Naloxone is in the library for one teaching point: its duration is short, 30 to 90 minutes, and shorter than that of almost every opioid it reverses. A patient who wakes after naloxone may renarcotise as the naloxone leaves and the opioid remains. [The naloxone scenario](scenario:naloxone-morphine) puts the two curves on one picture.

### Where to be careful

The model says nothing about how much naloxone is needed, which depends on the opioid's affinity (buprenorphine and fentanyl analogues need more) and on the degree of respiratory depression. Intranasal naloxone, which Papathanasiou's study also characterised, is not offered as a route.
