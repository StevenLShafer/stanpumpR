Oxygen is entered as a **fresh gas flow** in L/min at the flowmeter. With air and nitrous oxide it makes up the total fresh gas flow, and its share of that flow (plus the oxygen in any air) is the inspired oxygen fraction before any rebreathing.

Oxygen is modelled in the gas phase only, in the circuit and the alveoli, with a metabolic consumption of 3.5 mL/kg/min: it binds haemoglobin non-linearly and has no meaningful partition coefficient, so it is not carried into tissue compartments. Its alveolar fraction is floored at zero. The **oxygen panel** appears whenever any gas flow is running and shows the inspired and alveolar oxygen, so that a hypoxic mixture is visible whatever else is given.

Oxygen consumption also shrinks the gas volume, as uptake of an anesthetic does; carbon dioxide replaces most of it in the alveoli and is removed by the absorber from whatever exhaled gas is rebreathed. This is one of the deliberate differences from Gas Man, which does not model oxygen; see [Where the gas engine differs from Gas Man](help:models/gas-differences). It is why, at low fresh gas flows, the concentrations of the other gases run a little higher than the fresh gas composition alone would suggest.

Oxygen has no MAC and is not timed by *Time until threshold*.
