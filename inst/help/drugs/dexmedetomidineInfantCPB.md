### Infants at cardiac surgery, with parameters for bypass

This is the infant model from Zuppa and colleagues' phase 1 study of dexmedetomidine in corrective infant cardiac surgery (*Br J Anaesth* 2019;123:839-852): 122 neonates and infants, 0 to 180 days old. It is a two-compartment model with one set of parameters before bypass and **separate parameter sets for cardiopulmonary bypass**:

- at the start of bypass;
- at each temperature from 36 to 31 °C, with V1 scaled by (temperature/37)^−1.6;
- after bypass, when clearance recovers with a maturation term in postnatal age, days / (1.77 + days).

The maturation term is zero at birth, so a newborn entered at age 0 is treated as one day old. Clearance on bypass is about a sixteenth of its pre-bypass value, so an infusion continued through bypass produces a rising concentration. Enter the CPB events on the Events panel to see it; see [Events that change the kinetics](help:models/pk-events).

Until November 2026 this was the infant branch of [dexmedetomidine](help:drugs/dexmedetomidine). That drug now uses Morse's universal model at every age. Morse's model has no bypass parameters, so these sets became a drug of their own, as heparin on bypass is kept beside heparin. Choose this drug for an infant whose dexmedetomidine runs through cardiopulmonary bypass. For an infant who is not on bypass, the dexmedetomidine drug's model was fitted to more patients across a wider range of ages.

### Body size

Zuppa scaled the model to total body weight: volumes in proportion to weight / 70, and clearances to (weight / 70)^0.75. By default, stanpumpR replaces those factors with the library's fat-free-mass factors. The fat-free-mass equation is itself extrapolated below three years. Unticking *Adjust weight to fat-free mass* restores Zuppa's scaling.

The model is offered at every age, but it was fitted only to infants up to 180 days old. In anyone older it is an extrapolation.

### Effect site and band

The time to peak effect, 2 minutes, is described in the code as a guess. The shaded band is the same 0.4 to 0.8 ng/mL sedation range used for dexmedetomidine.

### Where to be careful

Parameters step at each event; they do not ramp. Temperature on bypass is therefore entered as a sequence of temperature events. The effects of dexmedetomidine on heart rate and conduction are pharmacodynamic and not represented. Two of Zuppa's 122 infants had junctional rhythm and two had atrioventricular block.
