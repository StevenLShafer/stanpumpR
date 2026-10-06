### Two models

**Age over 1 year:** the adult model of Dyck and colleagues (*Anesthesiology* 1993;78:821-828), who gave dexmedetomidine by infusion to volunteers. It is a fixed three-compartment model with a central volume of 8.06 L and no covariates: the same dose in micrograms gives the same curve at any weight or age above one year.

**Age 1 year or under:** the infant model cited in the code as "Zuppa BJA 2019", from Zuppa and colleagues' study of dexmedetomidine in infants undergoing cardiac surgery with cardiopulmonary bypass (*Br J Anaesth* 2019). It is a two-compartment model scaled to weight (linearly for volumes, allometrically for clearances) with **separate parameter sets for cardiopulmonary bypass**: at the start of bypass, at each temperature from 36 to 31 °C (V1 scales with (temperature/37)^−1.6), and after bypass, when clearance recovers with a maturation term. Clearance on bypass is about a sixteenth of its pre-bypass value, so an infusion continued through bypass produces a rising concentration. Enter the CPB events on the Events panel to see it; see [Events that change the kinetics](help:models/pk-events).

The infant citation lacks page numbers and a PubMed identifier in the code, and is one of the weaker citations in the library.

### Effect site

The adult time to peak effect is 10 minutes and the infant's 2 minutes; both are described in the code as guesses. Dexmedetomidine's sedative effect does lag its plasma concentration substantially, which is why a loading dose is given over ten minutes and why the effect-site line is so much smoother than the plasma line. See [the dexmedetomidine scenario](scenario:dexmedetomidine-loading).

### Typical concentrations

The shaded band is 0.4 to 0.8 ng/mL, the range associated with sedation in the intensive care unit. The library's *Typical* value, which draws the *Mid* band, is recorded as 10 ng/mL, which lies far outside that range and appears to be a transcription error; the *Range* band is the one to use.

### Where to be careful

The adult model has no covariates, and the elderly, who are markedly more sensitive to dexmedetomidine's hemodynamic effects, are not distinguished. Those effects, the bradycardia and the biphasic blood pressure response, are pharmacodynamic and not represented. The boundary between the two models at exactly one year is a step, not a transition.
