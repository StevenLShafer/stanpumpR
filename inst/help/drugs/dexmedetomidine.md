### Two models

**Age over 1 year:** the adult model of Dyck and colleagues (*Anesthesiology* 1993;78:821-828), who gave dexmedetomidine by infusion to volunteers. The published model is a fixed three-compartment model with a central volume of 8.06 L and no covariates. By default, with *Adjust weight to fat-free mass* ticked, stanpumpR treats those parameters as describing the 70 kg, 170 cm reference man and scales the volumes with the patient's fat-free mass relative to his, and the clearances with that ratio to the 0.75 power, so the same dose in micrograms gives a higher concentration in a smaller or leaner patient. Unticking the box restores the published, unscaled model, in which the same dose gives the same curve at any weight or age above one year.

**Age 1 year or under:** the infant model cited in the code as "Zuppa BJA 2019", from Zuppa and colleagues' study of dexmedetomidine in infants undergoing cardiac surgery with cardiopulmonary bypass (*Br J Anaesth* 2019). The infants analysed were up to 180 days old; stanpumpR uses the model up to one year, so between 181 days and one year it is an extrapolation. It is a two-compartment model. Zuppa scaled it to total body weight, the volumes in proportion to weight / 70 and the clearances to (weight / 70)^0.75. By default stanpumpR replaces those factors with the fat-free-mass factors used for the adult model (the fat-free-mass equation is itself extrapolated below three years); unticking *Adjust weight to fat-free mass* restores Zuppa's scaling. The model has **separate parameter sets for cardiopulmonary bypass**: at the start of bypass, at each temperature from 36 to 31 °C (V1 scales with (temperature/37)^−1.6), and after bypass, when clearance recovers with a maturation term. Clearance on bypass is about a sixteenth of its pre-bypass value, so an infusion continued through bypass produces a rising concentration. Enter the CPB events on the Events panel to see it; see [Events that change the kinetics](help:models/pk-events).

The infant citation lacks page numbers and a PubMed identifier in the code, and is one of the weaker citations in the library.

### Effect site

The adult time to peak effect is 10 minutes and the infant's 2 minutes; both are described in the code as guesses. Dexmedetomidine's sedative effect does lag its plasma concentration substantially, which is why a loading dose is given over ten minutes and why the effect-site line is so much smoother than the plasma line. See [the dexmedetomidine scenario](scenario:dexmedetomidine-loading).

### Typical concentrations

The shaded band is 0.4 to 0.8 ng/mL, the range associated with sedation in the intensive care unit; the *Typical* value that draws the *Mid* band is its midpoint, 0.6 ng/mL.

### Where to be careful

The adult model has no covariates of its own, and the elderly, who are markedly more sensitive to dexmedetomidine's hemodynamic effects, are not distinguished. Those effects, the bradycardia and the biphasic blood pressure response, are pharmacodynamic and not represented. The boundary between the two models at exactly one year is a step, not a transition, and it is large: the infant central volume per kilogram is far bigger than the adult one. For a 9 kg, 75 cm boy, the plasma concentration immediately after a bolus is about 16 times higher just over one year than at one year with the default fat-free-mass scaling, and about twice as high with the published scaling. Neither model was developed in children between six months and one year.
