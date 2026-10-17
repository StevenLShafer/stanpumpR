### The model

The fentanyl parameters are a pooled analysis of several studies: the code lists its sources as McClain and Hug (1980), Scott and Stanski (1987), Hudson (1986), Varvel (1989) and Shafer (1990), the last being the model fitted to computer-controlled infusion data (Shafer SL, Varvel JR, Aziz N, Scott JC. *Anesthesiology* 1990;73:1091-1102). None of the five prints these values, and how they were derived is not recorded: they replaced Scott and Stanski's own parameters in 2019. Shafer's 1990 pooled model of the same five data sets is different (central volume 6.09 L, clearance 0.504 L/min). The citation returned by the model function lists all five.

At 70 kg the volumes are 12.1, 35.7 and 224 L and the clearances 0.632, 2.8 and 1.55 L/min.

### Covariates

Size only. By default, with *Adjust weight to fat-free mass* ticked, the volumes scale with the patient's fat-free mass relative to the 70 kg, 170 cm reference man and the clearances with that ratio to the 0.75 power, so height, age and sex enter through fat-free mass ([fat-free-mass scaling](help:models/fat-free-mass)). Unticking the box restores the original scaling on total weight: volumes linearly and clearances allometrically to the 0.75 power. The model has no age term of its own, although Scott and Stanski found fentanyl's pharmacodynamic sensitivity to increase with age.

### Effect site

The time to peak effect is 3.6 minutes, from Shafer and Varvel (*Anesthesiology* 1991;74:53-63, Figure 2, EEG). Fentanyl's effect site thus lags the plasma more than remifentanil's or alfentanil's, and a bolus reaches a lower fraction of its plasma peak at the effect site.

### MEAC and typical concentrations

MEAC is 0.6 ng/mL (Gourlay and colleagues reported a mean of 0.63 ng/mL in postoperative patients). The shaded band is 0.8 to 2 MEAC, 0.48 to 1.2 ng/mL. Concentrations of 2 to 5 ng/mL are typical during surgery with an inhaled agent; respiratory depression becomes clinically important above about 2 ng/mL in a patient who is not stimulated.

### Where to be careful

Fentanyl's large slow compartment (224 L at 70 kg) is what makes its context-sensitive half-time grow from minutes to hours as an infusion continues. Nothing about a single bolus predicts the behaviour of a long infusion; see [the context-sensitive decrement scenario](scenario:context-sensitive-opioids) and [Time until threshold](help:models/recovery).
