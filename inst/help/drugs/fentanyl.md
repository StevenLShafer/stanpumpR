### The model

Three compartments from **Jaeyoung Bae**, **Gyu-Jeong Noh** and colleagues (*Br J Anaesth* 2020;125:976-985, [doi](https://doi.org/10.1016/j.bja.2020.06.066)). They refitted the five classic fentanyl data sets that Steven Shafer supplied (McClain and Hug 1980, Scott and Stanski 1987, Hudson 1986, Varvel 1989 and Shafer 1990) together with 30 Korean patients sampled arterially after a 100 mcg bolus before open abdominal surgery: 95 adults aged 20 to 88 years, weighing 40 to 105 kg, 2092 plasma concentrations measured to 12 hours.

At 70 kg the volumes are 10.1, 26.5 and 206 L and the clearances 0.704, 2.38 and 1.49 L/min. The median absolute prediction error was 22.7%.

Until November 2026 the library used a pooled set of the same five studies whose values none of them prints (volumes 12.1, 35.7 and 224 L, clearances 0.632, 2.8 and 1.55 L/min), adopted in 2019 with its derivation unrecorded. Bae's set has an 11% higher clearance and smaller volumes, so fentanyl now falls faster after an infusion: after 4 hours at 2 mcg/kg/hr the plasma halves in about an hour, where it took about an hour and a half.

### Covariates

Weight only. The volumes scale as weight to the power 1.23 and the clearances as weight to the power 0.313, both exponents estimated from the data (the clearance exponent is the less certain, with a 95% interval of 0.04 to 0.58). With *Adjust weight to fat-free mass* ticked, as it is by default, the weight used is the pharmacokinetic weight, 70 kg times the patient's fat-free mass over the reference man's, so height, age and sex enter through fat-free mass ([fat-free-mass scaling](help:models/fat-free-mass)); unticked, it is total weight, as Bae published. The reference man, 70 kg and 170 cm, receives the published values either way. Outside 40 to 105 kg the exponents are an extrapolation. The model has no age term, although Scott and Stanski found fentanyl's pharmacodynamic sensitivity to increase with age.

### Effect site

The time to peak effect is 3.6 minutes, from Shafer and Varvel (*Anesthesiology* 1991;74:53-63, Figure 2, EEG). Fentanyl's effect site thus lags the plasma more than remifentanil's or alfentanil's, and a bolus reaches a lower fraction of its plasma peak at the effect site.

### MEAC and typical concentrations

MEAC is 0.6 ng/mL, the library's value. It sits near the 0.63 ± 0.25 ng/mL of Gourlay and colleagues (*Anesth Analg* 1988;67:329-337), but that was a **minimum effective concentration** (MEC): blood sampled from postoperative patients immediately before each patient-controlled demand, when pain had returned, not the concentration at which titration restored analgesia. Bae and colleagues (*Br J Anaesth* 2020;125:976-985) measured both after open abdominal surgery: a median MEC of 0.72 ng/mL and a median MEAC of 0.99 ng/mL. The MEAC panel divides by 0.6 whichever is meant, so read its percentages as relative to a concentration at which pain typically returns. The shaded band is 0.8 to 2 MEAC, 0.48 to 1.2 ng/mL. Concentrations of 2 to 5 ng/mL are typical during surgery with an inhaled agent; respiratory depression becomes clinically important above about 2 ng/mL in a patient who is not stimulated.

### Where to be careful

Fentanyl's large slow compartment (206 L at 70 kg) is what makes its context-sensitive half-time grow from minutes to hours as an infusion continues. Nothing about a single bolus predicts the behaviour of a long infusion; see [the context-sensitive decrement scenario](scenario:context-sensitive-opioids) and [Time until threshold](help:models/recovery).
