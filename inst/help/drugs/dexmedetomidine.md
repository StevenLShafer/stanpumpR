### Two models

**Above 1 year:** stanpumpR uses the universal model of Morse, Cortinez and Anderson (*J Clin Med* 2020;9:3480), fitted to five pooled data sets: 202 people with 2145 plasma concentrations, from 40.6 weeks postmenstrual age to 70.8 years and from 3.1 to 152 kg. The data were children after cardiac surgery, obese and non-obese adults at laparoscopic surgery, and healthy volunteers. The volunteer data of Hannivoort and colleagues entered as concentrations simulated from their model, not as their measurements.

It is a three-compartment model, with each parameter given for a standard 70 kg man 176 cm tall:

| Parameter | Value |
|---|---|
| V1, V2, V3 | 25.2, 34.4 and 65.4 L, in proportion to normal fat mass |
| Elimination clearance | 0.897 L/min, on fat-free mass to the 0.75 power, times a maturation factor |
| Intercompartmental clearances | 1.68 and 0.62 L/min, on fat-free mass to the 0.75 power |

**Normal fat mass** is the fat-free mass plus 29.3% of the fat mass. Fat therefore adds to the volumes but not to the clearances. A 140 kg man has larger volumes than a lean man of the same height, but his clearance reflects only his lean tissue. Fat-free mass comes from the Al-Sallami equation at every age.

**Maturation**: clearance is multiplied by PMA / (52.4 + PMA), where PMA is the postmenstrual age in weeks. PMA is the *Gestational age at birth* in the Patient Profile (blank means a term birth, 40 weeks) plus the postnatal age. Morse's model was fitted from term newborns upward, and its maturation is the paper's "42% of adult values [in term neonates], reaching 80% by 3 years"; since stanpumpR uses it only above one year, the factor starts at 64% for a one-year-old born at term and is 79% at three. The factor is used exactly as printed, so it is still a little below 1 in adults: 0.954 at 20 years and 0.986 at 70.

Two choices the paper leaves open:

- The intercompartmental clearances are given the same fat-free-mass scaling as the elimination clearance, since the paper fixes the fat factor on clearance at zero.
- The maturation factor is applied to the elimination clearance only, since the paper puts it on "CL".

Because the model has its own size covariates, *Adjust weight to fat-free mass* does not change it above one year.

**A check that does not match:** the clearances in the paper's own Table 3 run 2% (20 years) to 17% (3 years) above what its printed equations give. No single factor reconciles them, so the table appears to come from a different version of the model. stanpumpR follows the printed equations.

### Age 1 year or under

The infant model from Zuppa and colleagues' study of dexmedetomidine in infants undergoing cardiac surgery with cardiopulmonary bypass (*Br J Anaesth* 2019;123:839-852). The infants analysed were up to 180 days old; stanpumpR uses the model up to one year, so between 181 days and one year it is an extrapolation. It is a two-compartment model. Zuppa scaled it to total body weight, the volumes in proportion to weight / 70 and the clearances to (weight / 70)^0.75. By default stanpumpR replaces those factors with the fat-free-mass factors used for the adult model (the fat-free-mass equation is itself extrapolated below three years); unticking *Adjust weight to fat-free mass* restores Zuppa's scaling. The model has **separate parameter sets for cardiopulmonary bypass**: at the start of bypass, at each temperature from 36 to 31 °C (V1 scales with (temperature/37)^−1.6), and after bypass, when clearance recovers with a maturation term in postnatal age, days / (1.77 + days). That term is zero at birth, so a newborn entered at age 0 is treated as one day old. Clearance on bypass is about a sixteenth of its pre-bypass value, so an infusion continued through bypass produces a rising concentration. Enter the CPB events on the Events panel to see it; see [Events that change the kinetics](help:models/pk-events).

stanpumpR uses the paper's allometrically scaled model (its Table 7): per 70 kg, a clearance of 1240 mL/min before bypass, 74.1 on bypass and 623 after. The paper's abstract gives 1240 mL/min per 70 kg as the clearance *after* bypass; that is the value from the authors' linearly scaled model, which they also present and preferred statistically, and it only happens to equal the allometric model's clearance before bypass.

Morse's model covers infants too, but it cannot represent bypass, so the infant model is kept at one year or under (Steven L. Shafer's decision, November 2026). Until then the model above one year was fixed parameters close to Dyck and colleagues' pooled estimates (*Anesthesiology* 1993;78:821-828).

### Effect site

Above one year the time to peak effect is 14.9 minutes, from Colin and colleagues (*Br J Anaesth* 2017;119:200-210). They estimated the equilibration of sedation (MOAA/S) at 0.0428/min on Hannivoort's kinetics. Because Morse's kinetics are different, the equilibration is carried over as the time to peak effect it gives with Hannivoort's model, and the library then solves its own equilibration on Morse's model. Colin's equilibration for the bispectral index is much faster (a time to peak of 1.6 minutes), but it rests on a model of arousal, and the band here is for sedation. The infant model's time to peak effect, 2 minutes, is described in the code as a guess. Dexmedetomidine's sedative effect does lag its plasma concentration substantially. That is why a loading dose is given over ten minutes, and why the effect-site line is much smoother than the plasma line. See [the dexmedetomidine scenario](scenario:dexmedetomidine-loading).

### Typical concentrations

The shaded band is 0.4 to 0.8 ng/mL, the range associated with sedation in the intensive care unit. The *Typical* value that draws the *Mid* band is its midpoint, 0.6 ng/mL. Morse's own dosing simulations target 1 ng/mL.

### Where to be careful

Apart from size and maturation the model has no covariates. In particular, the elderly, who are markedly more sensitive to dexmedetomidine's hemodynamic effects, are not distinguished. Those effects, the bradycardia and the biphasic blood pressure response, are pharmacodynamic and not represented. Morse's central volume (25.2 L per 70 kg) is far larger than Hannivoort's 1.78 L. The paper attributes the difference to Hannivoort's study design: targeted infusions sampled early, and the hemodynamic response to a rapid infusion. A bolus given quickly may therefore produce a higher early plasma concentration than this model predicts.

The boundary between the two models at exactly one year is a step, not a transition. For a 9 kg, 75 cm boy, the plasma concentration immediately after a bolus is about 5 times higher at one year than just over it, and the clearance about twice as high. Zuppa's infants were at most six months old, so between six months and one year the infant model is an extrapolation.
