### The model

Vasopressin's parameters come from Fabian and colleagues (*J Physiol* 1969;204:653-668), who infused arginine vasopressin at 120 mU/min for 20 minutes into six volunteers aged 20 to 22 (five men, one woman), sampled plasma for 10 minutes after the infusion stopped, and measured its antidiuretic activity in the rat against the preparation they had infused. The fall was a single exponential over the whole sampling period, so the model is one compartment: the medians of the six, a volume of 8.5 L (about two thirds of the extracellular volume) and a clearance of 1.0 L/min, a half-life of about 6 minutes.

The clearance agrees with the label's 9 to 25 mL/min/kg in vasodilatory shock (14 mL/min/kg at 70 kg), with the 0.67 L/min Davison and colleagues measured by radioimmunoassay in nonpregnant women. Landry and colleagues (*Circulation* 1997;95:1122) found about 30 pg/mL on 0.01 U/min in septic shock: at the label's 530 units per mg the model's 10 mcU/mL is 19 pg/mL, 22 with the patients' own 3 pg/mL, about a quarter below theirs, while Davison's lower clearance would give 31. A radioiodinated-tracer study (Baumann and Dingman 1976) found a quarter of this clearance; the tracer is not the hormone.

### What is plotted

Plasma vasopressin in **mcU/mL** (microunits per mL, the same as mU/L), **above the patient's own**. That is the unit the source measured and the unit the drug is dosed in, so no conversion to mass is involved; at the label's 530 units per mg, 1 mcU/mL is 1.89 pg/mL. Vasopressin does not appear to bind plasma protein (the label, section 12.3), so the total plotted is the unbound concentration. Doses are in units: a bolus in U, an infusion in U/min, U/hr or per kg. The patient's own vasopressin, about 1 to 10 pg/mL and inappropriately low in septic shock, is not drawn.

### Covariates

None was estimated: the six subjects' weights were not reported, so the volume and clearance are fixed values, scaled to fat-free mass with [the scaling switch](help:models/fat-free-mass) on. In **pregnancy** the clearance is multiplied by 5.07: Davison and colleagues (*Am J Physiol* 1993;264:F348-F353) measured it in the same six women in late pregnancy and 10 to 12 weeks after delivery, 3.4 against 0.67 L/min, from the placenta's vasopressinase. They measured physiological concentrations, though, about a tenth of those of a pressor infusion, so applying the ratio at pressor doses is an extrapolation: if the enzyme saturates, pregnancy raises the clearance less than this.

### Where to be careful

Six subjects, a bioassay, and 10 minutes of sampling: the authors themselves call the half-life an approximation, and a slower second phase could be missed. The subjects were healthy; shock was not studied. The effect site peaks **3 minutes** after a bolus. That is a provisional clinical observation, not a measurement: the pressor response to vasopressin is not immediate, as the heart rate response to epinephrine is, and the time will be replaced when a measured one is published. No band is drawn and no pressure is predicted. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
