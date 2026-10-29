### The model

Norepinephrine's parameters come from Li, Koomen, Eleveld, Colin and colleagues in Groningen (*Clin Pharmacokinet* 2024;63:1597-1608). Thirty-six healthy volunteers aged 18 to 70, six men and six women in each of three age bands, were given 5 mcg and then 0.04, 0.08, 0.12, 0.16 and 0.20 mcg/kg/min for 15 minutes each, first awake and then under propofol and remifentanil, and 1219 arterial samples were fitted in NONMEM. Two compartments: clearance 2.1 L/min, central volume 2.4 L, intercompartmental clearance 0.6 L/min and peripheral volume 3.6 L for a 70 kg adult of 35.

### What is plotted

Plasma norepinephrine in ng/mL **above the patient's own**. Norepinephrine is endogenous, and the model's production rate, 497.7 ng/min, divided by its clearance gives a baseline of 1.40 nmol/L (0.24 ng/mL): exactly the median the study measured before any drug in its youngest volunteers. Add about 0.24 ng/mL to read a total; at 0.1 mcg/kg/min the baseline is about 7% of it. Doses are norepinephrine **base**, as Dutch and US labels state it; the model's clearance is the known metabolic clearance of norepinephrine only on that reading.

### Covariates

Weight, allometrically (exponent 0.75 on the clearances, 1 on the volumes), applied to the fat-free-mass weight when [the scaling switch](help:models/fat-free-mass) is on; and age, about 3% lower clearance per decade, as published. The model's third covariate, the measured **propofol** concentration, is not applied: the library cannot feed one drug's simulated concentration into another's clearance. The curve is the **awake** model; under general anaesthesia at the study's median 3.53 mcg/mL clearance is 11.8% lower, so every concentration is about 13% higher than drawn.

### The effect site

Joachim and colleagues in Paris (*Br J Clin Pharmacol* 2024;90:2861-2869) gave 29 anaesthetised adults 10 mcg boluses of dilute norepinephrine through a peripheral line and recorded the arterial pressure continuously: mean arterial pressure peaked a median of **74 seconds** after the bolus (interquartile range 53-94), heart rate at 74 seconds and cardiac output at 61. The effect site here is solved to peak at 74 seconds. That time includes the transit from a peripheral line, which their model carried as a lag and a depot; here it is folded into the effect site, as every time to peak in the library is. No pressure is drawn.

### Checked against the paper

The paper's own simulation of 0.12 mcg/kg/min for 15 minutes reaches about 24 nmol/L, and this model gives 25. The observed medians at the end of each awake step in its Table 2 run 10 to 22% above the typical-value curve, most at the highest rates; the paper's goodness-of-fit plots show the same slight underprediction at high concentrations.

### Where to be careful

Healthy volunteers, not patients in shock, in whom clearance falls with severity of illness (Beloeil 2005) and whose sympathetic tone raises the baseline many times over. The 13.7-second lag between the infusion arm and the sampling arm is not drawn. The pressure response is not modelled. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
