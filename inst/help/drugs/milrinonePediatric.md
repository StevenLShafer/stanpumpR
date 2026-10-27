### The model

Hornik and colleagues for the Pediatric Trials Network (*J Clin Pharmacol* 2019;59:1606-1619) pooled 111 opportunistic plasma samples from 74 patients from birth to 18 years. One compartment described them, with clearance 15.9 L/h at 70 kg scaled by weight to the 0.75 power, a maturation function of postmenstrual age (half-mature at 67.7 weeks, Hill coefficient 1.12) and creatinine clearance to the 0.522 power about 117 mL/min/1.73 m2; the volume is 32.2 L at 70 kg, in proportion to weight.

### What is plotted

Plasma milrinone in ng/mL, with the 100-300 ng/mL target range the authors simulated against. That range was a dose-finding target in their simulations, not a children's response range they measured, and the typical value marked within it, 167 ng/mL, is the adult model's: Bailey's concentration for a 50% rise in cardiac index after cardiopulmonary bypass in adults ([milrinone](help:drugs/milrinone)). No concentration-response relation was measured in these children. Postmenstrual age is taken as 40 weeks plus the age (a term birth): there is no gestational-age field, so for an infant born prematurely the maturation, and with it the clearance, is overstated and the plotted concentration is too low. Creatinine clearance is the updated bedside Schwartz estimate from the patient's own serum creatinine, as in the paper, at every age (the model is a child's; for an adult, use [milrinone](help:drugs/milrinone)); a blank creatinine is an assumed normal value for age and sex, and the estimate is held to the 13-261 mL/min/1.73 m2 the study covered. No time of peak effect was measured in children, so no effect site is drawn.

### Where to be careful

The sampling was sparse and opportunistic: between-patient variability in clearance was 70%, and the maturation parameters are imprecise (relative standard errors of 64% and 70%). In the paper's simulations, 50 mcg/kg with 0.5 mcg/kg/min stayed in the target range except at a creatinine clearance of 30 mL/min/1.73 m2, where halving both was needed. The adult model is [milrinone](help:drugs/milrinone). See [the vasoactive and resuscitation drugs](help:models/vasoactive).
