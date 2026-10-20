### The model

Milrinone's parameters come from Bailey and colleagues at Emory (*Anesthesiology* 1994;81:616-622): 25 adults having cardiac surgery, given 25, 50 or 75 mcg/kg just before or just after separation from cardiopulmonary bypass, or 50 mcg/kg with 0.5 mcg/kg/min for the rest of the study, with arterial samples for 16 hours. The pooled data were fitted to three compartments per kilogram: volumes of 102, 198 and 1398 mL/kg and clearances of 1.88 (elimination), 14.40 and 2.91 mL/kg/min. These six numbers reproduce the paper's own triexponential to its printed precision. The steady-state volume is 1.7 L/kg and the terminal half-life 15 hours. Fifty mcg/kg followed by 0.5 mcg/kg/min holds the concentration above 100 ng/mL, about 140 ng/mL at 6 hours and 190 ng/mL at 16 hours.

Butterworth and colleagues (*Anesth Analg* 1995;81:783-792) studied the same setting but sampled for only 6 hours, and their three-compartment fit could not identify the elimination clearance. Their observation that cardiac index peaked 5 minutes after a one-minute bolus gives the time to peak effect used for the effect site here.

### What is plotted

Plasma milrinone in ng/mL, and the effect site, which peaks 5 minutes after a bolus. The band is the 100-300 ng/mL target range used in adults and children, and the typical value, 167 ng/mL, is Bailey's concentration for a 50% rise in cardiac index after bypass. No cardiac index or blood pressure is drawn.

### Where to be careful

About 80% of milrinone leaves unchanged in the urine, and its clearance falls with creatinine clearance, but Bailey's patients had normal renal function and the model has no renal term: in renal impairment it will underpredict the concentration, possibly several-fold. [Milrinone in children](help:drugs/milrinonePediatric) (Hornik 2019) carries a renal term. Every parameter is per kilogram, applied to the fat-free-mass weight when [the scaling switch](help:models/fat-free-mass) is on. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
