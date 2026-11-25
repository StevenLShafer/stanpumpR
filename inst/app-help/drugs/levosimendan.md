### The model

Levosimendan's parameters come from Jonsson and colleagues in Uppsala (*Br J Clin Pharmacol* 2003;55:544-551). They pooled two efficacy studies in 190 patients with moderate to severe heart failure (NYHA III and IV), aged 30 to 84 and weighing 46 to 130 kg. The patients were given a 5- or 10-minute loading dose, then infusions of 0.05 to 0.6 mcg/kg/min for 6 to 48 hours. About ten plasma samples were taken from each patient over 60 hours.

A population fit gave two compartments. For a 76 kg patient:
- clearance 13.3 L/h, rising 1.5% per kg of body weight
- central volume 16.8 L, rising 0.9% per kg
- intercompartmental clearance 2.33 L/h
- peripheral volume 30.0 L

Weight was the only covariate retained. Sex, age, digoxin, beta blockers, bilirubin and creatinine clearance were tested and dropped.

The parent falls with a half-life of about 45 minutes, which carries 99% of a bolus; Jonsson's patients' individual terminal half-lives averaged 0.84 hours. The typical parameters also give a small slow tail (half-life about 11 hours), so after a day's infusion about a tenth remains 6 hours after the stop.

### What is plotted, and what is not

Levosimendan itself in **plasma**, ng/mL. Its haemodynamic effect lasts days after the infusion stops. That comes from the active metabolite OR-1896, which forms slowly (half-life about 70 to 80 hours), and stanpumpR does not draw it yet. So the curve here falls within hours while the effect persists, and there is no effect site and no therapeutic range.

The metabolite waits for Kivikko and colleagues' study of OR-1896 after a 24-hour infusion, and for a way to calculate a metabolite formed from another metabolite.

### Body size

Weight enters clearance and central volume linearly, as Jonsson fitted it, within the 46-130 kg the study included. Outside that range stanpumpR continues from the nearest edge in proportion to weight (clearance by weight<sup>0.75</sup>), because the straight line would reach zero clearance at about 9 kg. No child was studied. The intercompartmental clearance and peripheral volume carry no weight term in the source and follow fat-free mass when [the scaling switch](help:models/fat-free-mass) is on.

### Where to be careful

- Levosimendan is not approved in the United States (it is sold as Simdax elsewhere), so there is no US label.
- The variability is large: 39% between patients in clearance and 60% in central volume.
- Bertin and colleagues (*Clin Pharmacokinet* 2026) modelled levosimendan and both metabolites in 21 critically ill patients on VA-ECMO, including six neonates and infants. That model is recorded but not used.

See [the vasoactive and resuscitation drugs](help:models/vasoactive).
