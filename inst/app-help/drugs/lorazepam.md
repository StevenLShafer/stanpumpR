### The model

Lorazepam's disposition is the two-compartment model of Barr and colleagues (*Anesthesiology* 2001;95:286-298), fitted to arterial concentrations in 14 patients given lorazepam by target-controlled infusion for 12 to 72 hours of sedation after major surgery. The patients were older men (mean age 74 years, weight 85 kg, height 176 cm; 13 men and 1 woman). No covariate improved the fit: central volume 40.8 L, peripheral volume 102 L, clearance 0.107 L/min (6.4 L/h) and intercompartmental clearance 1.86 L/min. The elimination half-life is 952 minutes, about 16 hours.

Until November 2026 the disposition was built from the means reported by Nielsen-Kudsk and colleagues (*Acta Pharmacol Toxicol* 1983;52:121-127) in 6 young healthy volunteers given a single 2 mg dose: for 70 kg, central volume 41.3 L, peripheral volume 45.0 L, clearance 4.35 L/h and intercompartmental clearance 46.9 L/h. Barr's model replaced it because it was fitted to the patients lorazepam infusions are given to, from far larger doses followed for longer. Its clearance agrees with the 109 mL/min that Greenblatt and colleagues measured in young men (*Crit Care Med* 2000;28:2750-2757). For the reference man (below) the clearance is 35% higher than before and the peripheral volume twice as large.

Barr's patients distribute lorazepam faster and into a larger volume than the volunteers whose curves the labels describe, so the reference man now falls below the labels: 4 mg intravenously gives 59 ng/mL at 15 minutes (label: about 70 initially), 2 mg by mouth peaks at 13.8 ng/mL (label: about 20) and 4 mg intramuscularly at 42 ng/mL (label: about 48). The same disposition is used for every route, including oral doses in patients who are not in an intensive care unit.

Two other models were checked and set aside. Gonzalez and colleagues' model (*Clin Pharmacokinet* 2017;56:941-951) was fitted in children. Swart and colleagues' model of critically ill adults (*Br J Clin Pharmacol* 2004;57:135-145) has a central volume of 0.74 L, which long infusions cannot identify; it would put a 2 mg bolus at 2700 ng/mL.

### Routes

- **Intravenous**: bolus or infusion.
- **Oral**: absorption half-life 32.5 minutes and bioavailability 0.90 (Greenblatt and colleagues, *J Pharm Sci* 1982;71:248-252, and the label). Sublingual lorazepam is absorbed almost exactly as oral tablets are (half-life 28.5 minutes, 94 to 98 per cent), so enter a sublingual dose as `mg PO`.
- **Intramuscular**: absorption half-life 14.2 minutes, bioavailability 0.96 (Greenblatt and colleagues, 1982).
- Intranasal lorazepam (bioavailability 78 per cent) has no published absorption rate and is not offered.

### Covariates

None in the source, whose values belong to its typical patient. With the [fat-free-mass switch](help:models/fat-free-mass) on, they are anchored at Barr's cohort, so that an 85 kg, 176 cm, 74-year-old receives Barr's values: the volumes scale with fat-free mass and the clearances with its 0.75 power, and the 70 kg reference man receives a central volume of 36.1 L, a peripheral volume of 90.2 L, a clearance of 5.85 L/h and an intercompartmental clearance of 1.70 L/min. With the switch off, every patient receives Barr's values. Greenblatt and colleagues found clearance about a fifth lower in the elderly; Barr's patients, aged 74 ± 6 years, were too alike in age to show it. In critically ill patients, Swart found clearance fell with PEEP and was much lower with alcohol abuse. None of these is modelled.

### Effect site

The time to peak effect is 18 minutes. It comes from the EEG beta response in 9 volunteers given a bolus and a 4-hour infusion (Greenblatt and colleagues, *Crit Care Med* 2000;28:2750-2757): an equilibration half-life of 8.8 minutes, which on this model's bolus curve puts the effect-site peak at 18 minutes (on Greenblatt's own plasma curve, about 19). Barr's sedation model has no effect site of its own. On the earlier Nielsen-Kudsk disposition, whose distribution was slower, the same equilibration half-life put the peak at 26 minutes. The EEG effect was greatest 30 minutes after the loading dose, with the infusion running. Slower equilibration has been found for a psychomotor tracking test after oral doses (half-time 0.43 hours; Gupta and colleagues, *J Pharmacokinet Biopharm* 1990;18:89-102). Lorazepam reaches its peak effect much later than midazolam, so a repeat bolus given before the first has peaked will overshoot.

### Typical concentrations

The shaded band and threshold come from Barr and colleagues. They estimated the steady-state plasma concentrations at which a postoperative ICU patient has an even chance of a Ramsay score of at least 2, 3, 4, 5 and 6: 34, 51, 104, 152 and 188 ng/mL. The band is moderate sedation, a Ramsay score of 3 or 4, which Barr targeted and the library draws for the sedatives: it runs from 51 to 152 ng/mL, where the median patient scores 3 or 4, with the typical value at 104, an even chance of a score of at least 4. It was 34 to 104 ng/mL. The default **time-until-threshold** level is also **51 ng/mL**, the concentration below which a typical patient is more likely than not to emerge from light sedation. Anticonvulsant and anxiolytic concentrations are lower, about 20 to 30 ng/mL. Barr's patients received fentanyl or epidural morphine, and age and opioids both changed the sedation. The AGNP consensus guidelines' 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5) gives 5 to 100 ng/mL, 12 hours after a dose, for anxiety disorders on oral doses; it is not drawn.

### Where to be careful

Barr's models predict that, for a 71-year-old, 70 kg man, emergence after a 72-hour infusion takes 11.9 hours from light and 31.1 hours from deep sedation with lorazepam, against 3.6 and 14.9 hours with midazolam; in their patients, emergence to a sedation score of 2 took 8.7 hours with lorazepam against 3.0 with midazolam. The injection's propylene glycol vehicle accumulates during high-dose infusions and is not modelled. Lorazepam adds to the respiratory depression of opioids.
