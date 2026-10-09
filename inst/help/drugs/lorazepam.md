### The model

Lorazepam's disposition is a two-compartment model built from the means reported by Nielsen-Kudsk and colleagues (*Acta Pharmacol Toxicol* 1983;52:121-127), who gave 6 young healthy volunteers intravenous and intramuscular lorazepam. They reported a distribution half-life of 0.31 hours, an elimination half-life of 14.1 hours, a central volume of 0.59 L/kg and a clearance of 62.2 mL/kg/h. The other two parameters follow exactly from these four. For 70 kg: central volume 41.3 L, peripheral volume 45.0 L, clearance 4.35 L/h and intercompartmental clearance 46.9 L/h. The clearance, about 1.04 mL/min/kg, lies in the middle of the published adult range.

Building a model from mean half-lives is an approximation. It is checked against the labels: in the reference man, 4 mg intravenously gives 74 ng/mL at 15 minutes (label: about 70 initially), 2 mg by mouth peaks at 19.7 ng/mL (label: about 20) and 4 mg intramuscularly at 53 ng/mL (label: about 48).

Two other models were checked and set aside. Gonzalez and colleagues' model (*Clin Pharmacokinet* 2017;56:941-951) was fitted in children. Swart and colleagues' model of critically ill adults (*Br J Clin Pharmacol* 2004;57:135-145) has a central volume of 0.74 L, which long infusions cannot identify; it would put a 2 mg bolus at 2700 ng/mL. Barr and colleagues' ICU model (*Anesthesiology* 2001;95:286-298) would be the natural replacement, once its full parameters are in hand.

### Routes

- **Intravenous**: bolus or infusion.
- **Oral**: absorption half-life 32.5 minutes and bioavailability 0.90 (Greenblatt and colleagues, *J Pharm Sci* 1982;71:248-252, and the label). Sublingual lorazepam is absorbed almost exactly as oral tablets are (half-life 28.5 minutes, 94 to 98 per cent), so enter a sublingual dose as `mg PO`.
- **Intramuscular**: absorption half-life 14.2 minutes, bioavailability 0.96 (Greenblatt and colleagues, 1982).
- Intranasal lorazepam (bioavailability 78 per cent) has no published absorption rate and is not offered.

### Covariates

None in the source, which reports its parameters per kilogram. With the [fat-free-mass switch](help:models/fat-free-mass) on, the volumes scale with fat-free mass and the clearances with its 0.75 power; with it off, all scale with weight. Clearance is about a fifth lower in the elderly. In critically ill patients, Swart found it fell with PEEP and was much lower with alcohol abuse. None of these is modelled.

### Effect site

The time to peak effect is 26 minutes. It comes from the EEG beta response in 9 volunteers given a bolus and a 4-hour infusion (Greenblatt and colleagues, *Crit Care Med* 2000;28:2750-2757): an equilibration half-life of 8.8 minutes, which on this model's bolus curve puts the effect-site peak at 26 minutes. The EEG effect was greatest 30 minutes after the loading dose. Slower equilibration has been found for a psychomotor tracking test after oral doses (half-time 0.43 hours; Gupta and colleagues, *J Pharmacokinet Biopharm* 1990;18:89-102). Lorazepam reaches its peak effect much later than midazolam, so a repeat bolus given before the first has peaked will overshoot.

### Typical concentrations

The shaded band and threshold come from Barr and colleagues. They estimated the steady-state plasma concentrations at which a postoperative ICU patient has an even chance of a Ramsay score of at least 2, 3 and 4: 34, 51 and 104 ng/mL. The band runs from 34 to 104 ng/mL, with the typical value at 51, the edge of the moderate sedation (Ramsay 3 to 4) Barr targeted. The default **time-until-threshold** level is also **51 ng/mL**, the concentration below which a typical patient is more likely than not to emerge from light sedation. Anticonvulsant and anxiolytic concentrations are lower, about 20 to 30 ng/mL. Barr's patients received fentanyl or epidural morphine, and age and opioids both changed the sedation.

### Where to be careful

Barr found emergence after a 72-hour infusion took 11.9 hours from light and 31.1 hours from deep sedation with lorazepam, against 3.6 and 14.9 hours with midazolam. The injection's propylene glycol vehicle accumulates during high-dose infusions and is not modelled. Lorazepam adds to the respiratory depression of opioids.
