### The model

Metoprolol's parameters come from Lammers and colleagues in Amsterdam (*Clin Pharmacokinet* 2017;56:1231-1244), who gave twelve healthy young men a cocktail of probe drugs, including metoprolol 20 mg intravenously on one occasion and a 100 mg tablet on another, and fitted the plasma concentrations in NONMEM. Two compartments describe them: clearance 65.8 L/h, central volume 187 L, intercompartmental clearance 74.5 L/h and peripheral volume 97.6 L, with first-order oral absorption (ka 1.19 per hour) and an oral bioavailability of 45%. The terminal half-life is about 3.4 hours. These are the values after an overnight fast; a 36-hour fast raised clearance 13%, which is not applied.

### CYP2D6

Metoprolol is cleared mainly by CYP2D6, and the **CYP 2D6** field of the Patient Profile changes it. In this study one poor metaboliser had 0.44 times the typical clearance and 2.28 times the bioavailability (so all of an oral dose reaches the circulation), and one ultrarapid metaboliser had 2.01 times the clearance and 0.43 times the bioavailability. Each factor rests on a single man, and the authors fixed them, so take them as direction and rough size. Intermediate and normal metabolisers are both the typical subject, as they were in the study.

### What is plotted

Plasma metoprolol in ng/mL, for doses of metoprolol tartrate as labelled, intravenous (mg) or oral (mg PO, and mg PO bid for twice-daily dosing). No beta-blockade, heart rate or blood pressure is drawn. The effect site peaks **5 minutes** after a bolus. After 4 mg intravenously in patients at cardiac catheterisation, Ritchie and colleagues (*J Pharm Sci* 1998;87:177-182) observed the peak effects 5 to 10 minutes after the bolus; their effect compartment, fitted to the drug's content in the heart rather than plasma, put the heart-rate peak at 3.3 minutes with wide uncertainty and the PR interval and contractility at about 7 minutes. The label puts maximal beta blockade about 20 minutes after a 10-minute infusion. Heart rate is the endpoint, at the start of the observed range.

### Where to be careful

Twelve healthy men aged 19 to 33 years: no patients, no women, no elderly, no liver disease. The parameters are fixed values, scaled to fat-free mass when [the scaling switch](help:models/fat-free-mass) is on. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
