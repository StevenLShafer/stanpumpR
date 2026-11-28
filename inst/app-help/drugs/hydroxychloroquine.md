### The model

Hydroxychloroquine (Plaquenil) is taken by mouth for rheumatoid arthritis and lupus, and to prevent and treat malaria. This is the model of Carmichael, Charles and Tett (*Ther Drug Monit* 2003;25:671-681), fitted to 123 adults with rheumatoid arthritis in Sydney, 88 of them women. Nine took a single tablet and, on another day, an intravenous infusion, and were sampled every 15 minutes for 8 hours. The other 114 gave only trough samples, about 24 hours after a dose, over up to six months of treatment.

The model has one compartment, with first-order absorption after a lag:

- clearance 9.89 L/h;
- volume of distribution 605 L;
- absorption rate 0.765 /h after a lag of 0.445 h;
- bioavailability 0.746, measured in the nine patients given both routes.

### Read this before using the curve

**The concentrations are in whole blood.** Hydroxychloroquine concentrates in blood cells, and a whole-blood level is several times a plasma or serum level: after one 200 mg tablet the label gives a peak of 129.6 ng/mL in blood and 50.3 ng/mL in plasma. Do not compare the curve with a level measured in serum or plasma.

**The average at steady state is right. The time course is too fast.** The steady-state average depends only on clearance and bioavailability, and the model reproduces the paper's own values: 487 ng/mL on 200 mg a day and 974 ng/mL on 400 mg a day. The rise to that level and the fall after stopping are another matter. Hydroxychloroquine has several phases, and its terminal half-life in whole blood is 40 to 50 days (the label). A one-compartment model fitted mostly to troughs sees only a middle phase, and its half-life is 42 hours. So the curve reaches steady state in about 9 days, where patients take weeks to months, and it falls to nothing within two weeks of the last dose, where the drug stays in the blood for months. Use the curve for the level a regimen settles at, not for how long it takes to get there or to wash out.

The first hours after a single dose are also only roughly right. The label gives a whole-blood peak of 129.6 ng/mL, 3.3 hours after one 200 mg tablet in healthy men. The model gives 176 ng/mL at 5.6 hours.

### Doses

Doses are entered as labelled, in mg of hydroxychloroquine sulfate. A 200 mg tablet holds 155 mg of hydroxychloroquine base. The paper gives its doses as the base, so the model converts each dose (× 155/200), and the concentrations plotted are of the base, as the assay measures it.

The label's adult doses are 400 to 600 mg a day at first for rheumatoid arthritis and 200 to 400 mg a day afterwards, 200 or 400 mg a day for lupus, 400 mg once a week to prevent malaria, and for treating malaria 800 mg, then 400 mg at 6, 24 and 48 hours. Children from 31 kg are dosed per kilogram (`mg/kg PO`). The label warns that daily doses above 5 mg/kg of actual body weight raise the risk of retinopathy.

Only the oral route is offered. There is no intravenous product in the United States.

### Covariates

The paper tested only one covariate, methotrexate. Weight was missing for a third of the patients, so no effect of body size could be tested. With [the scaling switch](help:models/fat-free-mass) on, clearance and volume are scaled to the patient's fat-free mass from the typical patient of the paper's cohort (about 70 kg, 55 years old, seven tenths women; heights are not printed and are taken from Australian population data). With the switch off, every patient receives the published values.

Patients who also took methotrexate had a volume of distribution 1070 L larger and the same clearance. This is not applied: the app has no field for it, the steady-state average does not depend on volume, and the authors suspect it reflects how that group was sampled.

The model has no term for renal or hepatic function. The label says a lower dose may be needed in renal or hepatic disease and gives no number. About 16 to 30% of a dose is excreted unchanged in urine.

### Pregnancy

Not applied. One study of serum levels in 22 pregnancies with lupus fitted an apparent clearance about 60% higher in the third trimester than after delivery (Balevic and colleagues, *Lupus Sci Med* 2022;9:e000602). An earlier study by the same group, with paired samples from 25 women, found no change in clearance. The authors recommend no change of dose, and note that missed doses lower the level more than pregnancy does.

### No band

No therapeutic range is established. The paper says so for rheumatoid arthritis, and the label says the relation between exposure and response has not been fully characterised. In lupus, a meta-analysis (Garg and colleagues, *Arthritis Care Res* 2021;73:707-716) found whole-blood levels of 750 ng/mL or more associated with less active disease. That threshold is recorded and not drawn. Blood levels are mostly used to find out whether doses are being missed, which is common: this curve [assumes every dose was taken](help:cautions).

### What is not plotted

Concentration only, in ng/mL of whole blood. No effect is drawn, and neither are the metabolites.
