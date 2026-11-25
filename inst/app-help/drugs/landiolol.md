### The model

Landiolol's parameters come from Kunisawa and colleagues in Asahikawa (*Ther Clin Risk Manag* 2015;11:107-114). They studied eight patients aged 64 to 84 having peripheral arterial surgery, anaesthetised with propofol and remifentanil. Each was given landiolol by target-controlled infusion to plasma targets of 500 and then 1,000 ng/mL, for 30 minutes each, and 112 arterial samples were assayed. A population fit gave two compartments per kilogram:
- clearance 30.7 mL/min/kg
- central volume 65.0 mL/kg
- intercompartmental clearance 48.3 mL/min/kg
- peripheral volume 54.4 mL/kg

The fit also had a lag of 0.63 minutes, which stanpumpR does not draw. The authors left it out of their own infusion pump too, so every curve here is about 38 seconds early.

### Why this source

Three population models of landiolol have been published, all in Japanese patients:
- **Honda and colleagues (2008):** 47 healthy men; clearance 36.6 mL/min/kg, central volume 101 mL/kg.
- **Kunisawa and colleagues (2014):** nine gynaecologic surgical patients; clearance 34.0 mL/min/kg.
- **Kunisawa and colleagues (2015):** this study.

Only the last is in stanpumpR's collection of source papers, so it is used for every patient for now. Its patients were elderly and anaesthetised, a population close to the perioperative patients stanpumpR is mostly used for.

The three clearances agree within 20%. The US label (Rapiblyk) gives a clearance of 57 mL/kg/min after a 20-hour infusion in healthy European volunteers. That figure was measured in whole blood rather than plasma, and in awake rather than anaesthetised subjects, which may explain why it is almost twice this model's.

### What is plotted

Landiolol in **arterial plasma**, mcg/mL. Doses are landiolol (base), as the US product is labelled: 9.3 mcg/kg/min of landiolol is 10 mcg/kg/min of landiolol hydrochloride.

The effect site peaks **2 minutes** after a bolus. That is the time of the lowest heart rate after 0.2 and 0.3 mg/kg given at 0.1 mg/kg per 15 seconds to healthy volunteers (Krumpl and colleagues, *Eur J Clin Pharmacol* 2017;73:417-428). Heart rate fell significantly from 1 minute, and stayed down for 9 to 29 minutes even though the drug's half-life is about 4 minutes. No heart rate is drawn.

### Covariates

None in the source: weight, lean body mass and age were tested and none was retained. Every parameter is per kilogram, applied to the fat-free-mass weight when [the scaling switch](help:models/fat-free-mass) is on.

### Where to be careful

- Eight patients is a small study, and they were elderly, anaesthetised, and on dopamine.
- Landiolol is hydrolysed by esterases in plasma. Its concentration depends on how the blood is handled: these samples were stopped in ethanol and neostigmine at the bedside.
- There is no therapeutic concentration range; landiolol is titrated to heart rate.

See [the vasoactive and resuscitation drugs](help:models/vasoactive).
