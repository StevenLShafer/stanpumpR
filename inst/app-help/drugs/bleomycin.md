### The model

Yee and colleagues (*Clin Pharmacol Ther* 1983;33:668-673) measured bleomycin by radioimmunoassay in 14 children with cancer at St. Jude Children's Research Hospital, 1 to 17 years (median 14.5), after intravenous bolus doses of 20 or 30 units/m² and prolonged infusions. After a bolus, bleomycin falls in two phases, with half-lives of about 25 minutes and 3.2 hours. Clearance is 51.8 mL/min per m² of body-surface area, two thirds of it through the kidneys.

stanpumpR builds the two-compartment model from the paper's clearance, central volume (4.3 L/m²), steady-state volume (9.9 L/m²) and terminal half-life. Its fast half-life, 25 minutes, is then a check against the printed 0.3 hours.

### Children and adults

The model was fitted in children, but it is offered at every age. Alberts (1978), studying adults with normal kidneys, measured the same clearance (50.6 mL/min/m²) and similar half-lives (24 minutes and 4 hours); the FDA label quotes 51 mL/min/m². Yee's authors conclude that bleomycin's disposition in older children "is as in adults". No adult study prints enough numbers to build a model of its own.

### What is plotted

Plasma bleomycin in microunits per mL. Bleomycin is dosed in USP units; one unit is the former "milligram activity", so 1 mcU/mL corresponds to about 1 ng/mL in the older literature. There is no therapeutic range, so none is drawn.

### Where to be careful

- **Normal kidneys only.** Bleomycin is cleared mostly by the kidneys, and clearance falls steeply in renal failure: to 18 mL/min/m² after more than 300 mg/m² of cisplatin, and to 4.8 in a child with obstructed ureters. The model has no renal term; the label advises lower doses below a creatinine clearance of 50 mL/min.
- **Children under 3.** Their clearance was higher (70.5 mL/min/m² in three courses); the curve uses the whole group's value.
- **Long infusions.** After 4- to 5-day infusions, Kramer (1978) saw a slow tail below 10 mcU/mL that a two-compartment model does not predict.
- **Pregnancy:** no pharmacokinetic data were found; nothing is changed.

**Routes not modelled.** The FDA label gives bleomycin intravenously, intramuscularly, subcutaneously and intrapleurally. Only the intravenous route is modelled here, because the model was fitted to intravenous doses. If you need a route stanpumpR does not offer, please contact Steven Shafer (steven.shafer@stanford.edu). For a one-time simulation in the meantime, a published pharmacokinetic study of that route can often be uploaded to the **Single Paper Review** tab, which reads its model and simulates it.
