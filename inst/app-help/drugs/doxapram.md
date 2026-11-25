### The model

Doxapram's pharmacokinetics come from Clements, Robson and Prescott (*Eur J Clin Pharmacol* 1979;16:411-416), who gave 1.5 mg/kg doxapram hydrochloride over 2 minutes to six healthy men (63 to 72 kg) and sampled venous plasma for 24 hours. Three compartments fitted better than two. The paper prints only one subject's complete curve, Subject 2's, and only means for the rest, and those means do not belong to any single three-compartment model. So the model is Subject 2's curve, converted exactly to three compartments: a central volume of 0.44 L/kg, peripheral volumes of 0.25 and 0.90 L/kg, and a clearance of 5.2 mL/min/kg. Its half-lives are 14 minutes, 66 minutes and 6.4 hours.

The rapid fall after a single injection is distribution, not elimination: doxapram leaves the plasma for the tissues within minutes, which is why a bolus lasts only 5 to 12 minutes (label). The terminal half-life is about 6 hours, so during an infusion the concentration keeps rising for many hours. Clements found that a constant infusion would take 9 to 21 hours to reach 90% of its plateau, which is why he and later authors give doxapram as a fast infusion stepped down over the first hour.

### Why one subject, and how far to trust him

Subject 2 is not the average subject: his clearance is about a fifth below the group's mean and his first phase is slower. He was chosen because his curve predicts other studies, ones it was not fitted to, better than the alternatives do:

- **Robson & Prescott 1979** (probably the same volunteers): the area after the same bolus was 272 ± 101 μg·min/mL; the model gives 289. At the end of a 2-hour infusion it predicts 5.3 to 5.7 μg/mL against a measured 4.0 ± 1.1.
- **Komatsu and colleagues 2005** (nine volunteers, Clements' stepped regimen doubled): 2.4 to 2.6 μg/mL measured over the hours of the study; the model gives 2.5 to 2.75 from the third hour on.
- **Mattila & Larma 1987** (nine patients after abdominal surgery, 1 mg/kg then 1 mg/kg/h): 1.5 μg/mL at 2 hours and 1.21 at 4 hours; the model gives 2.0 and 2.3. Every model overpredicts these patients, whose mean concentration fell during a constant infusion.

The only modern population model, **Roozekrans and colleagues 2017** (nine healthy men, arterial samples, two compartments), describes the first hour or two well but was sampled for only 3 hours, so it has no terminal phase, and it overpredicted the infusions above by a third to a half. It is not used for the plasma curve.

In patients, concentrations run from about half to twice what this model predicts. Robson & Prescott's patients in respiratory failure reached 6.6 to 6.8 μg/mL on 1.5 mg/min, about twice the prediction. Mattila's postoperative patients, and one patient with a half-life of 155 minutes while taking carbamazepine, were lower. Doxapram is metabolized by CYP3A4: inducers lower its concentration and strong inhibitors would be expected to raise it.

### What is plotted

Plasma doxapram in μg/mL. Doses are entered as labelled, as doxapram hydrochloride (Dopram is the hydrochloride monohydrate). The model is built from the measured concentrations after a dose of the hydrochloride, so no salt conversion is needed. The papers converted their doses to base for their own volumes and clearances, with two different factors: that changes the printed parameters, not the curve. The active metabolite, keto-doxapram (AHR 5955), is not plotted.

### The effect site

Roozekrans and colleagues fitted an effect compartment for doxapram's rise in **cardiac output**, with an equilibration half-life of 3.2 ± 2.0 minutes on their own model. A bolus through their model peaks at the effect site **5.8 minutes** after the injection, and that time to peak is used here: it is the one thing an effect site carries from one pharmacokinetic model to another. Three cautions:

- the endpoint is cardiac output, not breathing;
- no one in that study received a bolus (every dose was a 9- or 20-minute loading infusion);
- the uncertainty puts the time to peak anywhere from about 3.5 to 8 minutes.

The label says respiratory stimulation begins 20 to 40 seconds after an injection and peaks at 1 to 2 minutes. The model follows the later, measured study rather than the label.

### The band

No therapeutic range is established. The band, 1.5 to 3 μg/mL with a typical value of 2, is the library's own. Clements designed his infusion to hold 2 μg/mL, at which, he wrote, minute ventilation roughly doubles (citing Calverley 1978). Komatsu cites Calverley 1983 for 1.5 to 3.0 μg/mL as the range that effectively increases minute ventilation. Neither Calverley paper was read. One of Robson & Prescott's volunteers stopped his infusion for agitation at 3.75 μg/mL.

### Covariates

None was estimated. The parameters are per kilogram; with [the scaling switch](help:models/fat-free-mass) on they are scaled to fat-free mass, and with it off linearly in weight.

### Where to be careful

The label lists contraindications that matter around anaesthesia: neuromuscular blockade and other mechanical limits to ventilation, epilepsy, head injury or stroke, severe hypertension or coronary disease, and use with mechanical ventilation. Doxapram can mask residual neuromuscular block, narcosis can return after it wears off (observe for half an hour to an hour), and after a volatile anaesthetic that sensitizes the heart to catecholamines it can provoke arrhythmias. Its pressor effect adds to that of sympathomimetics and MAO inhibitors, and a rapid infusion can cause haemolysis. The label's limits are 1.5 mg/kg per injection, 2 mg/kg in total by injection, 4 mg/kg by infusion after anaesthesia, and 3 g in 24 hours. In an elderly postoperative population, a nocturnal infusion trial (Rosenberg 1996) was stopped after a brain-stem infarct, with three of nine patients on doxapram having adverse events.

This is not a neonatal model. Doxapram's pharmacokinetics in preterm infants (Flint 2021, Ogawa 2015) depend on gestational age, postnatal age and serum AST, and the US product, which contains benzyl alcohol, is labelled not for use in neonates.

No ventilation, blood pressure or cardiac output is predicted.
