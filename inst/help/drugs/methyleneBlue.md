### The model

No published compartmental model of intravenous methylene blue rests on a modern assay, so this one is **derived** rather than fitted. The ProvayBlue label (NDA 204630, revised 12/2017, section 12.3) reports, after 2 mg/kg given over 5 minutes, a mean peak plasma concentration of 2917 ng/mL, an AUC of 13 977 ng·h/mL and a steady-state volume of 255 L. The European Proveblue summary of product characteristics (section 5.2) gives the terminal half-life, 24.7 hours. A two-compartment model has four parameters, and those four numbers determine it exactly. At 70 kg the result is a central volume of 46.8 L, a peripheral volume of 208 L, a clearance of 10.0 L/h and an intercompartmental clearance of 17.8 L/h. Its half-lives are 1.1 hours and 24.7 hours, and 30% of the area lies in the faster phase.

An older study (Peter 2000, whole blood) fitted three exponentials with much faster early phases, but its clearance is about eighteen times the label's, and its shape cannot be reconciled with the label's numbers, so it was not used.

### The check

Walter-Sack and colleagues (*Eur J Clin Pharmacol* 2009;65:179-189) gave 50 mg over 9.5 minutes to 16 healthy volunteers (mean 67 kg) and measured plasma methylene blue by LC-MS/MS for 24 hours. Their study did not enter the derivation. The model predicts their plasma AUC **a third low** (5208 against 7639 ng·h/mL). In clearance terms that is 0.143 against 0.098 L/h per kg. The gap cannot come from their shorter sampling, which would have pushed their clearance up, not down. The two studies used different doses (2 mg/kg against about 0.75 mg/kg), and whether clearance depends on dose is not known. The model follows the label, which studied the clinical dose. Concentrations may be about a third higher than drawn. A second study points the other way. American Regent's NCT02478281 (12 volunteers, 1 mg/kg, sampled for 72 hours, results posted on ClinicalTrials.gov) found a clearance of 0.25 L/h per kg, nearly twice the label's. Its peak per milligram was less than half any other source's, which suggests a difference in assay or sample handling rather than in the drug: methylene blue binds to plastic. The label's clearance lies between the two studies.

### What is plotted

Total plasma methylene blue in ng/mL; leucomethylene blue and the metabolite azure B are not plotted. The label does not name the matrix of its peak and AUC. A search excerpt of an FDA review, not yet read here, describes them as whole-blood values with an extrapolated AUC of 15 668 ng·h/mL. If that is confirmed, the curve will be relabelled as whole blood and rederived. Doses are methylene blue (methylthioninium chloride) as labelled, in mg/kg or mg, as a bolus or an infusion.

### Covariates

None. The label doses per kilogram but does not give its subjects' weight, so 70 kg is assumed. With [the scaling switch](help:models/fat-free-mass) on, the 70-kg values are scaled to fat-free mass; with it off, linearly in weight. Renal impairment raises exposure: the summary of product characteristics reports the AUC to 96 hours 52%, 116% and 192% higher in mild, moderate and severe impairment. The half-life was unchanged except in severe impairment, so this is not a clearance change alone, and no equation is given. No renal term is applied, and the Serum creatinine field does not affect this drug. Hepatic impairment had no consistent effect.

### Where to be careful

**The first half hour.** In Walter-Sack's study, whole blood peaked 10 minutes after the injection, but plasma peaked 30 minutes after it. The label's blood-to-plasma ratio of 5.1 at 5 minutes tells the same story: methylene blue enters red cells first and returns to the plasma. No compartmental model with plasma as its central compartment rises after an infusion stops, so the curve here is too high at the end of a short infusion. Its first half hour does not have the measured shape.

There is no therapeutic concentration range, so no band is drawn. No time to peak effect has been measured for the vasopressor or the methaemoglobin-reducing effect, so no effect site is drawn. No blood pressure or methaemoglobin level is predicted. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
