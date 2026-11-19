### The model

Glycopyrrolate's parameters are from Bartels and colleagues (*Br J Clin Pharmacol* 2013;76:868-879), who identified systemic glycopyrronium disposition from an intravenous reference arm in healthy volunteers (120 mcg of active moiety over five minutes) alongside inhalation data; only the systemic compartments are used. Three compartments: clearance 44.9 L/h, central volume 11.3 L, intercompartmental clearances 25.6 and 8.23 L/h, peripheral volumes 19.0 and 71.5 L. Half-times 0.09, 0.81 and 7.2 hours; steady-state volume 102 L.

### Dose basis

The injection is labelled as glycopyrrolate **bromide** (molecular weight 398), while the model's dose and concentration are the active cation (318); a labelled milligram is 0.80 mg of cation. The engine takes the labelled dose, so the conversion is folded into the parameters: every volume and clearance in the table above is the published value divided by 0.80, which leaves the rate constants unchanged and makes the plotted concentration the **active cation in ng/mL** for a bromide-labelled dose.

### Covariates

None in the source. The parameters take the default [fat-free-mass scaling](help:models/fat-free-mass) and are used as published with the switch off. Renal impairment markedly reduces clearance but no continuous relationship exists to apply.

### Effect site

None. The label's onset cannot identify an equilibration constant, and heart rate, secretions and the vagal response to neostigmine each need their own calibrated relationship. Only the plasma concentration is plotted; the shaded band (1 to 10 ng/mL) covers what 0.2 to 0.4 mg produce during distribution, for about the first 30 minutes after 0.2 mg and the first hour after 0.4 mg; after that the concentration is below 1 ng/mL.

### Where to be careful

The long terminal half-time describes this model and its sampling sensitivity, not the duration of the antimuscarinic effect; an older small study with a different assay reported a much shorter one, and it has not been spliced in. The model is a healthy-volunteer fit of a 120 mcg dose, so the peak after a 0.4 mg bolus is an extrapolation.
