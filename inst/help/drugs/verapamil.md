### The model

Intravenous verapamil's parameters come from Kates, Keefe, Schwartz, Harrison and colleagues at Stanford (*Clin Pharmacol Ther* 1981;30:44-51): twelve patients with chronic atrial fibrillation, all taking digoxin, given 15 mg at 1 mg/min and sampled for 24 hours. Every patient's curve was triphasic, and each was fitted to a three-compartment model whose rate constants and central volume the paper prints patient by patient. The model here is the mean of the twelve patients' per-kilogram volumes and clearances, which reproduces the paper's own summary exactly: a clearance of 12.5 mL/min/kg and a steady-state volume of 3.5 L/kg. Its half-lives are 2.6 minutes, 37 minutes and 4.8 hours.

One number in the paper's table is misprinted: patient 2's central volume is printed as 92.72 L, but that patient's own clearance and steady-state volume, printed beside it, require 9.272 L. The model uses the corrected value.

### Checked against a second study

Eichelbaum, Mikus and Vogelgesang in Bonn (*Br J Clin Pharmacol* 1984) gave racemic verapamil to three young healthy men and found a clearance of 14.3 mL/min/kg and a steady-state volume of 3.4 L/kg, within 15% of this model, the clearance somewhat higher, as would be expected without heart failure or age.

### Dose basis and covariates

Doses are verapamil hydrochloride, as labelled (the Bonn paper's own tables show its doses were the hydrochloride). Every parameter is per kilogram, applied to the fat-free-mass weight when [the scaling switch](help:models/fat-free-mass) is on.

### Where to be careful

Racemic verapamil is plotted. The (−)-isomer, about ten times as potent on AV conduction, is cleared faster and distributes more widely than the (+)-isomer, so the racemate's concentration is not the active concentration, and the balance between them differs after oral dosing. Norverapamil, which was not detected after the intravenous dose, is not plotted. Clearance falls with liver disease and reduced hepatic blood flow.

The effect site peaks **4 minutes** after a bolus, the midpoint of the label's "peak therapeutic effects occur within 3 to 5 minutes after a bolus injection", the slowing of the ventricular rate, as atropine's is its clinical time to peak heart rate. The PR interval lags much further: Berk and colleagues (*Pharmacotherapy* 1992;12:33-39) found it longest 28 ± 16 minutes after 5 mg in healthy men, and Carosella and colleagues (*Cardiovasc Drugs Ther* 1989;3:417-425) a hysteresis loop. No heart rate or conduction is drawn. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
