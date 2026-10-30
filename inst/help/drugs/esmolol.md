### The model

Esmolol's parameters come from de Bruijn, Reves and colleagues at Duke (*Anesthesiology* 1987;66:323-326): 19 patients anaesthetised for coronary artery bypass grafting, all on chronic beta blockers, given esmolol by peripheral vein in a stepped regimen for about 55 minutes, with venous blood drawn from the internal jugular vein and assayed by GC-MS. Thirteen patients' decay curves gave two compartments with mean half-lives of 1.34 and 9.9 minutes, a clearance of 128 mL/kg/min and a central volume of 0.413 L/kg. Built from those four, the model has a peripheral volume of 0.567 L/kg; the paper's V-beta, 1.90 L/kg, is the check, and the model's 1.83 is 4% from it.

### What is plotted

Esmolol in **whole blood**, mcg/mL, as assayed, for doses of esmolol hydrochloride as labelled. Because red-cell esterases hydrolyse esmolol, where the blood is drawn matters: the paper cites an animal study in which arterial concentrations were three to seven times peripheral venous. These samples were central venous. The clearance is less than half that of Sum and colleagues' awake volunteers (285 mL/kg/min, sampling site unreported), which the authors put down to anaesthesia and chronic beta blockade.

The effect site peaks **2 minutes** after a bolus, the time to the largest fall in heart rate after 1 mg/kg given over 30 seconds to healthy volunteers (Krumpl and colleagues, *Eur J Clin Pharmacol* 2017;73:417-428). Reves and colleagues (*J Thorac Cardiovasc Surg* 1990;100:221-227), reading heart rate every 15 seconds in anaesthetised coronary surgical patients after 80 mg, put the peak at 1.6 minutes. Slowing of AV conduction comes later: Sintetos and colleagues (*Clin Pharmacol Ther* 1987;41:112-117) found the PR interval longest 6 to 10 minutes after a bolus, when blood esmolol was already negligible. Heart rate is the endpoint used; no heart rate is drawn.

### Covariates

None in the source; every parameter is per kilogram, applied to the fat-free-mass weight when [the scaling switch](help:models/fat-free-mass) is on.

### Where to be careful

Means of thirteen patients, not a population model, and the individual parameters scatter widely (the central volume's coefficient of variation is 75%; for four of the thirteen the printed values admit no compartmental model at all). No heart rate is predicted. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
