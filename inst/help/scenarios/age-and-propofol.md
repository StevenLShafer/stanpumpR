## What to look for

A 25-year-old man, 70 kg, given propofol 2 mg/kg and 100 mcg/kg/min for an hour. Note the effect-site concentration at 5 minutes and at 60 minutes, and the time it takes to fall to the 1 mcg/mL threshold after the infusion stops.

Now open **Patient Profile**, change the age to **85**, and look again. Nothing else has changed. The concentrations are higher throughout, the peak after the bolus is higher, and the fall to threshold takes longer. The Eleveld model reduces the fast peripheral volume and the elimination clearance with age, so the same dose produces more concentration and clears more slowly.

This is the pharmacokinetic half of why the elderly need less propofol. The pharmacodynamic half, that the older brain is also more sensitive to a given concentration, is not in the plot: Schnider found the concentration for a given EEG effect fell with age too. The two effects multiply.

## Try next

- Try age 45 and age 65 to see that the change is gradual, not a step.
- Keep the age at 85 and reduce the doses until the curve matches what the 25-year-old had: a common rule of thumb is to halve the induction dose.
- Change the sex to female at either age. Eleveld's model gives women a higher clearance.

## Background

[Propofol](help:drugs/propofol) describes the Eleveld model and its covariates; [Covariates and body size](help:models/covariates) has the general equations. Age effects differ between drugs: the fentanyl and alfentanil models in the library have no age term at all, so the same experiment with them shows nothing.
