### Research model — not for clinical dosing

This is a research and teaching model of plasma concentration. It is **not** validated clinical decision support: a simulated concentration is not an activated clotting time, an aPTT, a bleeding or clotting risk, or a safe time for a procedure or a neuraxial block.

### The model

Argatroban kinetics are from Cox and colleagues (*J Clin Pharmacol* 2004;44:981-990), a two-compartment intravenous model fitted to 152 patients undergoing percutaneous coronary intervention who received argatroban (a 250–300 mcg/kg bolus then a 15 mcg/kg/min infusion) together with a platelet glycoprotein IIb/IIIa antagonist (abciximab or eptifibatide). The clearance is 22.0 L/h at the 87 kg population median, the central volume 11.0 L, the peripheral volume 13.0 L and the inter-compartmental clearance 10.0 L/h; the half-lives are about 13 and 86 minutes. Doses are given intravenously as a bolus and/or an infusion, in micrograms or micrograms per kilogram.

### Covariates

Body weight is the only covariate the final model kept: it enters clearance linearly and additively about the 87 kg median (CL = 22.0 + 0.018 × (weight − 87) L/h), a small effect — clearance moves only from about 21.7 to 22.2 L/h across 70–100 kg. Weight is evaluated at the pharmacokinetic weight under the [fat-free-mass switch](help:models/fat-free-mass). Age appeared to affect the central volume in screening but was not retained, and race and sex had no effect.

### The patient this represents

The population is PCI patients on a GP IIb/IIIa antagonist, with normal-to-mildly impaired hepatic function; the paper notes the parameters are consistent with argatroban in healthy subjects, so the co-administered antagonist is not expected to change the kinetics. Argatroban is cleared by the liver, and the label reports markedly reduced clearance in moderate hepatic impairment; this model has **no hepatic covariate**, so in a patient with liver disease it will under-predict concentrations. There is no renal covariate, because argatroban is not renally cleared.

### What is not plotted

The study also fitted the activated clotting time as a sigmoid Emax function of plasma argatroban, but that is not plotted here: the ACT depends on the device and reagent, the fit is the study's own, and a simulated ACT is not a safe-to-proceed signal. The model has no effect site, no therapeutic band and no time-until-threshold.
