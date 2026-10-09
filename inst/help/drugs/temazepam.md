### The model

No two-compartment model of temazepam has been published, so this one was fitted for stanpumpR to the published means of two intravenous studies:

- van Steveninck and colleagues (*Clin Pharmacol Ther* 1994;55:546-555) infused 0.4 mg/kg over 30 minutes into 9 healthy young volunteers, twice. They reported a peak of about 1000 ng/mL, areas under the curve to 3 hours, 8 hours and infinity of 1.4, 2.8 and 6.2 µg·h/mL, and a half-life of 10.5 hours.
- Halliday and colleagues (*Br J Anaesth* 1987;59:465-467) gave 11 volunteers 20 mg over 20 seconds and plotted the means for 2 hours.

Scaled to the same dose, Halliday's concentrations were about 1.6 times van Steveninck's in the first hours. A single model was fitted to both. Per kilogram, the central volume is 0.278 L/kg, the peripheral volume 0.523 L/kg, the clearance 1.10 mL/min/kg and the intercompartmental clearance 0.112 L/h/kg. For 70 kg, that is volumes of 19.5 and 36.6 L, a clearance of 4.63 L/h and an intercompartmental clearance of 7.8 L/h.

The half-lives are 0.88 and 10.8 hours. The model matches van Steveninck's total exposure and half-life within 4 per cent and Halliday's concentrations from 10 minutes on within 13 per cent. It sits above van Steveninck's early concentrations and 20 per cent below Halliday's at 5 minutes. A fit to van Steveninck alone put Halliday's concentrations 35 to 44 per cent low, and most of the oral peaks too.

The fit minimised the squared logarithms of the ratios of model to observed values: van Steveninck's five summary values and Halliday's seven mean concentrations, each with equal weight, with each study simulated at its own dose, infusion time and mean weight. Halving or doubling the weight given to Halliday changes the predicted oral peak by about 5 per cent. The full derivation, with the data, the alternatives tried and a script that reproduces the fit, is in the developer document `docs/temazepam.md`.

The specification this model was built from proposed a one-compartment reduction of Ochs and colleagues' oral data (1.45 L/kg, 2.33 mL/min/kg). Spreading 30 mg through that volume gives at most about 300 ng/mL, against the label's 865, so a second compartment is needed.

### Oral only

The intravenous formulation was a research solution, and earlier intravenous temazepam caused venous thrombosis; there is no product. Temazepam is offered by mouth (`mg PO`, and `mg PO qd` for a nightly dose), with bioavailability 0.92 (the label's 8 per cent first-pass loss). Absorption follows Müller and colleagues (*Eur J Clin Pharmacol* 1987;33:211-214): an absorption half-life of 0.38 hours for a soft gelatin capsule taken in the morning. The same capsule taken at night was absorbed more slowly, with a half-life of 0.53 hours and a peak about 30 per cent lower; that is not modelled.

In the reference man, 20 mg peaks at 545 ng/mL at 55 minutes, and 30 mg at 818 ng/mL. Published single-dose peaks vary twofold with formulation and time of day: 362 to 708 ng/mL after 20 mg, and 560 to 865 ng/mL after 30 mg capsules (the label: 865). After 30 mg every night for a week, the model gives 217 ng/mL 9 hours after a dose and 82 ng/mL at 24 hours; the label reports 260 ± 210 and 75 ± 80.

### Covariates

None in the source, which dosed by weight. With the [fat-free-mass switch](help:models/fat-free-mass) on, the volumes scale with fat-free mass and the clearances with its 0.75 power; with it off, all scale with weight. Women clear temazepam about a quarter more slowly than men (Divoll and colleagues, *J Pharm Sci* 1981;70:1104-1107); that is not modelled beyond body size.

### Effect site

None; only the plasma concentration is plotted. van Steveninck and colleagues found no equilibration delay: saccadic eye velocity and EEG beta activity were linear in the plasma concentration, and their concentration-effect plots showed proteresis, the effect waning while the concentration was still high, rather than the lag an effect site produces (*Clin Pharmacol Ther* 1994;55:535-545).

### Typical concentrations and the threshold

The shaded band runs from 250 to 600 ng/mL. Psychometric performance deteriorated above about 250 ng/mL (Saletu and colleagues, *Acta Psychiatr Scand Suppl* 1986;332:67-94). van Steveninck chose about 600 ng/mL as a concentration that sedated awake volunteers clearly without putting them to sleep. The typical value, 400 ng/mL, lies between the two. The default **time-until-threshold** level is **250 ng/mL**, read against plasma: after 20 mg the reference man falls below it 3.4 hours after the dose, and after 30 mg 5.2 hours after.

### Where to be careful

This is a model fitted to published means, not a published model; the two intravenous studies behind it disagree with each other by about 1.6-fold, and the oral studies by up to twofold. Temazepam adds to the sedation and respiratory depression of opioids and other sedatives. Its binding to plasma proteins varies with free fatty acids, so the free fraction, and possibly the effect at a given total concentration, changes over hours.
