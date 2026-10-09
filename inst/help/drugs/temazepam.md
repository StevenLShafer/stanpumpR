### The model

No two-compartment model of temazepam has been published, so this one was fitted for stanpumpR to published means. van Steveninck and colleagues (*Clin Pharmacol Ther* 1994;55:546-555) infused 0.4 mg/kg intravenously over 30 minutes into 9 healthy young volunteers on two occasions six months apart. They fitted each subject separately but reported only summary values: a peak of about 1000 ng/mL, areas under the curve to 3 hours, 8 hours and infinity of 1.4, 2.8 and 6.2 µg·h/mL, and a half-life of 10.5 hours.

Two compartments fitted to those five means reproduce each within 1.5 per cent. Per kilogram, the central volume is 0.274 L/kg, the peripheral volume 0.607 L/kg, the clearance 1.04 mL/min/kg and the intercompartmental clearance 0.407 L/h/kg. For 70 kg that is 19.2 and 42.5 L, 4.38 L/h and 28.5 L/h. The fitted distribution half-life, 0.30 hours, is close to the 0.4 to 0.6 hours the label reports, which the fit was not given.

The specification this model was built from proposed a one-compartment reduction of Ochs and colleagues' oral data (1.45 L/kg, 2.33 mL/min/kg). Spreading 30 mg through that volume gives at most about 300 ng/mL, against the label's 865, so a second compartment is needed.

### Oral only

The intravenous formulation was a research solution, and earlier intravenous temazepam caused venous thrombosis; there is no product. Temazepam is offered by mouth (`mg PO`, and `mg PO qd` for a nightly dose), with bioavailability 0.92 (the label's 8 per cent first-pass loss). Absorption follows Müller and colleagues (*Eur J Clin Pharmacol* 1987;33:211-214): an absorption half-life of 0.38 hours for a soft gelatin capsule taken in the morning. The same capsule taken at night was absorbed more slowly, with a half-life of 0.53 hours and a peak about 30 per cent lower; that is not modelled.

In the reference man, 20 mg peaks at 392 ng/mL at 41 minutes, and 30 mg at 588 ng/mL. Published single-dose peaks vary twofold with formulation and time of day: 362 to 708 ng/mL after 20 mg, and 560 to 865 ng/mL after 30 mg capsules. After 30 mg every night for a week, the model gives 278 ng/mL 9 hours after a dose and 103 ng/mL at 24 hours; the label reports 260 ± 210 and 75 ± 80.

### Covariates

None in the source, which dosed by weight. With the [fat-free-mass switch](help:models/fat-free-mass) on, the volumes scale with fat-free mass and the clearances with its 0.75 power; with it off, all scale with weight. Women clear temazepam about a quarter more slowly than men (Divoll and colleagues, *J Pharm Sci* 1981;70:1104-1107); that is not modelled beyond body size.

### Effect site

None; only the plasma concentration is plotted. van Steveninck and colleagues found no equilibration delay: saccadic eye velocity and EEG beta activity were linear in the plasma concentration, and their concentration-effect plots showed proteresis, the effect waning while the concentration was still high, rather than the lag an effect site produces (*Clin Pharmacol Ther* 1994;55:535-545).

### Typical concentrations and the threshold

The shaded band runs from 250 to 600 ng/mL. Psychometric performance deteriorated above about 250 ng/mL (Saletu and colleagues, *Acta Psychiatr Scand Suppl* 1986;332:67-94). van Steveninck chose about 600 ng/mL as a concentration that sedated awake volunteers clearly without putting them to sleep. The typical value, 400 ng/mL, is about the peak after 20 mg. The default **time-until-threshold** level is **250 ng/mL**, read against plasma: after 20 mg the reference man falls below it 2.3 hours after the dose, and after 30 mg 7.1 hours after.

### Where to be careful

This is a model fitted to published means, not a published model, and the oral studies it was checked against disagree with each other by up to twofold. Temazepam adds to the sedation and respiratory depression of opioids and other sedatives. Its binding to plasma proteins varies with free fatty acids, so the free fraction, and possibly the effect at a given total concentration, changes over hours.
