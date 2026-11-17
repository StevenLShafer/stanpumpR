### The model

Sotalol's disposition comes from the PEAKS registry (Yellepeddi and colleagues, *CPT Pharmacometrics Syst Pharmacol* 2025;14:658-667): 22 adults with atrial fibrillation or flutter (age 48 to 79, weight 69 to 185 kg) given intravenous sotalol by the package insert, 60 to 120 mg over an hour, then two oral doses. Two compartments: clearance 9.5 L/h at a creatinine clearance of 92.4 mL/min, central volume 38.2 L at 104 kg, intercompartmental clearance 71.3 L/h and peripheral volume 89.1 L.

### Covariates

- **Renal function**: clearance is 9.5 × (creatinine clearance / 92.4)^0.7 L/h, the creatinine clearance by Cockcroft-Gault from the Patient Profile's **Serum creatinine** (blank means an assumed normal creatinine for age and sex). Sotalol is not metabolised and leaves unchanged in the urine, which is why its doses are set by creatinine clearance. PEAKS required a creatinine clearance above 60 mL/min, so below that the curve is an extrapolation, in the direction the label describes.
- **Weight** on the central volume, to the power 1.1. With [the scaling switch](help:models/fat-free-mass) on, both use the fat-free-mass weight.

### Oral

The registry's own oral estimates could not be used (a bioavailability above 1, from samples drawn only 2 to 4 hours after each dose). Bioavailability is the Betapace label's ("90 to 100%"), and the absorption rate constant (0.605 /h) and lag (14 minutes) are the FDA reviewer's population model of the oral and intravenous crossover behind the injection's approval. The label says peak concentrations follow an oral dose by 2.5 to 4 hours.

### Dose

Every US product is **sotalol hydrochloride**, and the doses here are milligrams of the hydrochloride as labelled, as PEAKS dosed them.

### No band

No therapeutic plasma range is established: sotalol is started and adjusted by the QT interval, which lengthens in proportion to the concentration ("QTc prolongation is directly related to the concentration of sotalol", injection label). PEAKS measured 220 to 1900 ng/mL after intravenous loading.

### Pregnancy

With the Pregnant box ticked and the creatinine left blank, the creatinine clearance is raised by half, the rise in glomerular filtration of late pregnancy, which raises sotalol clearance by a third. O'Hare and colleagues (*Eur J Clin Pharmacol* 1983) measured clearance 2.4 against 1.5 mL/min/kg at 32-36 weeks against postpartum, the same direction and larger.
