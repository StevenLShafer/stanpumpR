### The model

Lidocaine's parameters are from Schnider and colleagues (*Anesthesiology* 1996;84:1043-1050), who derived and cross-validated pharmacokinetic parameters for computer-controlled infusion of lidocaine in pain therapy. It is a **two-compartment** model: a central volume of 0.088 L/kg with elimination and one peripheral compartment, and no third compartment.

### Covariates

Size only. By default, with *Adjust weight to fat-free mass* ticked, the volumes scale with the patient's fat-free mass relative to the 70 kg, 170 cm reference man and the clearances with that ratio to the 0.75 power, so height, age and sex enter through fat-free mass ([fat-free-mass scaling](help:models/fat-free-mass)). Schnider's model is per kilogram of total body weight, with fixed rate constants: unticking the box restores that, scaling the volumes and clearances linearly with weight.

### Regional anesthesia (RA)

A dose in **mg RA** is a tissue injection (a nerve block or an infiltration). It enters a depot that is absorbed into the circulation by first-order kinetics, with the whole dose assumed to arrive (bioavailability 1) and no lag. The absorption rate, 0.0111/min (a half-time of 62 minutes), is the single rate that best reproduces, with this model's disposition, the arterial plasma curve after 600 mg of 1.5% lidocaine with epinephrine 5 mcg/mL for axillary block (Simon and colleagues, 2002; mean peak 2.87 mcg/mL at 26 minutes). The model peaks at 2.76 mcg/mL at about 42 minutes. Simon's own absorption half-time, 8.4 minutes, was fitted with their own disposition and does not transfer: with an independent intravenous model it predicts a peak of 6.9 mcg/mL. The rate describes axillary block with epinephrine; other sites and plain solutions absorb at different rates.

A perineural catheter infusion is entered as **mg/hr RA**, a constant rate into the same tissue depot; see [the lidocaine infusion scenario](help:scenarios/lidocaine-perineural-infusion).

### Effect site

The time to peak effect is 5 minutes.

### Typical concentrations

The shaded band, 0.5 to 1.5 mcg/mL, is the range used for systemic analgesia (the lidocaine infusions of enhanced-recovery protocols) and for the treatment of ventricular arrhythmias. Central nervous system toxicity, beginning with perioral numbness and tinnitus, appears above about 5 mcg/mL in most patients; seizures and cardiovascular collapse follow at higher concentrations. The window between effect and toxicity is narrower than for the anesthetic drugs. See [the lidocaine infusion scenario](scenario:lidocaine-infusion).

### Where to be careful

Lidocaine's clearance depends on hepatic blood flow and falls with heart failure, hepatic disease, and the reduced cardiac output of anesthesia itself; a long infusion in such a patient accumulates well beyond what this typical model predicts. Its active metabolites (monoethylglycinexylidide, glycinexylidide) are not modelled.
