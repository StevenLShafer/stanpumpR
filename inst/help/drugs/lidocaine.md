### The model

Lidocaine's parameters are from Schnider and colleagues (*Anesthesiology* 1996;84:1043-1050), who derived and cross-validated pharmacokinetic parameters for computer-controlled infusion of lidocaine in pain therapy. It is a **two-compartment** model: a central volume of 0.101 L/kg with elimination (clearance 0.0215 L/kg/min) and one peripheral compartment (0.452 L/kg, intercompartmental clearance 0.0589 L/kg/min), and no third compartment. These are the paper's final estimates from all 13 patients (its parameter set C); until October 2026 the library used set B, the interim set fitted to the first eight patients and tested prospectively on the last five.

### Covariates

Size only. By default, with *Adjust weight to fat-free mass* ticked, the volumes scale with the patient's fat-free mass relative to the 70 kg, 170 cm reference man and the clearances with that ratio to the 0.75 power, so height, age and sex enter through fat-free mass ([fat-free-mass scaling](help:models/fat-free-mass)). Schnider's model is per kilogram of total body weight, with fixed rate constants: unticking the box restores that, scaling the volumes and clearances linearly with weight.

### Regional anesthesia (RA)

A dose in **mg RA** is a tissue injection (a nerve block or an infiltration). It enters a depot that is absorbed into the circulation by first-order kinetics, with the whole dose assumed to arrive (bioavailability 1) and no lag. The absorption rate, 0.0124/min (a half-time of 56 minutes), is the single rate that best reproduces, with this model's disposition, the mean arterial plasma concentrations after 600 mg of 1.5% lidocaine with epinephrine 5 mcg/mL for axillary block (Simon and colleagues, 2002; the largest mean, 2.94 mcg/mL, at 40 minutes; the mean of the individual peaks 2.87 mcg/mL at 26 minutes). The model peaks at 2.73 mcg/mL at about 41 minutes. Simon's own absorption half-time, 8.4 minutes, was fitted with their own disposition and does not transfer: with an independent intravenous model it predicts a peak of 6.9 mcg/mL. The rate describes axillary block with epinephrine; other sites and plain solutions absorb at different rates.

A perineural catheter infusion is entered as **mg/hr RA**, a constant rate into the same tissue depot; see [the perineural infusion scenario](help:scenarios/lidocaine-perineural-infusion). For a block dose injected into a vein by mistake, see [the intravascular-injection scenario](help:scenarios/lidocaine-intravascular-injection).

### Effect site

The time to peak effect is 5 minutes.

### Typical concentrations

The shaded band, 0.5 to 1.5 mcg/mL, is the range used for systemic analgesia (the lidocaine infusions of enhanced-recovery protocols). It is the library's own: no therapeutic range is established, and the intravenous antiarrhythmic label gives none. Central nervous system toxicity, beginning with perioral numbness and tinnitus, becomes apparent as venous plasma concentrations rise above 6 mcg/mL (the label); seizures and cardiovascular collapse follow at higher concentrations. The window between effect and toxicity is narrower than for the anesthetic drugs. See [the lidocaine infusion scenario](scenario:lidocaine-infusion).

### Where to be careful

Lidocaine's clearance depends on hepatic blood flow and falls with heart failure, hepatic disease, and the reduced cardiac output of anesthesia itself; a long infusion in such a patient accumulates well beyond what this typical model predicts. Its active metabolites (monoethylglycinexylidide, glycinexylidide) are not modelled.
