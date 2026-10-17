### Research model — not for clinical dosing

This is a research and teaching model of concentration. The curve is **not** validated clinical decision support, and a simulated value is not an ACT, a safe time to start surgery, or a protamine dose. Anticoagulation on bypass is managed with a clotting-time assay at the bedside, which this model does not predict.

### The model

The heparin kinetics are from Jia and colleagues (*J Transl Med* 2015;13:45), who fitted a two-compartment model to 41 Chinese adults having cardiac surgery with cardiopulmonary bypass. Because there is no direct assay for plasma heparin concentration, they measured **anti-factor IIa activity** (a chromogenic assay) and modelled that. The plotted curve is therefore anti-IIa activity in **IU/mL**, not a mass concentration: heparin's potency is defined by its activity, so its doses are entered in international units (IU) and have no conversion to milligrams.

At the reference adult the clearance is 1.18 L/h, the central volume 3.04 L, the intercompartmental clearance 0.171 L/h and the peripheral volume 8.01 L. The initial half-life is about 90 minutes and the terminal half-life about 37 hours — the slow return of heparin from the peripheral compartment, which is what the authors used to explain the heparin rebound seen after protamine.

### Covariates

None. Age, weight and sex were tested and none improved the fit, so the parameters are absolute. For display the model is written as the library's 70 kg reference adult and scaled to fat-free mass under the [fat-free-mass switch](help:models/fat-free-mass); heparin is also dosed per kilogram (IU/kg), which is a separate scaling of the dose.

### Where to be careful

This is the kinetics of **high-dose heparin during bypass**, with its hemodilution and hypothermia to 32 °C. Heparin's clearance is in fact saturable, and its half-life outside bypass rises with the dose (the package insert gives 0.5 to 2 hours). None of that is in this linear model; it should not be read as heparin's behaviour on the ward or in a low-dose infusion. The anti-IIa activity it plots is not anti-Xa activity and not the ACT. Protamine reversal cannot be represented.

### Provenance note

The four parameter values are transcribed from the paper's Table 2 as supplied in a specification; the table itself had not been re-read against the source when this was written (2026-10-17). They reproduce the paper's reported initial half-life, end-of-bypass activity and the size and timing of the rebound after protamine.
