### Research model — not for clinical dosing

This is a research and teaching model. The plotted activated clotting time (ACT) is **not** validated clinical decision support: it is not a heparin dose, a protamine-reversal plan, or a safe time to go on or come off bypass. Heparin on bypass is managed by a measured ACT on a specific device, and the response to a dose depends on the patient and the circuit in ways no simulation captures.

### A second heparin model

The library carries two unfractionated-heparin models. This one, **`heparinCPB`**, is the **anti-factor Xa / ACT** model; the other, `heparin`, plots **anti-IIa activity** (Jia 2015). They measure different activities of the same drug and must not be compared or combined. (This mirrors the two apixaban and two dabigatran models.)

### The model

Heparin kinetics are from Delavenne and colleagues (*Br J Anaesth* 2017;118:705-712), a two-compartment model with linear elimination fitted to the anti-factor Xa activity of 50 adults during cardiac surgery with cardiopulmonary bypass (361 anti-Xa and 341 ACT measurements, jointly). Heparin is dosed in **international units**:

- clearance 0.841 L/h, central volume 3.1 L, peripheral volume 2.23 L, intercompartmental clearance 4.67 L/h.

Only **body weight** was a covariate — on the central volume (scaling directly with weight) and on clearance (as weight^0.767), both relative to a 70 kg reference. No age, sex, renal or severity effect was retained. Weight is evaluated at the pharmacokinetic weight under the [fat-free-mass switch](help:models/fat-free-mass).

### Why the ACT, not an activity

The anti-factor Xa activity is not plotted. It drives the **activated clotting time**, which the paper fitted as a direct, instantaneous Emax function of the central anti-Xa activity:

`ACT = 116 + 720 × C / (3.49 + C)`,

with `C` the anti-Xa activity in IU/mL, `116 s` the baseline clotting time, `720 s` the maximal prolongation and `3.49 IU/mL` the activity giving half of it. There is no effect compartment, so the ACT at each moment follows the anti-Xa activity at that moment. A 300 IU/kg bolus in a 70 kg patient reaches about 6.8 IU/mL and an ACT near 590 s; the paper found a bolus followed by an infusion (55 IU/kg/h) held the ACT above 400 s with less variability than repeated boluses.

There is no MEAC and no "time until threshold": the ACT is a research endpoint here, not a clinical target, and a measured ACT differs between devices (the authors fitted a Hemochron Junior2 ACT). The shaded band runs from 480 s, the threshold the 2024 EACTS/EACTAIC/EBCP guidelines on cardiopulmonary bypass give for bypass with uncoated equipment and cardiotomy suction (Wahba and colleagues, *Interdiscip Cardiovasc Thorac Surg* 2025;40:ivaf002, section 9.3.1, https://doi.org/10.1093/icvts/ivaf002), to 600 s, an upper edge no source gives, chosen by Steven L. Shafer; it is a reference for reading the curve, not a target to dose to.

### Where to be careful

The model is small (50 patients) and specific to the bypass setting — hemodilution, high doses, a short course. The dose-to-ACT relationship is a population average; between-patient variability in both the heparin kinetics and the ACT response is large, which is exactly why heparin on bypass is dosed to a measured ACT rather than from a model. Protamine reversal, which the study managed clinically, is not represented.
