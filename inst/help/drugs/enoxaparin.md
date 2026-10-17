### Research model — not for clinical dosing

This is a research and teaching model of anti-Xa activity. It is **not** validated clinical decision support: a simulated anti-Xa activity is not a bleeding or clotting risk, a dose recommendation, or a safe time for surgery or a neuraxial block.

### The model

Enoxaparin kinetics are from Berges and colleagues (*Br J Clin Pharmacol* 2007;64:428-438, the PROPHRE.75 study), a two-compartment model with first-order subcutaneous absorption fitted to anti-Xa activity in 189 patients over 75 years old, on 4000 IU once daily for VTE prophylaxis. Doses are entered in anti-Xa **international units (IU)**, given **subcutaneously**, and the curve is anti-Xa activity in IU/mL. The parameters are apparent, so the absorbed fraction is already inside them; the absorption rate constant is 0.63 /h, the clearance 0.70 L/h, the central volume 6.43 L, the peripheral volume 8.18 L and the inter-compartmental clearance 0.34 L/h at the reference patient (65 kg, creatinine clearance 69 mL/min).

### Covariates

Clearance rises with body weight (exponent 0.78) and with creatinine clearance (exponent 0.25), and the central volume rises with weight (exponent 1.25) — the paper's own fit, close to standard allometry. Creatinine clearance is a simplified **MDRD** estimate (the equation the paper used) from the serum creatinine in the [Patient Profile](help:patient-profile), or an assumed normal value when blank. Weight is evaluated at the pharmacokinetic weight under the [fat-free-mass switch](help:models/fat-free-mass).

### The patient this represents

The fit is in patients **over 75**, mostly women, at a low fixed prophylactic dose. The paper found the renal and weight effects statistically significant but of slight magnitude (the terminal half-life differs by under 1.5 hours across the range it studied). A younger or heavier patient, or the curative dose, is outside the data. Anti-Xa activity is plotted as the model's output, not a clinical threshold: the study's "empirical" 1.0 IU/mL bleeding cut-off was defined without clinical evaluation and is not drawn.
