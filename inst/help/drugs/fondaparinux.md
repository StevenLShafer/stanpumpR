### Research model — not for clinical dosing

This is a research and teaching model of plasma concentration. It is **not** validated clinical decision support: a simulated concentration is not an anti-Xa activity, a bleeding or clotting risk, a dose recommendation, or a safe time for surgery or a neuraxial block.

### The model

Fondaparinux kinetics are from Delavenne and colleagues (*Thromb Haemost* 2010;104:252-260), a two-compartment model with first-order subcutaneous absorption fitted to patients on 2.5 mg once daily after major orthopaedic surgery (hip replacement, hip fracture repair or knee replacement). Doses are entered in **mg SC** (or **mg/kg SC**), given **subcutaneously**, and the curve is plasma concentration in mg/L. The parameters are apparent, so the absorbed fraction (fondaparinux is essentially completely absorbed) is already inside them; the absorption rate constant is 0.716 /h, the clearance 0.278 L/h, the central volume 8.14 L, the peripheral volume 15.6 L and the inter-compartmental clearance 0.082 L/h at the reference patient (68 kg, 74 years, creatinine clearance 87 mL/min). The two exponentials have half-lives of about 15 hours and 176 hours at that patient; the slow one carries only 3% of a dose's initial amplitude, so the curve falls with an effective half-life near the label's 17 to 21 hours but keeps a small tail for days, which a long plot shows.

### Covariates

Clearance rises with body weight (exponent 0.534) and creatinine clearance (exponent 0.532) and falls with age (exponent −0.461); the central volume rises with weight (exponent 0.522). Creatinine clearance is a simplified **MDRD** estimate, which the paper found more accurate than Cockcroft–Gault here, from the serum creatinine in the [Patient Profile](help:patient-profile), or an assumed normal value when blank. Weight is evaluated at the pharmacokinetic weight under the [fat-free-mass switch](help:models/fat-free-mass).

### The patient this represents

The fit is in patients **after major orthopaedic surgery** on the 2.5 mg prophylactic dose; clearance falls with age and renal impairment (about 15% lower in moderate and 43% in severe renal impairment). The treatment dose, or a non-surgical population, is outside the data. The concentration is plotted as the model's output, with no therapeutic band and no time-until-threshold.
