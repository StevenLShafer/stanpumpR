The mammillary three-compartment model is the workhorse of anesthetic pharmacokinetics. Drug is given into a central compartment (V1: the blood and the organs in rapid equilibrium with it) and distributes reversibly into a rapidly equilibrating peripheral compartment (V2: muscle and viscera) and a slowly equilibrating one (V3: fat and other poorly perfused tissue). Elimination is from the central compartment.

## Parameters

| Symbol | Meaning | Units |
|---|---|---|
| V1, V2, V3 | Volumes of the central, fast and slow compartments | L |
| CL1 | Elimination (metabolic) clearance | L/min |
| CL2, CL3 | Intercompartmental clearances to the fast and slow compartments | L/min |
| k10 = CL1/V1 | Elimination rate constant | 1/min |
| k12 = CL2/V1, k21 = CL2/V2 | Transfer to and from the fast compartment | 1/min |
| k13 = CL3/V1, k31 = CL3/V3 | Transfer to and from the slow compartment | 1/min |

Each drug's page tabulates all of these at six reference patients. Published models are variously reported as volumes and clearances or as V1 and rate constants; the drug files hold whichever the paper gave and convert. A model with no third compartment (lidocaine, rocuronium, oliceridine, oxycodone) is handled by setting CL3 to zero.

## The differential equations

With A1, A2, A3 the amounts in the three compartments and I(t) the infusion rate,

```
dA1/dt = I(t) - (k10 + k12 + k13) A1 + k21 A2 + k31 A3
dA2/dt = k12 A1 - k21 A2
dA3/dt = k13 A1 - k31 A3
Cp     = A1 / V1
```

## The closed-form solution

Because the system is linear, the plasma concentration after a unit bolus is a sum of three exponentials,

```
Cp(t) = A e^(-α t) + B e^(-β t) + C e^(-γ t)
```

where α > β > γ are the roots of the characteristic cubic of the rate-constant matrix (stanpumpR solves it in `cube()`), and the coefficients A, B, C follow from the rate constants. The three terms are the rapid distribution, slow distribution and elimination phases of the familiar tri-exponential curve. The pages for each drug report the half-lives ln(2)/α, ln(2)/β and ln(2)/γ.

A constant infusion is the integral of the bolus response, so it is also a sum of the same exponentials with different coefficients. A dose table is therefore simulated by adding up, at each time point, the contribution of every bolus and infusion segment that has started. Nothing is integrated numerically, and the answer is exact at every evaluated time.

## What the phases mean clinically

- **α** (minutes) is the fall after a bolus as drug leaves the blood for tissue. It is why a bolus produces a high, brief plasma peak.
- **β** (tens of minutes) is distribution into slower tissue.
- **γ** (hours to a day) is elimination once distribution is complete. It is the terminal half-life, and it is a poor guide to how fast a drug wears off after a short infusion, because most of the drug is still redistributing.

Use *Time until threshold* rather than any half-life to see how fast a given drug wears off from a given regimen; see [Time until threshold](help:models/recovery) and the [context-sensitive decrement scenario](scenario:context-sensitive-opioids).

## Limits of the model

- It is linear: doubling the dose doubles every concentration. Saturable metabolism, protein-binding changes and enzyme induction are not represented.
- Its parameters are constants within a simulation except where [events](help:models/pk-events) step them.
- It describes the typical patient of its source population. See [Cautions](help:cautions).

## In the code

`R/getDrugPK.R` computes the rate constants, eigenvalues and coefficients; `R/cube.R` solves the cubic; `R/advanceClosedForm0.R` advances a dose table without events and `R/advanceClosedForm1.R` with them; `R/simCpCe.R` dispatches. See [What is in the repository](help:repository).
