### The model

Aspirin kinetics are from Koh and colleagues (*Drug Des Devel Ther* 2025;19:7853-7863). They fitted a population model to 669 plasma concentrations of acetylsalicylic acid (ASA) and salicylic acid (SA) from 44 healthy Korean adults taking 100 mg **enteric-coated** aspirin daily, and validated it against 80 and 160 mg.

- **Absorption.** After a 2.8-hour lag, 69% of the dose is absorbed at a constant rate over 1.6 hours. The rest is absorbed first-order, slowly for the tablet (0.053 per hour).
- **Pre-systemic step.** Everything absorbed passes through a pre-systemic compartment, which sends 80% on as ASA and 20% directly to SA.
- **ASA** has one compartment (V/F 23.5 L). It is converted entirely to SA, with an apparent clearance of 70 L/h at the median weight and a half-time of about 14 minutes.
- **Salicylate** has its own two-compartment kinetics, and is plotted on its own row (see [salicylate](help:drugs/salicylate)).

The parameters are apparent: absolute bioavailability was not identified.

### Routes and approximations

**mg PO** is the enteric-coated **tablet**. The capsule studied in the source, which is absorbed faster, is not offered. The engine absorbs through at most two first-order depots, so each absorption path, together with the pre-systemic step, is approximated by one depot with a lag:

- The **slow path's** depot matches the mean and spread of its delay.
- The **constant-rate path's** depot was fitted to the ASA curve of Koh's exact structure.

Against that exact structure, the ASA peak for 100 mg is 0.48 mg/L against 0.49, but at 3.8 hours against 4.4. The salicylate peak is 13% low. Both areas under the curve are exact. The ASA-to-SA conversion and the pre-systemic split are carried exactly.

Two details could not be checked against the paper's supplement:
- The median weight that the weight terms are normalised to is taken as 68.35 kg.
- The lag is assumed to apply to both absorption paths.

### Body size

The ASA-to-SA conversion rate carries the source's weight term, (W/68.35)^1.31. W is the pharmacokinetic weight with the [fat-free-mass switch](help:models/fat-free-mass) on and total weight with it off. The ASA volume takes the library's fat-free-mass scaling.

### Where to be careful

**Low dose only.** At analgesic and anti-inflammatory doses, salicylate elimination saturates. This linear model would then underpredict salicylate, and its half-life, more and more as the dose rises.

The model is for the enteric-coated tablet only; plain aspirin is absorbed within an hour. The source studied healthy Korean adults.

The antiplatelet effect, irreversible acetylation of platelet COX-1 (modelled in the source as thromboxane B2 turnover), is not plotted. There is no effect site, no shaded band and no CYP2D6 term.
