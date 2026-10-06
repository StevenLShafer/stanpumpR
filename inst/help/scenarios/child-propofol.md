## What to look for

A five-year-old, 20 kg, given propofol 3 mg/kg and 200 mcg/kg/min for an hour: roughly half as much again per kilogram as the adult regimen in [the maintenance scenario](scenario:propofol-induction-maintenance). The effect-site concentration lands in the same band.

The reason is allometry. The Eleveld model scales volumes roughly with weight but clearance with weight to the 0.75 power, so a 20 kg child has 29 per cent of a 70 kg adult's volume but 39 per cent of the clearance. Per kilogram, the child clears propofol faster, and a per-kilogram infusion that holds an adult's concentration lets a child's fall. The sigmoid scaling of V1 (half-maximal at 34 kg) adds to the effect: the child's central volume is relatively larger, so a per-kilogram bolus produces a lower peak.

## Try next

- Change the infusion to the adult rate of 150 mcg/kg/min and see the concentration drop below the band.
- Change the patient to 1 year, 10 kg, 75 cm. Maturation of clearance is nearly complete by one year in this model, so the picture is similar to the five-year-old's. Try 3 months (set the age unit to months), 6 kg, 60 cm: clearance is now immature, and the same per-kilogram regimen gives higher concentrations.
- Compare a drug with linear weight scaling and no maturation: give ketamine 1 mg/kg to the child and to a 70 kg adult. The curves are identical, because that model's parameters are simply proportional to weight.

## Background

[Covariates and body size](help:models/covariates) explains allometric scaling and maturation; [Propofol](help:drugs/propofol) describes the Eleveld model, which was fitted to data from premature neonates to the elderly. Pediatric predictions are still where the model is least well supported; see [Cautions](help:cautions).
