## What to look for

A five-year-old, 20 kg, given propofol 3 mg/kg and 200 mcg/kg/min for an hour: roughly half as much again per kilogram as the adult regimen in [the maintenance scenario](scenario:propofol-induction-maintenance). The effect-site concentration lands in the same band.

The reason is allometry. The Eleveld model scales clearance with weight to the 0.75 power, so this 20 kg child, with 29 per cent of a 70 kg adult's weight, has about 43 per cent of the adult's clearance. Per kilogram, the child clears propofol half as fast again, and a per-kilogram infusion that holds an adult's concentration lets a child's fall. The sigmoid scaling of V1 (half-maximal at 34 kg) adds to the effect: the child's central volume is relatively larger, nearly twice the adult's per kilogram, so a per-kilogram bolus produces a lower peak.

## Try next

- Change the infusion to the adult rate of 150 mcg/kg/min and see the concentration drop, to about 3 mcg/mL, towards the bottom of the band.
- Change the patient to 1 year, 10 kg, 75 cm. Maturation of clearance is nearly complete by one year in this model, so the picture is similar to the five-year-old's. Try 3 months (set the age unit to months), 6 kg, 60 cm: clearance is now immature, but per kilogram it is still above the five-year-old's, and the infant's volumes are larger per kilogram too, so on this model the same per-kilogram regimen gives concentrations a little lower, not higher.
- Compare a drug with linear weight scaling and no maturation: give ketamine 1 mg/kg to the child and to a 70 kg adult, with *Adjust weight to fat-free mass* unticked. The curves are identical, because that model's parameters are then simply proportional to weight. With the box ticked, as it is by default, the model is scaled to fat-free mass instead, and the child's concentrations run about a quarter lower over the two hours (the effect-site peak about a third lower); see [Scaling to fat-free mass](help:models/fat-free-mass).

## Background

[Covariates and body size](help:models/covariates) explains allometric scaling and maturation; [Propofol](help:drugs/propofol) describes the Eleveld model, which was fitted to data from premature neonates to the elderly. Pediatric predictions are still where the model is least well supported; see [Cautions](help:cautions).
