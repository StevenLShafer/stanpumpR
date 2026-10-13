### The model

Phenytoin is the one drug in the library with **saturable (Michaelis-Menten) elimination**: its clearance falls as the concentration rises, so a modest dose increase can raise the steady-state level out of proportion. It is simulated by a numerical engine (see the model notes), not by the closed-form solver the other drugs use.

The parameters are from Odani and colleagues (*Biol Pharm Bull* 1996;19:444-448), who fitted 531 steady-state levels from 116 Japanese patients with epilepsy, children and adults: one compartment, total phenytoin, with a volume of 1.23 L/kg, a maximal elimination rate of 9.80 mg/day/kg and a Michaelis constant of 9.19 mg/L, each scaled by weight to the power 0.463. Odani assumed oral bioavailability was complete. At 300 mg/day a typical 70 kg adult reaches about 12 mg/L; at 400 mg/day, about 30.

### Dose basis and routes

Concentrations are phenytoin acid. The dose carries its own basis, converted once: phenytoin **sodium** (the extended-release capsule and the injection) is 0.92 phenytoin acid by weight; the acid suspension and chewable tablet are entered as is. **Fosphenytoin** is a route of phenytoin, not a separate drug, so the saturable elimination sees every source at once: it is prescribed in phenytoin sodium equivalents (`mg PE`, `mg PE IM`, `mg PE/min`), converted to a phenytoin compartment at a 15-minute conversion half-life (intramuscularly it is absorbed first, completely, at 2.47/h).

The extended-release capsule is absorbed at 0.225/h (fixed, Cheng 2020), peaking at about 11 hours; the suspension and chewable tablet have no published absorption rate and are given a label-anchored 2.0/h (peak about 2.4 hours). These are recorded as calibrations, not estimates, in the registry.

### CYP2C9

The maximal elimination rate is 33 per cent lower in CYP2C9 intermediate metabolisers (activity score 1, the *1/*3 genotype; Odani 1997) and, following CPIC's maintenance-dose guidance, 50 per cent lower in poor metabolisers. Set the **CYP 2C9** field in the [Patient Profile](help:patient-profile); a score of 1.5 is entered as normal. CYP2C19 and CYP2D6 carry no established phenytoin effect.

### What is not modelled

Albumin binding and the unbound fraction (the curve is total phenytoin, so in hypoalbuminaemia or renal failure a total of 10 mg/L may carry a therapeutic unbound level), valproate displacement, enzyme induction or inhibition by co-medication, and the early albumin displacement by fosphenytoin. There is no effect site and no time until threshold.

### Band

10 to 20 mg/L total (ILAE; Patsalos and colleagues, *Epilepsia* 2008;49:1239-1276), the same range as 1 to 2 mg/L unbound at normal binding.
