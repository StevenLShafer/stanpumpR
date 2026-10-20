### The model

Phenytoin is the one drug in the library with **saturable (Michaelis-Menten) elimination**: its clearance falls as the concentration rises, so a modest dose increase can raise the steady-state level out of proportion. It is simulated by a numerical engine (see the model notes), not by the closed-form solver the other drugs use.

The parameters are from Odani and colleagues (*Biol Pharm Bull* 1996;19:444-448), who fitted 531 steady-state levels from 116 Japanese patients with epilepsy, children and adults: one compartment, total phenytoin, with a volume of 1.23 L/kg and a maximal elimination rate of 9.80 mg/day/kg, both per kilogram at Odani's typical 42 kg and scaled by weight to the power 0.463, and a Michaelis constant of 9.19 mg/L that does not change with weight. Odani assumed oral bioavailability was complete. At 300 mg/day a typical 70 kg adult reaches about 12 mg/L; at 400 mg/day, about 30.

### Dose basis and routes

Concentrations are phenytoin acid. The dose carries its own basis, converted once: phenytoin **sodium** (the extended-release capsule and the injection) is 0.92 phenytoin acid by weight; the acid suspension and chewable tablet are entered as is. **Fosphenytoin** is a route of phenytoin, not a separate drug, so the saturable elimination sees every source at once: it is prescribed in phenytoin sodium equivalents (`mg PE`, `mg/kg PE`, `mg PE/min`, `mg PE IM`, `mg/kg PE IM`), converted to a phenytoin compartment at a 15-minute conversion half-life (intramuscularly it is absorbed first, completely, at 2.47/h).

The extended-release capsule is absorbed at 0.225/h (fixed, Jun 2020), peaking at about 11 hours; the suspension and chewable tablet have no published absorption rate and are given a label-anchored 2.0/h (peak about 2.4 hours). These are recorded as calibrations, not estimates, in the registry.

### CYP2C9

The maximal elimination rate is 33 per cent lower in CYP2C9 intermediate metabolisers (activity score 1, the *1/*3 genotype; Odani 1997) and, following CPIC's maintenance-dose guidance, 50 per cent lower in poor metabolisers. Set the **CYP 2C9** diplotype in the [Patient Profile](help:patient-profile): \*1/\*3 and \*2/\*2 are intermediate, \*2/\*3 and \*3/\*3 poor, and \*1/\*2 (activity score 1.5) is treated as normal, as CPIC advises. CYP2C19 and CYP2D6 carry no established phenytoin effect.

### What is not modelled

Albumin binding and the unbound fraction (the curve is total phenytoin, so in hypoalbuminaemia or renal failure a total of 10 mg/L may carry a therapeutic unbound level), valproate displacement, enzyme induction or inhibition by co-medication, and the early albumin displacement by fosphenytoin. There is no effect site and no time until threshold.

### Pregnancy

With **Pregnant** ticked, the Michaelis constant on the total scale is multiplied by 0.48 and the maximal elimination rate by 1.1, on every route, fosphenytoin's included. In the third trimester **total** phenytoin falls to 39 per cent of its value after delivery, at an unchanged dose, while **unbound** phenytoin falls only to 82 per cent (Tomson and colleagues, *Epilepsia* 1994;35:122-130): most of the fall is a fall in binding, the unbound fraction rising about 2.1-fold. Elimination saturates in the unbound concentration, so the constant on the total scale falls by the same factor (1 / 2.1 = 0.48), and the small rise in the maximal rate makes the unbound steady state fall to about 80 per cent at a total of 15 mg/L. At the dose that holds 15 mg/L outside pregnancy, the plotted total falls to about 5.7 mg/L.

**The plotted curve is total phenytoin, and the 10 to 20 mg/L band is a band for the total.** In late pregnancy the total falls well below the band while the unbound, active concentration stays near its baseline, so a low total is not by itself a reason to raise the dose: measure the free phenytoin. The factors are tied to that operating point, since elimination saturates; the trimesters before the third are not represented. See [Pregnancy](help:models/pregnancy).

### Band

10 to 20 mg/L total (ILAE; Patsalos and colleagues, *Epilepsia* 2008;49:1239-1276), the same range as 1 to 2 mg/L unbound at normal binding.
