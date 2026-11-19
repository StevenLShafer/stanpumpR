### The model

Phenytoin is one of the few drugs in the library with **saturable (Michaelis-Menten) elimination** (the others are GHB, exenatide and cetagliptin): its clearance falls as the concentration rises, so a modest dose increase can raise the steady-state level out of proportion. It is simulated by a numerical engine (see the model notes), not by the closed-form solver most drugs use.

The parameters are from Odani and colleagues (*Biol Pharm Bull* 1996;19:444-448), who fitted 531 steady-state levels from 116 Japanese patients with epilepsy, children and adults: one compartment, total phenytoin, with a volume of 1.23 L/kg and a maximal elimination rate of 9.80 mg/day/kg, both per kilogram at Odani's typical 42 kg and scaled by weight to the power 0.463, and a Michaelis constant of 9.19 mg/L that does not change with weight. Odani assumed oral bioavailability was complete. At 300 mg/day of phenytoin acid (the suspension or chewable tablet) a typical 70 kg adult averages about 12 mg/L at steady state; at 400 mg/day, about 30. The same doses as the sodium capsule, which carries 0.92 as much phenytoin, average about 10 and 22 mg/L.

### Dose basis and routes

Concentrations are phenytoin acid. The dose carries its own basis, converted once: phenytoin **sodium** (the extended-release capsule and the injection) is 0.92 phenytoin acid by weight; the acid suspension and chewable tablet are entered as is. **Fosphenytoin** is a route of phenytoin, not a separate drug, so the saturable elimination sees every source at once: it is prescribed in phenytoin sodium equivalents (`mg PE`, `mg/kg PE`, `mg PE/min`, `mg PE IM`, `mg/kg PE IM`), converted to a phenytoin compartment at a 15-minute conversion half-life (intramuscularly it is absorbed first, completely, at 2.47/h).

The extended-release capsule is absorbed at 0.225/h (fixed, Jun 2020), peaking at about 11 hours; the suspension and chewable tablet have no published absorption rate and are given a label-anchored 2.0/h (peak about 2.4 hours). These are recorded as calibrations, not estimates, in the registry.

### CYP2C9

The maximal elimination rate is 33 per cent lower in CYP2C9 intermediate metabolisers (activity score 1, the *1/*3 genotype; Odani 1997) and, following CPIC's maintenance-dose guidance, 50 per cent lower in poor metabolisers. Set the **CYP 2C9** diplotype in the [Patient Profile](help:patient-profile): \*1/\*3 and \*2/\*2 are intermediate, \*2/\*3 and \*3/\*3 poor, and \*1/\*2 (activity score 1.5) is treated as normal, as CPIC advises. CYP2C19 and CYP2D6 carry no established phenytoin effect.

### What is not modelled

Albumin binding and the unbound fraction (the curve is total phenytoin, so in hypoalbuminaemia or renal failure a total of 10 mg/L may carry a therapeutic unbound level), induction or inhibition by co-medication other than the two interactions below, and the early albumin displacement by fosphenytoin. There is no effect site and no time until threshold.

Valproate (which displaces phenytoin from albumin and lowers the plotted total) and zonisamide (which raises Km) change phenytoin when the **Include antiseizure drug interactions** box is ticked. See [Antiseizure drug interactions](help:models/antiseizure-interactions).

### Band

10 to 20 mg/L total, 8 to 12 hours after a dose: the AGNP consensus guidelines' 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5, https://doi.org/10.1055/a-2860-7861; laboratory alert level 50 mg/L), the same range as the ILAE's (Patsalos and colleagues, *Epilepsia* 2008;49:1239-1276) and as 1 to 2 mg/L unbound at normal binding.

**The plotted curve is total phenytoin, and the 10 to 20 mg/L band is a band for the total.** In late pregnancy the total falls well below the band while the unbound, active concentration falls far less (Tomson and colleagues measured 39% and 82% of baseline in the third trimester at an unchanged dose, and the model is calibrated to those two falls), so a low total is not by itself a reason to raise the dose: measure the free phenytoin. The pregnancy factors were set at a typical total of 15 mg/L and are tied to that operating point, since elimination saturates; the trimesters before the third are not represented.
