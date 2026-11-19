### The model

From Teixeira-da-Silva and colleagues (*Pharmaceutics* 2022;14:811), who fitted 1,751 routine levels from 836 patients aged 0.1 to 93 years: one compartment, total valproate, apparent clearance 0.646 L/h and volume 14 L at 70 kg, clearance scaled by weight to the power 0.75 and by (age/15)<sup>−0.0154</sup>, and volume in proportion to weight. The weight is the pharmacokinetic weight under the default [fat-free-mass scaling](help:models/fat-free-mass) and total body weight with the switch off. The half-life is about 15 hours and 1,000 mg/day averages about 64 mg/L at steady state.

### Formulations and routes

One disposition, a product-specific input: the delayed-release (enteric divalproex) tablet is the default (`mg PO DR`, absorbed at 0.78/h); the syrup or capsule (`mg PO liquid`) is faster (2.64/h) and the extended-release tablet (`mg PO ER`) slower (0.38/h) and 0.89 as available, the basis for giving 8 to 20 per cent more drug on conversion. The injection (`mg`, `mg/hr`) is interchangeable with the oral products mg for mg; all doses are in valproic acid equivalents.

### What is not modelled

The published age term, (age/15)<sup>−0.0154</sup>, is applied as printed in the paper's Equation 1, with age floored at 0.1 years, the youngest patient studied. It changes clearance by under 5% between 5 and 80 years. The paper's own worked examples do not reproduce it. Co-medication is not applied unless the **Include antiseizure drug interactions** box is ticked (below), so by default the curve is valproate **without** phenytoin, phenobarbital or carbamazepine, each of which raises clearance by 40 to 65 per cent. Valproate's binding to albumin saturates in the therapeutic range, so the unbound concentration rises faster than the total; only total valproate is plotted.

The co-medication multipliers the source gives (phenytoin, phenobarbital and carbamazepine each raise clearance about 40 to 65 per cent) are applied when the **Include antiseizure drug interactions** box is ticked. See [Antiseizure drug interactions](help:models/antiseizure-interactions).

### Band

50 to 100 mg/L total, 12 hours after a dose: the AGNP consensus guidelines' 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5, https://doi.org/10.1055/a-2860-7861; laboratory alert level 120 mg/L), the same range as the ILAE's (Patsalos and colleagues, 2008).

**The plotted curve is total valproate, and the 50 to 100 mg/L band is a band for the total.** In late pregnancy the total falls, often below the band, while the unbound, active concentration does not fall, so a low total is not by itself a reason to raise the dose: measure the free valproate. The free fraction is higher still in labour.
