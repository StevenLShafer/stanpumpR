### The model

From Teixeira-da-Silva and colleagues (*Pharmaceutics* 2022;14:811), who fitted 1,751 routine levels from 836 patients aged 0.1 to 93 years: one compartment, total valproate, apparent clearance 0.646 L/h and volume 14 L at 70 kg, clearance scaled by weight to the power 0.75 and volume in proportion to weight. The weight is the pharmacokinetic weight under the default [fat-free-mass scaling](help:models/fat-free-mass) and total body weight with the switch off. The half-life is about 15 hours and 1,000 mg/day averages about 64 mg/L at steady state.

### Formulations and routes

One disposition, a product-specific input: the delayed-release (enteric divalproex) tablet is the default (`mg PO DR`, absorbed at 0.78/h); the syrup or capsule (`mg PO liquid`) is faster (2.64/h) and the extended-release tablet (`mg PO ER`) slower (0.38/h) and 0.89 as available, the basis for giving 8 to 20 per cent more drug on conversion. The injection (`mg`, `mg/hr`) is interchangeable with the oral products mg for mg; all doses are in valproic acid equivalents.

### What is not modelled

The published age term, (age/15)<sup>−0.0154</sup>, is omitted: it changes clearance by under 5% between 5 and 80 years, and the paper's own worked examples do not reproduce it. Co-medication is not applied, so the curve is valproate **without** phenytoin, phenobarbital or carbamazepine, each of which raises clearance by 40 to 65 per cent. Valproate's binding to albumin saturates in the therapeutic range, so the unbound concentration rises faster than the total; only total valproate is plotted.

### Pregnancy

With **Pregnant** ticked, the clearance of total valproate is multiplied by 1.6 and nothing else moves. The free fraction doubles in pregnancy, from 10 per cent early on to 20 per cent at term (Nau and Krauer, *J Clin Pharmacol* 1986;26:215-221), while the free concentration does not fall (Yerby and colleagues found it 25 per cent higher by delivery; Tomson, unchanged): a total of 1.25 / 2.0 = 0.625 of baseline at the same dose, a clearance 1.6 times higher. The factor is derived from binding studies, not a measured clearance, and is of low confidence.

**The plotted curve is total valproate, and the 50 to 100 mg/L band is a band for the total.** In late pregnancy the total falls, often below the band, while the unbound, active concentration does not fall, so a low total is not by itself a reason to raise the dose: measure the free valproate. The free fraction is higher still in labour. See [Pregnancy](help:models/pregnancy).

The co-medication multipliers the source gives (phenytoin, phenobarbital and carbamazepine each raise clearance about 40 to 65 per cent) are applied when the **Include antiseizure drug interactions** box is ticked. See [Antiseizure drug interactions](help:models/antiseizure-interactions).

### Band

50 to 100 mg/L total (ILAE; Patsalos and colleagues, 2008).
