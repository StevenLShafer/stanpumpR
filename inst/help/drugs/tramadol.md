### The model

Tramadol's disposition is from Holford and colleagues (*J Pharmacol Clin Toxicol* 2014;2:1023), who fitted parent and metabolite jointly to intravenous tramadol in 57 healthy and 56 postoperative adults. At 70 kg the parent has a central volume of 90 L, a peripheral volume of 79 L, an intercompartmental clearance of 105 L/h and a total clearance of 28.9 L/h, of which 10.5 L/h is formation of the metabolite. Clearances scale to the 0.75 power of size and volumes linearly. Tramadol is offered by mouth and intravenously; oral bioavailability is 0.70 for capsules, with the absorption rate set so the oral peak falls near one hour.

### Modelled as a prodrug

Tramadol is not inert: it has weak μ-opioid activity and, more importantly, inhibits serotonin and noradrenaline reuptake, and both mechanisms contribute to its analgesia in humans. This model deliberately represents **only the μ-opioid effect of its metabolite**, [desmetramadol](help:drugs/desmetramadol) (O-desmethyltramadol, M1), because a reuptake-inhibition effect folded into an opioid MEAC would misrepresent both. So tramadol is given no effect site of its own, the same treatment codeine gets, and the opioid effect appears on the desmetramadol row.

The difference from codeine is worth stating: codeine is a prodrug as a matter of pharmacology, whereas tramadol is a prodrug only as a matter of what this model chooses to represent. A patient's response to tramadol is **not** fully described by the desmetramadol curve, and a poor metaboliser forming almost no metabolite still gets monoaminergic analgesia that does not appear here.

### CYP2D6 phenotype

Formation of desmetramadol is CYP2D6-mediated, which is the whole reason phenotype matters for tramadol. The weights (poor 0.10, intermediate 0.58, normal 1, ultrarapid 2.25) come from Stamer and colleagues' early metabolite exposure by number of active genes (*Clin Pharmacol Ther* 2007;82:41-47); the intermediate value has independent support from Lee 2019. Because formation is a branch of total clearance, the **CYP 2D6** field changes tramadol's own clearance too, from about 19.5 L/h in a poor metaboliser to 42 in an ultrarapid one: poor metabolisers have higher tramadol and less metabolite, which is clinically familiar. See [Active metabolites](help:models/metabolites).

### Covariates

Weight, by Holford's allometry. Under the default [fat-free-mass scaling](help:models/fat-free-mass) the same exponents apply to the fat-free-mass ratio instead of total weight; tramadol and desmetramadol make this calculation identically, because the pair was fitted jointly. Age, height and sex do not enter.

### Where to be careful

Only the opioid effect is represented, so the model understates tramadol in general and especially in poor metabolisers. The metabolite's absolute concentration scale is not identified (see the desmetramadol page), although the formed concentrations are. M2 and the downstream metabolites, and the separate behaviour of the two enantiomers, are not modelled. See [the tramadol scenario](scenario:tramadol-oral).
