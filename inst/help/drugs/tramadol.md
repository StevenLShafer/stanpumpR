### The model

Tramadol's disposition is from Allegaert and colleagues (*Clin Pharmacokinet* 2015;54:167-178), a pooled population analysis of intravenous tramadol and its metabolite in 295 people from 25 weeks postmenstrual age to 85 years. At 70 kg the parent has a central volume of 51 L, a peripheral volume of 134 L, an intercompartmental clearance of 227 L/h and a total clearance of 32.1 L/h, of which 10.5 L/h is formation of the metabolite. Clearances scale to the 0.75 power of size and volumes linearly, and each clearance matures with age (below). Tramadol is offered by mouth and intravenously; oral bioavailability is 0.70 for capsules, with the absorption rate set so the oral peak falls near one hour.

Until October 2026 the model was Holford and colleagues' earlier fit to adults (*J Pharmacol Clin Toxicol* 2014;2:1023), which Allegaert's group took as its starting point and re-estimated with the children added. In an adult the two agree closely after the first quarter hour of an intravenous dose (the same 4.3-hour terminal half-life, metabolite exposure within 6 per cent). The smaller central volume gives a higher concentration in the first few minutes, and Allegaert's maturation describes the first year of life, which Holford's did not.

### Dose basis

Doses are entered as **tramadol hydrochloride**, as the drug is labelled and prescribed, and the plotted concentrations are tramadol base. Allegaert and colleagues converted every hydrochloride dose to base before fitting, multiplying by 0.8784 (263.38/299.84), as Holford's group had, so all the parameters are on the base scale. The library applies the same factor, so 100 mg of tramadol hydrochloride intravenously gives an initial concentration of 1722 ng/mL at 70 kg (87.84 mg of base in 51 L), not 1961. Until October 2026 the hydrochloride dose was fed to the base-scale model unconverted, which put every tramadol and desmetramadol concentration 14 per cent high.

### Modelled as a prodrug

Tramadol is not inert: it has weak μ-opioid activity and, more importantly, inhibits serotonin and noradrenaline reuptake, and both mechanisms contribute to its analgesia in humans. This model deliberately represents **only the μ-opioid effect of its metabolite**, [desmetramadol](help:drugs/desmetramadol) (O-desmethyltramadol, M1), because a reuptake-inhibition effect folded into an opioid MEAC would misrepresent both. So tramadol is given no effect site of its own, the same treatment codeine gets, and the opioid effect appears on the desmetramadol row.

The difference from codeine is worth stating: codeine is a prodrug as a matter of pharmacology, whereas tramadol is a prodrug only as a matter of what this model chooses to represent. A patient's response to tramadol is **not** fully described by the desmetramadol curve, and a poor metaboliser forming almost no metabolite still gets monoaminergic analgesia that does not appear here.

### CYP2D6 phenotype

Formation of desmetramadol is CYP2D6-mediated, which is the whole reason phenotype matters for tramadol. The weights (poor 0.10, intermediate 0.58, normal 1, ultrarapid 2.25) come from Stamer and colleagues' early metabolite exposure by number of active genes (*Clin Pharmacol Ther* 2007;82:41-47); the intermediate value has independent support from Lee 2019. Because formation is a branch of total clearance, the **CYP 2D6** field changes tramadol's own clearance too, from about 22.7 L/h in a poor metaboliser to 45 in an ultrarapid one (adult, 70 kg): poor metabolisers have higher tramadol and less metabolite, which is clinically familiar. See [Active metabolites](help:models/metabolites).

### Covariates

**Weight**, by Allegaert's allometry. Under the default [fat-free-mass scaling](help:models/fat-free-mass) the same exponents apply to the fat-free-mass ratio instead of total weight; tramadol and desmetramadol make this calculation identically, because the pair was fitted jointly.

**Age**, through maturation. Each clearance pathway is multiplied by PMA<sup>Hill</sup> / (TM50<sup>Hill</sup> + PMA<sup>Hill</sup>), with PMA the postmenstrual age in weeks, taken as 40 + 52 × age in years (a term birth; a premature infant's gestational age cannot be entered). From Allegaert's Table 8, tramadol's clearance by other pathways matures with TM50 39.0 weeks and Hill 6.76, and its formation of the metabolite (CYP2D6) with TM50 39.9 weeks and Hill 9.00, so both are about half the adult value at term and essentially mature by six months. The metabolite's own elimination follows glomerular filtration (TM50 47.7 weeks, Hill 3.4): 0.36 of the adult value at term, 0.90 at one year. The first-pass formation of metabolite matures with the formation clearance. The intercompartmental clearances and the volumes do not mature. Height and sex enter only through fat-free mass when that scaling is on.

One finding of Allegaert's is not carried: the children of one study had a clearance by other pathways a third of everyone else's and a metabolite elimination about half, which the authors could not explain. The model uses the typical values of all the other studies.

### Where to be careful

Only the opioid effect is represented, so the model understates tramadol in general and especially in poor metabolisers. The maturation assumes a term birth, and the oral absorption and first pass were set for adults; no oral data in infants informed them. The metabolite's absolute concentration scale is not identified (see the desmetramadol page), although the formed concentrations are. M2 and the downstream metabolites, and the separate behaviour of the two enantiomers, are not modelled. See [the tramadol scenario](scenario:tramadol-oral).
