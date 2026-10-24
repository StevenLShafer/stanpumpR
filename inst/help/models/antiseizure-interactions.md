Many epilepsy patients take two or more antiseizure drugs, and these drugs change one another's kinetics more than any other group in the library. The **Include antiseizure drug interactions** box under [Graph Options](help:graph-options) applies those changes. It is enabled while the dose table holds a pair of drugs that interact, or carbamazepine, which induces its own metabolism. With the box ticked, each drug the interactions change is drawn twice in its panel: its curve with the interactions, and, in a lighter or darker shade of its colour, the same drug *without* them, named in the legend as, for example, *lamotrigine (no interactions)*. The gap between the two lines is the interaction. The hover readout gives both values.

The model is concentration only, like every antiseizure drug in the library: no seizure effect is modelled, and it is a teaching and research tool, not dosing guidance (see [Cautions](help:cautions)).

## What actually interacts

The interactions that matter clinically come from three mechanisms, and only one of them is inhibition of CYP3A4.

- **Enzyme induction.** Carbamazepine, phenytoin and phenobarbital (and primidone, which becomes phenobarbital) induce CYP3A4, the CYP2C enzymes, the UDP-glucuronosyltransferases (UGT) and epoxide hydrolase. Every drug those enzymes clear is cleared faster: lamotrigine, valproate, topiramate, tiagabine, lacosamide, eslicarbazepine, cenobamate, zonisamide and ethosuximide, and carbamazepine itself. The effect ranges from large (lamotrigine, roughly doubled) to modest (ethosuximide, about +22%, since it is only partly CYP3A4-metabolised). Induction needs new enzyme, so it builds over one to two weeks and fades as slowly after the inducer is stopped.
- **Inhibition.** Valproate inhibits glucuronidation (UGT, which roughly halves lamotrigine's clearance), CYP2C9 (phenytoin, phenobarbital) and epoxide hydrolase (carbamazepine-10,11-epoxide). Inhibition acts as soon as the inhibitor is present. CYP2C19 inhibitors (felbamate, oxcarbazepine and eslicarbazepine at high doses, topiramate, cenobamate, stiripentol) raise phenytoin and clobazam. Among antiseizure drugs, stiripentol is the clear CYP3A4 inhibitor; it is not in the library.
- **Displacement from albumin.** Valproate displaces phenytoin from albumin, so the *total* phenytoin concentration falls while the *unbound*, active concentration does not.

## How the model works

Nearly every published number for an antiseizure interaction is a categorical covariate in a population model: "lamotrigine clearance × 1.94 in patients taking phenytoin" (Rivas 2008). It says how big the change is in patients on ordinary maintenance doses. It does not say how the change depends on the perpetrator's concentration, or how fast it develops, and a simulator that starts and stops drugs needs both.

**Concentration.** Each interaction is a saturable function of the perpetrator's own plotted plasma concentration *C*,

```
fold(C) = 1 + (Fmax - 1) C / (EC50 + C)
```

calibrated so that at the perpetrator's reference concentration *Cref*, the typical value of its band, it equals the published factor: `Fmax = 1 + (fold - 1) (EC50 + Cref) / Cref`. Where no source gives the EC50, it is set to the lower edge of the perpetrator's reference range. Across that range the effect then varies from about 25% below to 10% above the published factor, as flat as a categorical covariate suggests; a low level gives a proportionally smaller effect; and no concentration can give an unbounded induction or a negative clearance. Valproate's inhibition of lamotrigine has a measured shape: Gidal 2003 found it half-maximal at 5.6 mcg/mL of valproate, so it is close to maximal at any therapeutic level, and the model uses that EC50.

**Time course.** Inhibition and displacement follow the perpetrator's concentration immediately. Induction goes through the turnover of the induced enzymes:

```
dE/dt = kdeg (fold(C(t)) - E),     E = 1 before the first dose
```

with the enzyme half-life of 86.1 hours (kdeg 0.00805 per hour) that Punyawudho and colleagues (2009) measured for carbamazepine de-induction with intravenous stable-labelled carbamazepine. It is a composite of all the enzymes carbamazepine induces, and it is used for every inducer because none of the others has its own estimate; Magnusson's probe-drug estimates for carbamazepine, 70 hours for CYP3A4 and 105 hours for CYP1A2, bracket it. Induction is half-developed in 3.6 days and essentially complete in two weeks, and de-induction after the inducer stops takes as long. Phenytoin and phenobarbital are slow to reach their own steady states, which slows their induction further.

**Several perpetrators.** Inhibitors multiply. Inducers of one drug drive the same enzymes, so their excess adds: `E = 1 + sum(Ei - 1)`. Rivas found two or more inducers raised lamotrigine clearance × 2.37, against × 3.10 if phenytoin and carbamazepine multiplied and × 2.54 if their excess adds; Grasela 1999 found a second inducer added only 13%. The combined induction multiplies any inhibition.

**Carbamazepine induces itself.** The library's carbamazepine model (Graves 1998) is the induced state of patients on maintenance therapy. Bertilsson 1986 found that carbamazepine's clearance doubles as it induces its own metabolism (half-life 45 hours after a single dose against 23 hours on maintenance). With the box ticked, carbamazepine therefore starts at half Graves's clearance and induces itself through the equation above, returning to Graves's clearance at a steady 8 mcg/mL. Because the induction depends on the concentration, a lower dose is induced less (Kudriakova 1992 found autoinduction proportional to dose). Another inducer's published factor on carbamazepine (phenytoin × 1.42 in Graves) was measured against the induced state, and the model reproduces it exactly at the reference concentrations.

**Phenytoin.** Phenytoin's elimination saturates, so an interaction acts on its maximal elimination rate (Vmax) or on Km. Km on the total scale is the unbound Km divided by the unbound fraction, so valproate's displacement (unbound fraction 9.6% to 15.6%, Perucca 1980) is a factor of 0.615 on Km, and zonisamide's effect (Km × 1.16, Odani 1996, the library's own phenytoin source) another.

**Solving.** Each drug is simulated on its own, the multipliers are computed from the other drugs' concentrations, and each changed drug is simulated again with a clearance that changes in steps, each step held to within 2% of the multiplier and solved exactly by the event engine (see [Events that change the kinetics](help:models/pk-events)); phenytoin's numerical engine reads its multipliers step by step. Because victims are also perpetrators (phenytoin and valproate change each other), this is repeated until no concentration moves by more than 0.2% of its peak, usually within five passes.

## The interactions applied

<!-- generated: antiseizure-interaction-table -->

The maximal factor is what the equation above reaches at a very high perpetrator concentration. The phenobarbital factors come from a population study of outpatients (Teixeira-da-Silva 2020), not from the library's phenobarbital model, which was fitted in status epilepticus, and are applied to it as relative changes.

## What is not modelled

- **Unbound phenytoin.** The library plots total phenytoin. Valproate both displaces phenytoin from albumin and inhibits its metabolism. Perucca 1980 (full text) measured both: the unbound fraction rose from 9.6% to 15.6% (displacement, applied as Km × 0.615) and the free intrinsic clearance fell about 18% (metabolic inhibition, not applied). Only the displacement is applied, so the plotted total falls, as Perucca measured; the unbound, active phenytoin does not fall and in fact rises. With valproate on board a total phenytoin level understates the active drug; a free level is the guide. Plotting free phenytoin (which rises) alongside the total is a documented, deliberately deferred refinement (see `docs/antiseizure-interactions-review.md`).
- **Metabolites.** Carbamazepine-10,11-epoxide, which valproate raises, and N-desmethylclobazam are not plotted.
- **Interactions without a number, or outside the library.** Some interactions are described only in direction, or their size is given only in a product label (cenobamate's effects on phenytoin, lamotrigine, carbamazepine and phenobarbital, now read from the XCOPRI label and recorded but not yet applied — cenobamate has no therapeutic band, so applying them needs a defined reference concentration), or a drug involved is not in the library (felbamate, oxcarbazepine, stiripentol, clobazam, perampanel, rufinamide, brivaracetam). None of those is applied.
- **Drugs outside the group.** Oral contraceptives, rifampicin and the azoles are not modelled, nor is any pharmacodynamic interaction.

## Scenarios

- [Valproate doubles lamotrigine](scenario:valproate-lamotrigine-inhibition)
- [Stopping carbamazepine raises lamotrigine](scenario:carbamazepine-lamotrigine-deinduction)
- [Carbamazepine induces its own metabolism](scenario:carbamazepine-autoinduction)
- [Phenytoin and valproate change each other](scenario:phenytoin-valproate-displacement)
- [Phenytoin lowers carbamazepine](scenario:carbamazepine-phenytoin-induction)

## Sources

The registry behind this page is `inst/extdata/antiseizureInteractions.csv` (built by `data-raw/antiseizureInteractions.R`), one row per interaction and source, with the evidence read for each.

<!-- generated: antiseizure-interaction-sources -->
