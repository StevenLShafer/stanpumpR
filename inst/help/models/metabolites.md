Several opioids, and amiodarone, act partly or wholly through a metabolite the body forms from them. stanpumpR models this with a **metabolite link**: a drug's model may name another drug as its active metabolite, and then a dose of the parent produces a second curve for the metabolite, on the metabolite's own row.

## Which drugs form which

| Parent | Active metabolite | Notes |
|---|---|---|
| [codeine](help:drugs/codeine) | [morphine](help:drugs/morphine) | A prodrug: codeine has no effect site of its own, the analgesia is the morphine's. CYP2D6-dependent. |
| [tramadol](help:drugs/tramadol) | [desmetramadol](help:drugs/desmetramadol) | Modelled as a prodrug for its opioid effect; tramadol's monoaminergic analgesia is not represented. CYP2D6-dependent. |
| [hydrocodone](help:drugs/hydrocodone) | [hydromorphone](help:drugs/hydromorphone) | Oral only; CYP2D6-dependent. |
| [oxycodone](help:drugs/oxycodone) | [oxymorphone](help:drugs/oxymorphone) | Oxymorphone is also a drug in its own right. |
| [amiodarone](help:drugs/amiodarone) | [desethylamiodarone](help:drugs/desethylamiodarone) | Not a prodrug: both are active and appear equally potent, and neither has an effect site. All of amiodarone's clearance forms the metabolite, mass for mass, as Pollak and colleagues modelled it. Oral only, as a constant daily rate. Not CYP2D6-dependent. |

## How it is computed

The parent's plasma curve is convolved through the metabolite's own disposition, which leaves a sum of exponentials over the two drugs' combined eigenvalues, so the metabolite advances through the same closed-form machinery as everything else, with no numerical solver. An oral dose of the parent adds a second branch for metabolite formed during first pass, before the parent reaches the circulation.

Forming the metabolite does **not** change the parent's own plasma or effect-site curve: formation is modelled as an independent transfer, and the parent's fitted clearance already includes it. What it changes is the metabolite's row. A metabolite that was never given directly still gets a row created for it, so giving codeine alone shows the morphine it produces.

Because a contribution crosses from one drug's row to another's, the metabolite contributions are added in after every drug has been simulated. Each drug keeps its own simulation and the folded total separately, which is what lets the total be rebuilt safely whenever an input changes.

## A prodrug has no effect site

Codeine and tramadol are given a time to peak effect of zero, so they have no effect site: only the plasma concentration is plotted, their MEAC is zero, and the effect appears entirely on the metabolite's row. Their shaded bands are plasma-concentration ranges, not therapeutic effect-site ranges. See [The effect site and ke0](help:models/effect-site).

Amiodarone has no effect site either, but it is **not** a prodrug: it is active itself, and simply has no published equilibration model for its antiarrhythmic effect. Its row and its metabolite's both plot serum concentrations, and its shaded band is the therapeutic window for serum amiodarone.

## Time until threshold across the link

Concentrations add, but recovery times do not, so the merged row's [time until threshold](help:models/recovery) is not built from the two rows' recovery columns: it is solved again from the combined effect-site state underneath them. This is exact, because the whole intravenous path is linear. Without it, a patient given only the parent would see no recovery time at all for the opioid they actually had.

## CYP2D6 phenotype

All four opioid links are CYP2D6-dependent, so the **CYP 2D6** field in the Patient Profile scales formation; amiodarone's link is not, and the field does not change it. The phenotypes are the terms the genotyping laboratories report: poor, intermediate, normal and ultrarapid, with normal as the reference the formation parameters are published against. Each parent's page shows the effect of phenotype on its formation rate, and on its own clearance where formation is a branch of it. This is the work that activated the CYP 2D6 field, which was present but disabled before.

## What is not modelled

Second-generation metabolites that would need a two-stage cascade (morphine-6-glucuronide from codeine, for example) are not formed. The enantiomers of desmetramadol, formed and eliminated stereoselectively, are carried as a single racemic species. Several times to peak effect and minimum effective concentrations in this work are provisional, as each drug's page and the code say.
