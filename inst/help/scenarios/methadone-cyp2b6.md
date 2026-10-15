## What to look for

Ten milligrams of oral methadone every twelve hours for a fortnight, in a 70 kg 40-year-old man who is a **CYP2B6 poor metaboliser** (the Patient Profile field is set for you). Methadone is cleared mainly by CYP2B6-catalysed N-demethylation to EDDP, and this patient's two reduced-function alleles cut his non-renal clearance of S(+)-methadone by 38 per cent and of R(-)-methadone by 23 per cent.

The curve climbs for the best part of two weeks. By day 14 it runs between 109 and 130 ng/mL over a dosing interval, with the upper end above the top of the shaded band. The MEAC panel, below the concentration plot, scores the same curve against the 60 ng/mL analgesic concentration.

Now change **CYP 2B6** in the Patient Profile to *Normal* and apply. The same regimen settles between 78 and 99 ng/mL, inside the band.

The size of the difference depends on when you look:

| | Day 1 | Day 3 | Day 7 | Day 14 |
|---|---|---|---|---|
| Normal metaboliser | 28 | 57 | 75 | 78 ng/mL |
| Poor metaboliser | 31 | 69 | 99 | 109 ng/mL |
| Ratio | 1.12 | 1.21 | 1.32 | 1.39 |

This is the lesson. One dose tells you almost nothing: after a day the two patients are 12 per cent apart, well inside the noise of any single measurement. Clearance is what accumulation is made of, so the genotype effect grows with every dose and is at its largest at steady state, which methadone takes a fortnight to reach.

## Try next

- Set **CYP 2B6** to *Intermediate*, the commonest reduced-function group (one normal and one reduced allele, such as `*1/*6`). The day-14 trough is 100 ng/mL, between the other two: the effect is graded by allele count.
- Change the units to `mg bid` to give the same dose intravenously. Every concentration rises by about a seventh, the reciprocal of the 0.70 oral bioavailability, but the ratio between the two phenotypes stays at 1.39. That is a limit of the model, not a finding: the genotype here scales systemic clearance only, while Kharasch measured a larger genetic effect by mouth than by vein, because an oral dose is also metabolised on its first pass through the liver.
- Add a `0 mg PO bid` row at day 14 to stop the drug, and set *Max time* to 28 days. Three days after the last dose the poor metaboliser still has 41 ng/mL and the normal metaboliser 22 ng/mL, so the genotype changes how long the drug lingers as well as how high it reaches.
- Compare [a single intravenous bolus](scenario:methadone-bolus), which shows the same disposition over a day instead of a fortnight.

## Background

[Methadone](help:drugs/methadone) records the model: Henthorn and Kharasch's separate R(-) and S(+) kinetics, summed to the racemate, with the CYP2B6 multipliers taken from Kharasch and colleagues (*Anesthesiology* 2015;123:1142-1153), who gave intravenous and oral methadone to 64 volunteers, 58 of them in cohorts chosen for their `*1/*1`, `*1/*6` and `*6/*6` genotypes (21, 20 and 17). The reference, the normal metaboliser, is Henthorn's published typical patient. The drug's page tabulates clearance and half-life at each phenotype.

Two cautions. The CYP2B6 rapid and ultrarapid categories carry the normal metaboliser's clearance here. Kharasch's carriers of the increased-function allele were four subjects of two different CPIC phenotypes, pooled and untested, which is too little to set a multiplier. And a pharmacokinetic difference is not a dosing rule: the CPIC guideline for CYP2B6 and methadone (Robinson and colleagues, *Clin Pharmacol Ther* 2024;116:932-938) recommends standard dosing, titration and monitoring for rapid, normal, intermediate and poor metabolisers, and makes no recommendation for ultrarapid ones, for whom there are no data. The exposure difference has not been shown to change treatment outcome or QTc prolongation, and genotype has not been studied in pain at all. Genotype is also not the whole of CYP2B6 activity: the enzyme is induced by rifampin, carbamazepine, phenobarbital, efavirenz and by chronic methadone itself, none of which is modelled. See [Cautions](help:cautions).
