### The model

Prednisolone's parameters are from Xu, Winkler and Derendorf (*J Pharmacokinet Pharmacodyn* 2007;34:355-372), who fitted prednisone and prednisolone **together** to digitised human oral and intravenous profiles. The two interconvert by 11β-hydroxysteroid dehydrogenase, so the source is a reversible two-species system on **free** concentrations: prednisolone volume 110.4 L, irreversible clearance 36.4 L/h, conversion to prednisone 34.8 L/h; prednisone volume 397 L, irreversible clearance 101 L/h, conversion to prednisolone 90.6 L/h.

### How a reversible pair fits a mammillary engine

After a prednisolone dose the free prednisolone concentration is biexponential, with half-times of 0.82 and 2.45 hours, and any such curve is reproduced **exactly** by a two-compartment mammillary model whose central volume is 110.4 L: the prednisone pool plays the part of the peripheral compartment. The code does that algebra, giving an effective clearance of 54.7 L/h, Q 16.4 L/h and V2 34.1 L. The equivalence is exact for an intravenous prednisolone dose or infusion.

An oral prednisolone dose enters the circulation 0.59 as prednisolone and 0.33 as prednisone (0.08 is lost), and the engine's single input cannot carry two species. The oral route therefore uses one effective bioavailability, 0.745, chosen so the free prednisolone AUC equals the source's exact value, with the source's absorption constant of 0.42/h. The oral exposure is exact; the oral shape is approximate.

### What is plotted

**Free** prednisolone, in ng/mL. Total prednisolone is a nonlinear, cortisol-dependent function of free that is not applied. The shaded band (10 to 100 ng/mL free) covers what 20 to 40 mg produce. Prednisolone also appears on this row as the active metabolite of [prednisone](help:drugs/prednisone).

### Covariates

None in the source. The parameters take the default [fat-free-mass scaling](help:models/fat-free-mass) and are used as published with the switch off.

### Effect site

None; the glucocorticoid effect is genomic and takes hours. Only the plasma concentration is plotted.

### Where to be careful

Xu's is a typical-profile fit to digitised data, not an individual-data population model, and its volumes are effective free-concentration volumes rather than plasma spaces. Prednisolone's saturable binding to transcortin, which makes total concentrations dose-dependent, is exactly what plotting free concentration avoids, so do not compare this curve with a laboratory's total prednisolone.
