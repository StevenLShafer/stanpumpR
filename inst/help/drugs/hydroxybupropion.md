### The model

Hydroxybupropion is the active metabolite of [bupropion](help:drugs/bupropion), formed by CYP2B6. Its kinetics come from the same study as the parent's (Ghimire and colleagues, *J Clin Pharm Ther* 2026), fitted to the same single 150 mg sustained-release dose: two compartments, with these typical values:

| Parameter | Typical value |
|---|---|
| Vc/F | 2.2 L |
| CL/F | 0.9 L/h |
| Vp/F | 17.7 L |
| Q/F | 3.1 L/h |

These give disposition half-lives of 0.35 hours and 18.9 hours, close to the 20 hours usually quoted. A curve formed from bupropion, though, falls late at bupropion's slower rate (38.5 hours in this model), because a metabolite cannot leave faster than it is formed.

### Formed only

The source fixed the fraction of bupropion's clearance that forms hydroxybupropion at 0.1, because that fraction and the metabolite's own volumes and clearances cannot be told apart from concentrations alone. The parameters above are therefore conditional on that choice (and carry bupropion's unknown bioavailability), which is why the volumes are implausibly small. Scaling the metabolite's volumes, its clearances and the amount formed by any common factor leaves the *formed* concentrations unchanged, so the concentrations plotted after a bupropion dose are identified, but the scale for a direct dose is not. Hydroxybupropion is therefore offered with **no dosing unit**: it appears only when bupropion is given. See [Active metabolites](help:models/metabolites).

The formed amount is converted mass for mass. The source's own convention was not recovered; had it used molecular weights, the curve here would be low by the ratio of the two molecular weights, 255.74 / 239.74 = 1.067.

### Effect and concentrations

There is no validated concentration-response relation for the antidepressant effect, so the row plots plasma concentrations and has no effect site.

The shaded band, **850 to 1500 ng/mL** with 1200 as the typical value, is the AGNP 2018 consensus therapeutic reference range (Hiemke and colleagues, *Pharmacopsychiatry* 2018;51:9-62) for **bupropion plus hydroxybupropion**. It is drawn here because hydroxybupropion makes up most of the sum: its exposure is 16.5 times bupropion's in this model (one tenth of bupropion's 148.4 L/h, over the metabolite's 0.9 L/h). To compare with the range, add the bupropion row's concentration, a few tens of ng/mL to about a hundred. On 150 mg SR twice daily the model's average steady state is 1389 ng/mL of hydroxybupropion and 84 ng/mL of bupropion, a sum near the top of the range.

### Covariates

None was retained. The parameters take the same [fat-free-mass scaling](help:models/fat-free-mass) as bupropion's, identically, because the metabolite's apparent scale is consistent with the parent's only if the two scale together. Hydroxybupropion has been reported to accumulate in kidney failure, but the source did not retain kidney disease as a covariate, so the model does not respond to the creatinine field.

### Where to be careful

The parameters are conditional on the fixed forming fraction and on the source's mass convention, and come from a single dose in 19 adults; the true volumes and clearances differ by a factor the data do not identify, although the formed concentrations shown are unaffected. Steady-state concentrations on long-term therapy have not been checked against this model's source.
