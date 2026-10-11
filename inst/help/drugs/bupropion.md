### The model

Bupropion's kinetics are from Ghimire and colleagues (*J Clin Pharm Ther* 2026, [doi:10.1155/jcpt/3265655](https://doi.org/10.1155/jcpt/3265655)), a population analysis of a single 150 mg **sustained-release (SR)** tablet in 19 adults, many of them with chronic kidney disease, with bupropion and its active metabolite hydroxybupropion measured in plasma. Each was described by two compartments. For bupropion:

| Parameter | Typical value |
|---|---|
| Vc/F | 524.6 L |
| CL/F | 148.4 L/h |
| Vp/F | 5006.9 L |
| Q/F | 257 L/h |
| ka | 0.29 /h |

These give half-lives of 0.86 hours and 38.5 hours. The terminal half-life is longer than the 21 hours or so usually quoted for bupropion, which may reflect the single dose and small, kidney-disease-weighted sample; the parameters are used as published. The drug file converts the clearances to litres per minute, the library's unit.

### Oral only, and the curve is the SR tablet's

No subject received intravenous bupropion (there is no intravenous product), so every parameter is **apparent**, divided by the unmeasured oral bioavailability. Apparent parameters predict oral concentrations correctly, because the unknown bioavailability cancels, and only oral units are offered: **mg PO** for a single dose, and **mg PO qd** or **mg PO bid** for a dose repeated daily or twice daily until the end of the plot. Enter the dose as labelled (mg of bupropion hydrochloride); no salt conversion is applied.

The absorption rate, 0.29 per hour, is the SR tablet's, and includes its release from the tablet. The units are generic, but **the curve they draw is that of the sustained-release tablet**. The immediate-release tablet absorbs faster and peaks higher, and the extended-release (XL) tablet is different again: Zhang and colleagues (2019) found an apparent clearance of 221 L/h at 60.9 kg and an absorption lag for XL, and their 2017 crossover of the IR, SR and XL tablets found relative bioavailabilities of 1, 0.955 and 0.68, so an XL dose entered here would be overpredicted by about a third. Neither is modelled. Average concentrations on a steady regimen depend only on the daily dose over the clearance, so they are less sensitive to the formulation than the peaks and troughs are.

In this model a single 150 mg SR tablet gives a bupropion peak of about 61 ng/mL at 2.2 hours, and hydroxybupropion peaks at about 263 ng/mL at 3.9 hours.

### Active metabolite

Ghimire and colleagues split bupropion's clearance into a part that forms hydroxybupropion and a part that does not, fixing the forming fraction at **0.1**: one tenth of bupropion's clearance, 14.84 L/h, forms [hydroxybupropion](help:drugs/hydroxybupropion), mass for mass, and bupropion's own curve is unchanged by it, its total clearance already including formation. The metabolite's curve appears on its own row. At steady state, or over the whole of a single dose, hydroxybupropion's exposure is 14.84 / 0.9 = **16.5 times** bupropion's, its own clearance being only 0.9 L/h. See [Active metabolites](help:models/metabolites).

Bupropion is **not a prodrug**: it is itself an active noradrenaline and dopamine reuptake inhibitor. Neither it nor its metabolite has an effect site in the model, because there is no validated relation between concentration and antidepressant effect, so both rows plot plasma concentrations. Positron emission tomography gives context only: Learned-Coughlin and colleagues (2003) found about 26% occupancy of the striatal dopamine transporter on an SR regimen, without a concentration for half-maximal occupancy.

### Covariates

None. Kidney disease and vitamin D were examined in the source and not retained, so the model is a typical adult's and does not respond to the creatinine field. The parameters take the library's default [fat-free-mass scaling](help:models/fat-free-mass) for fixed published values, taken as the 70 kg reference man's, and are used exactly as published with the switch off. Hydroxybupropion is scaled identically. The subjects were adults, so the child and infant in the table above are extrapolation.

### Typical concentrations and regimens

No band is drawn on this row. The AGNP 2018 consensus therapeutic reference range (Hiemke and colleagues, *Pharmacopsychiatry* 2018;51:9-62), 850 to 1500 ng/mL, is for **bupropion plus hydroxybupropion**, and is drawn on the hydroxybupropion row because the metabolite makes up most of the sum.

On **150 mg PO bid** the average steady state is 300 mg/day over 148.4 L/h, 84 ng/mL of bupropion, and 16.5 times that, 1389 ng/mL, of hydroxybupropion: a sum of about 1470 ng/mL, near the top of the range. At four weeks the model gives bupropion from 58 to 115 ng/mL over a dosing interval and hydroxybupropion from 1281 to 1480 ng/mL, about 95% of the way there by the end of the first week.

### Where to be careful

- **The curve is the SR tablet's.** IR and XL doses are not modelled, and XL doses would be overpredicted.
- **The source is small and single-dose**, weighted towards chronic kidney disease, and its interindividual variability was incomplete; the curve is a typical patient's only.
- **No concentration-response relation** exists for the antidepressant effect, and the therapeutic range is for the sum of the parent and the metabolite.
- **The metabolite's parameters are conditional** on the fixed forming fraction; see its page.
