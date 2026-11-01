### The model

From Bamgboye and colleagues (*J Clin Pharmacol* 2026), who gave 20 adults a stable-isotope-labelled intravenous microdose alongside their oral topiramate: three compartments, linear, allometric on weight (the pharmacokinetic weight under the default [fat-free-mass scaling](help:models/fat-free-mass), total body weight with the switch off), with a clearance of 1.31 L/h, a central volume of 9.84 L and steady-state volume near 58 L at 70 kg. The terminal half-life is about 30 hours.

### Route

Oral (`mg PO`, with schedules). Absolute oral bioavailability is complete (109 per cent against intravenous, Clark 2013; about 100 per cent, Ahmed 2015), so the apparent and true parameters are the same and bioavailability is 1. The label's figure of 80 per cent is a different quantity, the tablet's bioavailability relative to an oral solution, not against an intravenous dose. The absorption rate, 2.38 per hour (a half-time of about 17 minutes, with no lag), is the estimate of Ahmed and colleagues (2015), who fitted it together with a stable-labelled intravenous dose in the same volunteers. Their disposition has two compartments rather than three, but it is approximately the same system sampled more coarsely in its first minutes: their central volume (59 L) corresponds to this model's central and fast peripheral volumes together (49 L, a fifth smaller), which mix within a couple of minutes, ten times faster than absorption, so the rate carries across. A 100 mg dose peaks at about 1.9 mg/L an hour and a half after it (Clark 2013 measured a mean time to peak of 1.35 hours; the label says 1 to 2 hours), and the curve stays within 8 per cent of its peak from 1 to 4 hours. Until November 2026 the library used 2.0 per hour, calibrated to a 1-hour time to peak; the two rates give peaks 1 per cent apart and the same exposure. Clark's mean curve is still near zero 5 minutes after the tablet, which suggests a short delay that neither Ahmed's model nor this one carries. The extended-release capsules have no published input model and are not offered, nor is the investigational intravenous product.

### What is not modelled

The empirical dependence of apparent clearance on daily dose that an earlier oral model carried, which a linear engine cannot hold. Enzyme-inducing co-medication (carbamazepine, phenytoin or phenobarbital, which raise clearance about 1.6-fold) is applied only when the **Include antiseizure drug interactions** box in Graph Options is ticked (see [Antiseizure drug interactions](help:models/antiseizure-interactions)).

### Band

5 to 20 mg/L, 12 hours after a dose: the AGNP consensus guidelines' 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5, https://doi.org/10.1055/a-2860-7861; laboratory alert level 30 mg/L), the same range as the ILAE's (Patsalos and colleagues, 2008).
