### The model

Diphenhydramine's pharmacokinetics come from Meredith and colleagues (*Clin Pharmacol Ther* 1984;35:474-479), who gave 0.8 mg/kg diphenhydramine hydrochloride intravenously to eight healthy men (mean age 43) and nine men with cirrhosis, and sampled venous plasma for 48 hours. Two exponentials described the healthy men's curves. The model takes their mean central volume (3.3 L/kg), clearance (9.8 mL/min/kg) and the two rate constants (half-lives 57 minutes and 8.8 hours), converted exactly to two compartments. Its volume at steady state, 6.4 L/kg, agrees with the paper's 6.5. At 70 kg that is a central volume of 231 L and a clearance of 0.68 L/min.

Diphenhydramine is cleared almost entirely by metabolism: only 2% of a dose appears unchanged in the urine. The terminal half-life of about 9 hours means that the concentration falls quickly for the first hour or two, as the drug distributes, and then slowly.

### The first minutes

Meredith's first sample was at 15 minutes. The central volume is extrapolated back from there, and any faster distribution in the first minutes was not seen. So the model probably **underestimates** the plasma concentration in the first minutes after an injection, which is when intravenous diphenhydramine sedates. Blyden and colleagues (1986) sampled from 5 minutes but did not publish the curve's exponents. Albert and colleagues (1975) bear this out: at the end of a 1-hour infusion the model is about a third below what they measured in two men, although their clearance agrees with Meredith's. Their two men's curves were too different, and too sparsely sampled after the infusion, to build a better model from, as the authors themselves said.

### How well it predicts other studies

- **Carruthers and colleagues 1978** (six young volunteers, four of them women, 50 mg intravenously; not used to build the model): 144 ± 17 ng/mL measured at 1 hour, 141 predicted; 37 ± 3 measured at 8 hours, 44 predicted. Their volunteers cleared the drug faster than Meredith's men.
- **Blyden and colleagues 1986** (ten volunteers, an assay that separates the main metabolite): clearance 6.2 mL/min/kg, a third below Meredith's. Clearance in the other published studies ranges from 9.8 to 12 mL/min/kg. The difference has not been explained.
- **Albert and colleagues 1975** (*J Pharmacokinet Biopharm* 3:159-170, doi [10.1007/BF01067905](https://doi.org/10.1007/BF01067905); two men of 76 and 95 kg, 50 mg infused over 1 hour): clearance 9.3 and 8.2 mL/min/kg, against Meredith's 9.8. At the end of the infusion they measured 258 and 179 ng/mL, where the model gives 160 and 128. From 12 hours the model runs above their measurements, where their assay was near its limit. Both men slept after the infusion, and the authors suggested that sleep may occur above about 70 ng/mL.

Clearance varied 1.6-fold even among Carruthers' six healthy volunteers (637 to 1,014 mL/min), so individual patients can differ widely from the curve.

### What is plotted

Plasma diphenhydramine in ng/mL. Doses are entered as labelled, as diphenhydramine hydrochloride. The paper does not say whether its volumes are per mg of the salt or the base, but its own central volume matches the hydrochloride, so no conversion is made.

### The effect site

No study has measured how fast diphenhydramine's effect follows its plasma concentration. The time to peak effect, **2 minutes**, is a clinical choice, from Meredith's observation that every subject was sedated 1 to 2 minutes after the injection.

### The band

The band, 30.4 to 74.4 ng/mL with a typical value of 41.5, comes from Gengo and colleagues (*Clin Pharmacol Ther* 1989;45:15-21, doi [10.1038/clpt.1989.3](https://doi.org/10.1038/clpt.1989.3)), who gave 50 mg orally to fifteen healthy men and measured the concentrations at which the effects wore off:

- **drowsiness** wore off between 30.4 and 41.5 ng/mL among the subjects (41.5 on average);
- **mental impairment** (digit symbol substitution) wore off between 58.2 and 74.4 ng/mL (68 on average).

So above the band most people are both drowsy and impaired, within it most are drowsy, and below it most are neither. The threshold for the time until threshold is 41.5 ng/mL, the time until drowsiness is expected to wear off. Carruthers found significant antihistamine effect, without significant sedation, at about 25 to 50 ng/mL.

The German consensus guideline for therapeutic drug monitoring (Hart 2026, Table 5) gives 10 to 60 ng/mL, sampled 10 hours after a dose, for diphenhydramine used for sleep, with a laboratory alert level of 200 ng/mL. That range comes from the same Gengo study but is for nightly oral use, not an intravenous injection, so it is not drawn.

### Covariates

None was estimated. The parameters are per kilogram; with [the scaling switch](help:models/fat-free-mass) on they are scaled to fat-free mass, and with it off linearly in weight.

### Where to be careful

This model is for healthy adults. In Meredith's men with cirrhosis the terminal half-life was 15 hours instead of 9, and the label warns that older patients are more likely to become dizzy, sedated or hypotensive. Diphenhydramine adds to the effects of alcohol and other sedatives, and it has atropine-like effects. The US label contraindicates it in newborns and premature infants and in nursing mothers. It recommends 10 to 50 mg intravenously for adults, given no faster than 25 mg/min, up to 100 mg if required and no more than 400 mg a day. For children it recommends 5 mg/kg or 150 mg/m² a day in four doses, up to 300 mg a day.

No sedation, antihistamine or anticholinergic effect is predicted.
