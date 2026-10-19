### The model

Intravenous phenylephrine's parameters come from Hengstmann and Goronzy in Bonn (*Eur J Clin Pharmacol* 1982;21:335-341), who gave four healthy volunteers about 0.84 mg of tritium-labelled phenylephrine over 12 to 20 minutes and separated free phenylephrine from its conjugates in serum for eight hours. Each subject's curve was fitted with two compartments, and the paper prints every subject's rate constants and central volume. The model here is the mean of the four: a central volume of 41 L, about the extracellular volume, a peripheral volume of 300 L, a clearance of 2.09 L/min, about a third of cardiac output, and an intercompartmental clearance of 3.74 L/min. That reproduces the paper's mean clearance and steady-state volume exactly, and its half-lives, 4.6 minutes and 2.7 hours, are the paper's. It also reproduces the measured concentration at the end of the infusion, about 10 ng/mL.

### Dose basis and covariates

The paper does not name the salt; doses are entered as phenylephrine hydrochloride, as labelled. If the paper's milligrams were the base, every concentration is 22% high. The subjects' weights are not reported, so the parameters are fixed, scaled to fat-free mass when [the scaling switch](help:models/fat-free-mass) is on.

### Where to be careful

Four healthy volunteers. A second model (Anderson and colleagues, 2017), fitted mostly to children given nasal phenylephrine with a few adult intravenous subjects, has a longer terminal half-life; the early, clinically important part of the curve is similar. Phenylephrine's effect on blood pressure is immediate and not modelled here. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
