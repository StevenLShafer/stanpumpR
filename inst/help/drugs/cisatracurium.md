### The model

Cisatracurium's parameters are from Sorooshian and colleagues (*Anesthesiology* 1996;84:1083-1091), a population (NONMEM) pharmacokinetic-pharmacodynamic model in 31 young (18 to 50 years) and 33 elderly (over 65 years) adults under nitrous oxide, fentanyl and isoflurane, with the first twitch measured at the adductor pollicis. It is a **two-compartment** model with elimination from the central compartment: clearance 319 mL/min and central volume 4.76 L at every age, peripheral volume 4.94 L in the young. The weight-normalized form of the model did not fit better, so the parameters are absolute.

The paper does not print the distribution clearance. The value used, 0.297 L/min, is the one that reproduces both of the published half-lives in the young (4.2 and 28.4 minutes). With it, the model reproduces the paper's own predicted time course (its Table 3) to within a minute at every dose. Ornstein and colleagues (*Anesthesiology* 1996;84:520-525) and Lien and colleagues (*Anesthesiology* 1996;84:300-308) report clearances of 4.6 to 5.3 mL/kg/min and terminal half-lives of 21 to 26 minutes, consistent with this model.

### Covariates

**Age.** The peripheral volume is 37% larger in the elderly, which with the same distribution clearance reproduces both elderly half-lives (4.5 and 36.3 minutes). The paper's Table 2 lists an elderly steady-state volume (13.3 L) that is 37% larger than the young Vss rather than reflecting a 37% larger V2; the text is followed because only it agrees with the published half-lives. Ages 50 to 65 were not studied; the volume and the time to peak effect are interpolated linearly across that gap.

**Size.** With *Adjust weight to fat-free mass* ticked, the volumes scale with fat-free mass relative to the 70 kg, 170 cm reference man and the clearances with that ratio to the 0.75 power. Unticked, the parameters are used as published, which is how the source found them best.

### Effect site

Sorooshian's ke0 is 0.071/min in the young and 0.060/min in the elderly, estimated with these same kinetics. They are carried as the times to peak effect they give, 12.3 and 12.9 minutes, from which ke0 is solved again for each patient, so the reference patients recover the published values. The slow equilibration is why onset is late after a small dose and quicker after a large one.

### Typical concentrations and recovery

Sorooshian's concentration for 50% twitch depression is 98 ng/mL in a young 70 kg adult (90 ng/mL in the elderly, and 10% higher for every 10 kg of weight), with a slope of 4.1. From those, the shaded band runs from 130 ng/mL (the first twitch recovered to 25% of baseline) through 165 ng/mL (90% block) to 200 ng/mL (95% block). The default recovery threshold is 130 ng/mL, so *Time until threshold* is the time to 25% recovery. The plot shows concentration, not twitch height, and the age and weight effects on potency are not applied to the band.

### Where to be careful

Cisatracurium is degraded by Hofmann elimination, which is slowed by hypothermia and acidosis; neither is modelled. Its metabolite laudanosine is not modelled. Because the drug is degraded in tissue as well as in plasma, a central-elimination model underestimates the true steady-state volume, by a factor of 1.55 in Sorooshian's estimate, though not the clearance or the concentrations it predicts. Doses are in mg of cisatracurium, the labelled strength.
