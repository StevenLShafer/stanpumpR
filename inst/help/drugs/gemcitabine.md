### The model

Gemcitabine comes from Tham and colleagues (*Cancer Chemother Pharmacol* 2008;63:55-64, with its 2015 erratum): 56 Asian adults with non-small-cell lung cancer given 1000 mg/m² over 30 minutes or 750 mg/m² over 75 minutes. Two compartments describe gemcitabine, which is cleared very fast (305 L/h for a 70-kg, 1.76-m man), mostly by deamination to the inactive dFdU, which is plotted in its own panel, [gemcitabineDFDU](help:drugs/gemcitabineDFDU):

- central volume 2.96 L, peripheral 47.6 L, intercompartmental clearance 156 L/h;
- 73% of the clearance forms dFdU, 23% the active triphosphate inside cells (dFdCTP, not plotted), and 4% leaves unchanged, fractions the authors took from a mass-balance study;
- half-lives of under a minute and 19 minutes; dFdU's 1.7 hours.

### What is plotted

Plasma gemcitabine in mcg/mL (mg/L). Doses are entered in mg as labelled; a dose prescribed per m² must be multiplied by the body surface area first. There is no effect site and no band.

### Covariates

**Lean body mass**, from weight, height and sex, on every parameter: clearances to the ¾ power, volumes in proportion, relative to a 70-kg, 1.76-m man. The paper prints the lean-body-mass equation (Janmahasatian's, which the library's Al-Sallami equation equals in adults) but neither its constants nor the exponents; the standard ones are used.

### Where to be careful

- **The triphosphate is not plotted.** It is the active form, inside cells; the paper modelled it in blood mononuclear cells, which are not the tumour.
- **The first minutes are poorly known**: the central volume is essentially unidentified (the first sample was at 10 minutes).
- **Not a monitoring tool.**

Every parameter was read in the paper's PDF and erratum.
