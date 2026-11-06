### The model

Doxorubicin in children comes from Völler and colleagues (*Clin Pharmacokinet* 2015;54:1139-1149): 94 children aged 0.2 to 17.7 years (median 5.3), body surface area 0.23 to 2.05 m², given infusions of 15 minutes to 24 hours. Three compartments describe doxorubicin, and half of its clearance forms the metabolite [doxorubicinolPediatric](help:drugs/doxorubicinolPediatric), plotted in its own panel. Every parameter scales linearly with body surface area, and clearance also rises with age:

- at the median child (0.77 m², 5.3 years), clearance 24.1 L/h per m², central volume 9.34 L/m², a large peripheral compartment of 560 L/m² and a small one of 27.8 L/m²;
- clearance per m² is 12.0 L/h in a newborn and 29.1 L/h in an 18-year-old of 1.8 m², as the paper reports;
- half-lives of 6 minutes, 2 hours and 31 hours at the median child.

The adults' model is [doxorubicin](help:drugs/doxorubicin) (Joerger 2007).

### What is plotted

Plasma doxorubicin in ng/mL. Doses are entered in mg as labelled; the paper does not say whether it recorded the hydrochloride or the base, so no conversion is made. A dose prescribed per m² must be multiplied by the body surface area first. There is no effect site and no band.

### Covariates

**Body surface area** (Mosteller's formula; the paper's own is in its supplement) on every parameter, and **age** on clearance: the youngest children clear doxorubicin about half as fast per m² as adolescents.

### Where to be careful

- **Children only.** An adult is an extrapolation; use the adults' model.
- **The fraction forming doxorubicinol** was fixed at 0.5, not estimated. Not a monitoring tool.

Every parameter was read in the paper's PDF, the clearance equation on the page image.
