### The model

Paclitaxel comes from Joerger and colleagues (*Clin Cancer Res* 2006;12:2150-2157): 168 Dutch adults with solid tumours, mostly non-small-cell lung and ovarian cancer, given solvent-based paclitaxel (Taxol, in Cremophor EL), 100 to 250 mg/m² over 3 hours. Paclitaxel's disposition is **nonlinear twice over**: its elimination saturates, and so does its passage into the tissues. Three compartments describe it:

- elimination from the central compartment at a maximal rate VMEL of 37.4 µmol/h (31.9 mg/h) in the typical woman, half-saturated at 0.53 µmol/L (0.45 mg/L);
- transport into a first peripheral compartment at a maximal rate VMTR of 169 µmol/h (144 mg/h), half-saturated at 0.826 µmol/L (0.71 mg/L), with return at 1.15 per hour; this compartment is an amount, with no volume of its own;
- linear distribution to a second peripheral compartment of 252 L, at an intercompartmental clearance of 20.1 L/h;
- a central volume of 12.8 L.

The engine integrates these equations numerically over the whole dose table, because doses do not add independently.

The model reproduces the paper's own simulations. For 175 mg/m² over 3 hours at a body surface area of 1.8 m², the typical woman reaches 3.83 µmol/L (3.27 mcg/mL) and stays above 0.1 µmol/L for 17.7 hours; the typical man reaches 3.29 µmol/L and stays above it for 13.0 hours. These are the values the paper's Figures 3 and 4 show.

### What is plotted

Total plasma paclitaxel in mcg/mL (mg/L); 0.1 µmol/L is 0.085 mcg/mL and 0.05 µmol/L is 0.043 mcg/mL. Doses are entered in mg or as an infusion rate; a dose prescribed per m² must be multiplied by the body surface area first. The time above 0.05 or 0.1 µmol/L relates to neutropenia, but it is a duration, not a concentration band, and is not drawn. There is no effect site.

### Covariates

- **Sex.** Men have a 20% higher maximal elimination and transport rate, a transport Km 2.11 times higher and a slower return from the first peripheral compartment, so a man's time above 0.1 µmol/L is almost 5 hours shorter than a woman's at the same dose.
- **Body surface area** (Mosteller's formula; the paper does not name its own), on both maximal rates, the second peripheral volume and its clearance. With the dose given per m², the time above 0.1 µmol/L barely changes with body size.
- **Age**, on the maximal elimination rate: 14% lower at 86 years than at 56. The term is held at 18 years for younger patients; the youngest studied was 33.
- **Total bilirubin** lowers the elimination rate (by 19% at 24 µmol/L), but the Patient Profile has no bilirubin field, so it is **held at 7 µmol/L** (0.41 mg/dL), the study's median. A patient with a raised bilirubin will stay above the thresholds longer than this curve shows.

### Where to be careful

- **Solvent-based paclitaxel only.** Albumin-bound (nab-) paclitaxel has a different disposition, and Cremophor's own nonlinearity is part of what this model describes.
- **The paper's Results swap two labels**, calling the sex-dependent transport Km (0.83 and 1.74 µmol/L) the elimination Km. Its Table 2 and covariate equations are right and are used; only they reproduce its figures.
- **Concurrent drugs** (carboplatin in some patients, CYP2C8 and CYP3A4 inhibitors) were not covariates.
- **Adults only. Not a monitoring tool:** the curve is a population prediction, not an estimate from measured levels.

Every parameter was read in the paper's PDF. See [chemotherapy](help:models/chemotherapy) for the other models considered.

### Pregnancy

With Pregnant ticked, Janssen 2021's factors are applied (the concentration at half-maximal elimination ×1.32, central volume ×2.27, the linear peripheral volume ×2.28), from 20 pregnant women, fitted on a model of this one's structure; the peak falls about a fifth and the late concentrations rise, the larger peripheral volume slowing the terminal fall.
