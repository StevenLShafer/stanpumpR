### The model

Crombag and colleagues (*Pharm Res* 2019;36:33) refitted Joerger's paclitaxel model to 684 Dutch adults, 18 to 84 years, a quarter of them 70 or older, given solvent-based paclitaxel (Taxol, in Cremophor EL) as 1- or 3-hour infusions. The structure is Joerger's: elimination from the central compartment that saturates, passage into a first tissue compartment that saturates too, and ordinary distribution into a second.

- Elimination at a maximal rate of 31.7 µmol/h (27.1 mg/h) in the typical woman, half-saturated at 0.46 µmol/L (0.39 mg/L).
- Transport into the first tissue compartment at up to 177 µmol/h, half-saturated at 1.63 µmol/L, returning at 1.20 per hour.
- A second tissue compartment of 267 L, reached at 16.6 L/h; a central volume of 12.0 L.

These are the values for the study's typical patient. With *Adjust weight to fat-free mass* ticked, all but the maximal elimination rate are scaled to the patient's fat-free mass (see below).

### Sex, size and age

Only the maximal elimination rate carries covariates: 12% higher in a man, rising with body-surface area (to the power 1.41, about 1.8 m²), and falling 5% for each decade of age (to the power −0.31, about 61 years). Bilirubin lowers it too; the Patient Profile has no bilirubin, so the study's median, 7 µmol/L, is assumed.

Crombag took this covariate model from Joerger's 2012 analysis and tested only age in her own data, so size was never tested on the central volume, the second tissue volume, the maximal transport rate or the intercompartmental clearance. With *Adjust weight to fat-free mass* ticked, stanpumpR scales the two volumes in proportion to fat-free mass and the transport rate and intercompartmental clearance to its 0.75 power, anchored at Crombag's typical patient (301 men and 383 women, median 61 years, body surface area 1.8 m²; the paper prints no weight or height, so 67.6 kg and 172.3 cm are derived from the body surface area and Dutch heights), who receives the published values. The maximal elimination rate keeps its own body-surface-area term and takes nothing more. Unticked, the four are as published.

### What is plotted

Total plasma paclitaxel, in mcg/mL. The measure used clinically is the time spent above 0.05 µmol/L, an exposure linked to neutropenia and neuropathy; it is not a therapeutic range, so no band is drawn.

### Where to be careful

- **Solvent-based paclitaxel only.** Nab-paclitaxel (Abraxane) is a different product with different kinetics.
- **Bilirubin is held at the median.** A patient with high bilirubin clears paclitaxel more slowly than the curve shows.
- **Pregnancy:** with Pregnant ticked, Janssen 2021's factors are applied, from a refit of this model's own data with 20 pregnant women: the half-saturation of elimination ×1.32, the central volume ×2.27 and the second tissue volume ×2.28.
