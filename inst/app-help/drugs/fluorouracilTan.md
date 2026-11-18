### The model

This is the library's second 5-FU model, beside [fluorouracil](help:drugs/fluorouracil) (Briki 2026). It comes from Tan and colleagues (*Clin Pharmacokinet* 2026;65:1027-1042), fitted to 140 adults from four prospective Dutch studies, mostly with colorectal or pancreatic cancer, given 46-hour infusions with or without a bolus. Two compartments describe the disposition, and 5-FU is eliminated by a **saturable pathway alone**:

- maximal rate Vmax 1,900 mg/h × (body surface area / 1.94 m²)^1.14, half-saturated at Km 8.04 mg/L;
- central volume 19.7 L, peripheral volume 15 L, intercompartmental clearance 52.4 L/h.

Body surface area was the only covariate retained. Sex, the DPYD activity score and the treatment cycle were tested and left out. The paper does not name its surface-area formula, and Mosteller's is used.

The model reproduces the paper's own simulated median AUCs (its Table 3) for a body surface area of 1.94 m²: 20.8 mg·h/L for 2,400 mg/m² over 46 hours (paper 21), 28.7 with a 400 mg/m² bolus first (29), and 23.6 with a 200 mg/m² bolus (24).

### How it differs from Briki 2026

Briki's model, from 726 patients, clears 5-FU by a linear pathway beside a smaller saturable one; Tan's has saturable elimination alone, with a larger capacity and a higher Km. For the same patient, Briki's gives an AUC about 14% lower: 17.9 against 20.8 mg·h/L for 2,400 mg/m² over 46 hours, 24.7 against 28.7 with a 400 mg/m² bolus. The difference lies between the two cohorts and neither paper settles it. Both are offered so the difference can be seen.

### What is plotted

Plasma 5-FU in mcg/mL (mg/L). Doses are entered in mg or as an infusion rate; a dose prescribed per m² must be multiplied by the body surface area first. The target for colorectal cancer, an AUC of 20 to 30 mg·h/L over the 46-hour infusion, is an exposure and is not drawn. There is no effect site.

### Where to be careful

- **DPD deficiency is not represented.** 29% of the patients carried a DPYD variant, and the genotype was not retained as a covariate, but a patient with low DPD activity clears 5-FU far more slowly than this curve shows. The CPIC and DPWG dose reductions are not clearances, and none is applied.
- **The slow rise during the infusion** and circadian variation are not modelled.
- **Adults only. Not a monitoring tool:** the curve is a population prediction, not an estimate from measured levels.

Every parameter was read in the paper's PDF. See [chemotherapy](help:models/chemotherapy) for the other models considered.
