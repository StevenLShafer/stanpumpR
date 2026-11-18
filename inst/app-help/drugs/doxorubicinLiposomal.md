### The model

Hsu (*Pharm Res* 2018;35:106) fitted the bioequivalence data of women with ovarian cancer given the innovator product, pegylated liposomal doxorubicin (Doxil, Caelyx), 50 mg/m² over an hour. Two analytes were measured and fitted together:

- **Doxorubicin inside the liposomes**, 98% of the dose, held in one small volume (2.52 L) and leaving it only as the drug is released, with a half-life of about 82 hours. Uptake of the liposomes themselves by the reticuloendothelial system was estimated near zero and fixed there.
- **Free doxorubicin**, the released drug plus the 2% of the dose never encapsulated, in two compartments (9.52 and 35.1 L) with a clearance of 2.68 L/h. stanpumpR draws it as a second panel, [free doxorubicin](help:drugs/doxorubicinLiposomalFree), in ng/mL.

### What is plotted

Encapsulated doxorubicin, in mcg/mL, which is about 99% of the total doxorubicin in plasma; free doxorubicin, in ng/mL, on its own panel. There is no therapeutic range, so no band is drawn.

### Where to be careful

- **Not conventional doxorubicin.** The liposome changes everything: Doxil is a different product with different dosing, and its free doxorubicin is not modelled as ordinary [doxorubicin](help:drugs/doxorubicin). In a fit like Hsu's only the ratio of release to elimination is pinned down, and the free clearance it gives, 2.68 L/h, is about a twentieth of conventional doxorubicin's.
- **A 50 mg/m² curve.** The label reports that Doxil's kinetics are nonlinear. At 10-20 mg/m², clearance is higher (0.041-0.056 L/h/m²) and the half-life shorter (52-55 hours), so at lower doses the curve overestimates exposure.
- **No covariates.** The study reported none; the curve does not change with body size, and the dose is entered per patient.
- **Pregnancy:** no pharmacokinetic data were found; nothing is changed.
