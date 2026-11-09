### The model

This third doxorubicin model comes from Pérez-Blanco and colleagues (*Br J Clin Pharmacol* 2016;82:1517-1527): 44 adults with non-Hodgkin lymphoma given 50 mg/m² over half an hour as part of R-CHOP, sampled up to 3 hours after the infusion. Three compartments describe doxorubicin, and 22% of its clearance forms doxorubicinol, which has two compartments of its own (drawn as **doxorubicinolPerezBlanco**):

- doxorubicin: clearance 62.4 L/h; volumes 17.7, 1830 and 71 L; intercompartmental clearances 50.7 and 28.4 L/h;
- doxorubicinol: clearance 26.8 L/h; volumes 79.8 and 653 L; intercompartmental clearance 424 L/h.

There are no covariates (bilirubin and AST were tested). The half-lives are 5 minutes, 2.2 hours and 46 hours for doxorubicin, and 7 minutes and 20 hours for doxorubicinol. For the cohort's mean dose of 89 mg the areas are 1.43 mg·h/L for doxorubicin and 0.73 for doxorubicinol, as the paper computes them.

### Its limits

All five volumes were **fixed** rather than estimated: doxorubicin's from Kontny 2013 (the paper cites the wrong reference for them), doxorubicinol's from a sensitivity analysis. With sampling ending 3 hours after the infusion, the long final phase of the curve is those borrowed volumes, not something these patients' data showed. The clearance and the fraction converted to doxorubicinol are what this study measured.

### How it compares with the other doxorubicin models

The library's [doxorubicin](help:drugs/doxorubicin) (Joerger 2007, adults) and [doxorubicinPediatric](help:drugs/doxorubicinPediatric) (Voller 2015, children) are the others. Compare clearance first: it is the one parameter that measures a physiological process, and Pérez-Blanco's 62.4 L/h is a little higher than Joerger's (50.8 L/h as the library carries it).

### Dose

Doses are entered in mg as labelled (doxorubicin hydrochloride). The paper does not state salt or base and computes everything in mass, so no conversion is applied, and doxorubicinol forms mass for mass, as in the paper's own arithmetic.

### Where to be careful

- **No therapeutic range** is established for doxorubicin; the paper relates the area under the curve to toxicity.
- **Pregnancy:** studies in pregnant women found larger volumes and possibly a higher clearance, but no factor was measured well enough to apply.
- **Not a monitoring tool:** the curve is a population prediction.

The parameters were read in the paper's PubMed Central text; the PDF is still to confirm them. See [chemotherapy](help:models/chemotherapy).
