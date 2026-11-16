### Two models, chosen for you

Cytarabine has two published population models, each fitted in a different population, and stanpumpR picks the one that matches the patient. You don't choose: most clinicians have no data to choose between published models, and stanpumpR's rule is that the program makes that choice from the patient's own characteristics.

- **Under 19 years:** Periclou and Avramis (*Cancer Chemother Pharmacol* 1996;39:42-50), children aged 2 months to 19 years with leukaemia or lymphoma.
- **19 years and older:** Krogh-Madsen and colleagues (*Cancer Chemother Pharmacol* 2012;69:1155-1163), 23 adults aged 22 to 68 with acute myeloid leukaemia.

The References panel under the plot names the model in use.

### Adults (Krogh-Madsen 2012)

Two compartments: clearance 272 L/h, central volume 62.8 L, intercompartmental clearance 13.7 L/h, peripheral volume 75.4 L. The half-lives are about 9 minutes and 4 hours. The paper found clearance falls a little with a high white count at diagnosis; the Patient Profile has no white count, so it is held at the study's median. Samples were below the assay's limit about 2 hours after a dose, so the late part of the curve rests on few measurements.

### Children (Periclou 1996)

Two compartments, every parameter proportional to the child's age times body surface area: clearance 2.59, intercompartmental clearance 2.01 L/h, central volume 0.48 L and peripheral volume 38.1 L, each times age (years) × BSA (m²). For a 10-year-old of 1.1 m² that is a clearance of about 29 L/h.

This model has real limits, and you should know them:

- **No plateau with age.** Clearance is zero at birth and keeps rising with age. The model holds the age at 2 months (0.17 years), the youngest studied.
- **A jump at 19.** At 18.9 years a 70-kg, 170-cm man has a clearance of about 89 L/h by the children's model; at 19 the adult model gives 272 L/h. Steven Shafer decided (10 November 2026) to accept this jump rather than leave either age group without a model.
- **The same half-lives at every age**, about 4 minutes and 23 hours, because all four parameters scale together. The paper itself quotes 17 minutes and 4 hours from its earlier analyses.

### What is plotted

Plasma cytarabine (ara-C), in mcg/mL. The active drug is ara-CTP inside the leukaemic cell, which is not modelled, so there is no therapeutic range for the plasma curve. Doses are entered in mg; cytarabine has no salt.

### Where to be careful

- **Not in pregnancy:** no maternal pharmacokinetics are known, and nothing is adjusted.
- **Not a monitoring tool:** the curve is a population prediction.

Both papers were read on their PDFs. See [chemotherapy](help:models/chemotherapy).
