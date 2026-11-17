### The model

Fludarabine comes from Langenhorst and colleagues (*Clin Pharmacokinet* 2019;58:627-637): 258 children and adults, aged 0.2 to 74 years and weighing 4.3 to 130 kg, given fludarabine phosphate over an hour on four days before an allogeneic stem-cell transplant. Three compartments describe F-ara-A, the circulating form, with every parameter scaled to weight about 70 kg:

- clearance (3.24 + 0.782 × eGFR) × (weight/70)^0.75 L/h, with eGFR in L/h;
- volumes 39, 20 and 50 L per 70 kg;
- intercompartmental clearances 8.6 and 3.8 L/h per 70 kg^0.75.

About two-thirds of the clearance is renal in an adult with normal kidneys. The half-lives in a 70-kg adult with an eGFR of 120 are 0.9, 3.5 and 15 hours.

The paper's text says the eGFR enters its equation "as L/h/kg", which would make the kidneys a trivial 2% of clearance against the paper's own 65%. Its abstract gives the terms per 70 kg, and only that reading reproduces the renal share and the paper's median adult exposure (a cumulative area of 26 mg·h/L). Steven Shafer decided (7 November 2026) to use it.

### Kidney function

The eGFR is the paper's own: Cockcroft-Gault from 17 years in girls and 14 in boys, the Schwartz equation below that, each with the patient's own serum creatinine (a blank field is the library's assumed normal creatinine for age and sex). It is capped at 140 mL/min/1.73 m², a cap that rises from 35 at birth to 140 at 18 months and falls by 8 per decade after 30.

### Dose: fludarabine phosphate

Enter doses as **fludarabine phosphate**, the approved dose form (365.2 g/mol). It is turned into F-ara-A (285.2 g/mol) within minutes, and the model converts the dose: each mg of the phosphate is 0.781 mg of F-ara-A. The curve is plasma F-ara-A in mcg/mL. The paper does not state its basis, but its exposures are reproduced only on this one.

### Where to be careful

- **No therapeutic range:** the paper's target is a cumulative area under the curve over the conditioning course, an exposure, not a concentration.
- **Pregnancy:** no pregnancy pharmacokinetics are known. Because most of the clearance is renal, the eGFR takes the 50% rise in filtration of late pregnancy when the creatinine is left blank, an assumption.
- **Not a monitoring tool:** the curve is a population prediction.

The parameters were read in the paper's PubMed Central text; the PDF is still to confirm them. See [chemotherapy](help:models/chemotherapy).
