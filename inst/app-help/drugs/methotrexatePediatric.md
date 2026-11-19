### The model

High-dose methotrexate in children and young adults comes from Taylor and colleagues (*Clin Transl Sci* 2023;16:2130-2143). Theirs is the model behind MTXPK.org, re-estimated in 1,668 patients aged 0.1 to 32 years at three US children's hospitals: 7,506 courses of 0.5 to 18 g/m² for acute lymphoblastic leukaemia, osteosarcoma and non-Hodgkin lymphoma, and 30,250 plasma concentrations followed to 380 hours. Three compartments describe the disposition, all scaled in proportion to body surface area (Mosteller) over 1.73 m²:

- clearance 8.24 L/h, times (serum creatinine / 40 µmol/L)^−0.26;
- central volume 22.1 L, and two peripheral compartments: 2.19 L exchanging at 0.225 L/h, and 6.07 L exchanging at 0.0342 L/h.

The slow third compartment is what gives high-dose methotrexate its long low tail, the part of the curve that decides when leucovorin rescue can stop.

### What is plotted

Plasma methotrexate in mcg/mL (mg/L). The clinical thresholds are given in µmol/L: **1 µmol/L = 0.454 mg/L**, so 0.1 µmol/L is 0.045 mg/L and 1 µmol/L is 0.45 mg/L. Doses are entered in mg or as an infusion rate. A dose prescribed per m² must be multiplied by the body surface area first. There is no effect site and no band: the thresholds that matter (glucarpidase, leucovorin rescue, discharge) are management criteria at stated times, not a therapeutic range.

For a typical child (body surface area 1.0 m², creatinine 0.3 mg/dL) given 5 g/m² over 24 hours, the model gives 86 µmol/L at the end of the infusion, against the paper's simulated mean of 85. After that the curve falls below the paper's means (0.33 µmol/L at 48 hours against 0.48), as a typical patient's should: a mean is pulled up by the slow eliminators.

### Covariates

- **Body surface area**, on every parameter in proportion. With [the scaling switch](help:models/fat-free-mass) on it is computed at the pharmacokinetic weight; with it off, at the actual weight.
- **Serum creatinine**, the patient's own value (not put on an adult scale), from the Patient Profile. Left blank, it is the normal value for the age and sex.
- **Serum albumin**, from the Patient Profile. The source's covariate is severe hypoalbuminaemia, an albumin of 2.5 g/dL or less, which speeds both distribution clearances (0.225 L/h ×1.17 and 0.0342 L/h ×1.34). It is all or nothing: any albumin at or below 2.5 g/dL gives the same curve, and above it, or left blank, the albumin is taken as normal.
- **Held at the reference**, because the Patient Profile has no field for them: a long infusion (a short one, 8 hours or less, clears 5% faster), no Down syndrome (clearance 16% lower), and **no pleural effusion**. An effusion enlarges the volumes, and the second peripheral volume by 43%.

### Where to be careful

- **Not a monitoring tool.** Methotrexate is dosed and rescued by measured levels at set times. This curve is a population prediction for the covariates entered, not an estimate from those levels, and it never replaces them, the rescue nomograms, or the glucarpidase criteria.
- **Kidney injury during the course.** The source's creatinine changed with time; the curve holds the entered value for the whole course. It cannot show the delayed elimination of an acute kidney injury that develops during the infusion, which is when methotrexate is most dangerous.
- **Children and young adults.** The cohort's oldest patient was 32. The adult high-dose models (Ibarra 2023, Blackman 2026) are on the [chemotherapy](help:models/chemotherapy) page.
- **Third spaces.** Pleural effusions and ascites hold methotrexate and release it slowly. The model can represent an effusion but is run without one.

Every parameter was read in the paper's PDF. Its Table 2 footnote calls the third compartment chained to the second, but the Methods specify NONMEM's mammillary three-compartment model, which is what is used.
