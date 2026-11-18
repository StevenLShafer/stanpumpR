### The model

High-dose methotrexate in adults comes from Ibarra and colleagues (*Br J Clin Pharmacol* 2023;89:660-671): 693 courses in 243 adults aged 18 to 88 (median 60), given 0.5 to 13.4 g/m² over 3 to 24 hours in 101 US oncology practices. Three compartments describe the disposition, every parameter scaled by weight (the clearances to the ¾ power, the volumes in proportion, for 70 kg):

- clearance 5.92 L/h, times (reference creatinine / serum creatinine)^0.694 and (albumin / 4 g/dL)^0.594;
- central volume 14.4 L, and two peripheral compartments: 3.55 L exchanging at 0.571 L/h, and 2.09 L exchanging at 0.0365 L/h.

The half-lives are 1.5, 4.9 and 40 hours and the steady-state volume 20 L, as the paper reports. The 40-hour phase is the slow tail that decides when rescue can stop. For children and young adults, use [methotrexatePediatric](help:drugs/methotrexatePediatric) (Taylor 2023).

### What is plotted

Plasma methotrexate in mcg/mL (mg/L). The clinical thresholds are in µmol/L: **1 µmol/L = 0.454 mg/L**, so 0.1 µmol/L is 0.045 mg/L. Doses are entered in mg or as an infusion rate; a dose prescribed per m² must be multiplied by the body surface area first. There is no effect site, and the rescue and discharge thresholds are not drawn as a band.

### Covariates

- **Weight**, on every parameter. With [the scaling switch](help:models/fat-free-mass) on it is the pharmacokinetic weight; with it off, the actual weight.
- **Serum creatinine**, from the Patient Profile. The paper's reference is 0.92 mg/dL for men and 0.77 for women, the healthy-adult means its Methods give. Left blank, the library's assumed 0.9 or 0.8 mg/dL is used, which puts clearance 1.5% above (men) or 2.6% below (women) the paper's typical value.
- **Serum albumin**, from the Patient Profile, as (albumin / 4 g/dL)^0.594 on clearance. This matters more here than for most drugs: 68% of the courses in the study were hypoalbuminaemic, and clearance is 11% lower at 3.3 g/dL (the cohort's median) and 24% lower at 2.5 g/dL. Left blank, albumin is held at the reference, 4 g/dL, and for a hypoalbuminaemic patient the curve then underestimates exposure, so enter it when it is known.

### Where to be careful

- **Not a monitoring tool.** The curve is a population prediction for the covariates entered, not an estimate from measured levels, and never replaces them or the rescue and glucarpidase criteria.
- **Kidney injury during the course.** The source's creatinine changed with time; the curve holds the entered value. It cannot show the delayed elimination of a kidney injury that develops during the course.
- **Left out by the authors:** shorter infusions (12% lower clearance), a kidney injury in the previous course (8%), and age.

Every parameter was read in the paper's PDF. The paper does not print its reference creatinine as a number; the values used are the healthy-adult means given in the same paragraph of its Methods.
