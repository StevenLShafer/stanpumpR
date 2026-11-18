### The model

Frank's population analysis of lixisenatide (*J Pharm Drug Deliv Res* 2013): 1223 healthy subjects and patients with type 2 diabetes in seven studies, given single or repeated subcutaneous doses. Every visit with anti-lixisenatide antibodies, or with an unknown antibody status, was excluded, because antibodies raise exposure markedly and unpredictably; the model describes antibody-negative patients.

- **Disposition:** one compartment. Apparent clearance 38.6 L/h and volume 41.2 L at the population medians.
- **Absorption:** first order, with a mean absorption time of 3.25 hours after 15 mcg. Absorption is slower than elimination, so the decline after the peak, with a half-life of about 2.3 hours, is set by absorption.

### Covariates

- **Renal function:** clearance × (1 + 0.099 × (creatinine clearance − 6.21 L/h)), with creatinine clearance by Cockcroft-Gault in L/h (6.21 L/h is 103.5 mL/min), from the entered serum creatinine or an [assumed normal](help:models/covariates) one. Moderate impairment, 1.8 to 3.0 L/h, raises exposure 1.5- to 1.8-fold.
- **Weight:** volume × (weight/82.1)<sup>1.56</sup>. With **Adjust weight to fat-free mass** ticked, the pharmacokinetic weight is used, in the volume and in Cockcroft-Gault.
- **Dose:** mean absorption time × (dose/15 mcg)<sup>0.236</sup>, a property of each injection: 2.95 hours after 10 mcg and 3.48 hours after 20 mcg. Each injection size is simulated with its own absorption and the results are added.

The weight term on the volume is held to the studied weights, 43.8 to 163 kg, and the dose term on the absorption time to the studied doses, 5 to 30 mcg per injection (the paper's Table 1). Creatinine clearance is not held: the table gives no numeric range, only that the renal study included severe impairment. A dedicated renal study tabulated in a later review (Scheen 2015) found a lower clearance after a single 5 mcg dose (20 L/h with normal renal function) than this pooled model gives.

At the medians, 20 mcg peaks at about 88 pg/mL 1.8 hours after the injection.

### What is not plotted

Concentration only, in pg/mL: not glucose, insulin or HbA1c. See [Antidiabetic models and their limits](help:models/antidiabetics).
