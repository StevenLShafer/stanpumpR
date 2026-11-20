### The model

Mirabegron (Myrbetriq) is a β3-adrenoceptor agonist taken once daily for an overactive bladder, as an extended-release (OCAS) tablet. No population model of it in adults has been published in a journal. The model here is the manufacturer's population analysis (Astellas report 178-PK-015) as the FDA reproduced it in its Clinical Pharmacology and Biopharmaceutics Review of NDA 202611 (2012), read on the review's pages. It was built in two steps:

- **Disposition**, fitted to the intravenous data of 101 healthy subjects (7.5 to 50 mg infused over 2 hours): three compartments, clearance 50.5 L/h and central volume 19.8 L, with rate constants 2.39/h and 0.0399/h to and from a slow compartment and 3.15/h and 0.374/h to and from a fast one. These are absolute parameters, not apparent ones divided by an unknown bioavailability. At the reference the peripheral volumes are 167 and 1186 L, Vss 1373 L in a woman and 1567 L in a man (label: about 1670 L), and the terminal half-life is 35 hours.
- **Absorption**, fitted to 15,106 samples from 4081 patients with overactive bladder in six phase 2 and 3 studies, with the disposition fixed: each tablet is released into the gut at a constant rate over 2.79 hours and absorbed from it first order at 0.109/h (sequential zero- and first-order absorption). The bioavailability is 0.220 at 50 mg in the reference woman.

Intravenous mirabegron is not a product, so the drug is offered by mouth only (mg PO, and mg PO qd for a daily schedule).

### Covariates

- **Dose:** bioavailability × (dose/50 mg)<sup>0.443</sup>, applied to each dose at its own size, so exposure rises more than in proportion to the dose: 20-fold from 25 to 200 mg, as the review states. The table above gives the fraction at several doses. The term is applied as fitted up to 200 mg, the largest dose studied.
- **Sex:** clearance and central volume × 1.142 in men, and bioavailability × 0.830, so a man's exposure is about 27% below a woman's at the same weight.
- **Weight:** clearance and central volume × (weight/70)<sup>0.5</sup>, and every other volume and intercompartmental clearance with them, since the source fixed its rate constants. With **Adjust weight to fat-free mass** ticked, the pharmacokinetic weight is used.
- **Age:** bioavailability × (age/60)<sup>0.245</sup>, held to the studied 18 to 91 years. The model was fitted in adults and does not describe children.
- **Renal function:** bioavailability × (creatinine clearance/92 mL/min)<sup>−0.258</sup>, the Cockcroft-Gault clearance from the entered serum creatinine or an [assumed normal](help:models/covariates) one, held to the studied 27 to 262 mL/min. In the source the renal term acts on bioavailability, not clearance; renal clearance is about a quarter of the total.

### What was left out

- **The dose term on the central volume.** The source also makes the central volume depend on the dose, × (dose/50 mg)<sup>−0.453</sup>. stanpumpR cannot yet carry a parameter that changes with each dose's size together with a zero-order oral release, so the central volume is held at its value for 50 mg. The area under the curve does not depend on the central volume, so every exposure is the source's; the peak and trough are not, except at 50 mg. At steady state on once-daily dosing the peak is 7% high and the trough 9% low at 25 mg, and the peak 7% and 13% low (the trough 11% and 24% high) at 100 and 200 mg. The terminal half-life is 35 hours at every dose, where the source gives 42 hours at 25 mg and 27 at 200 mg.
- **Other medicines and food.** The source's terms for proton pump inhibitors (clearance × 0.908) and weak or moderate CYP3A4 inhibitors (central volume × 1.426) are not applied, as the Patient Profile has no field for them. The food term (central volume × 1.187 in the non-Japanese studies) is held at its reference; the label says the tablet is taken with or without food, and the reviewer found the food result confounded.
- **CYP2D6.** Not in the model. Poor metabolisers' exposure was about 17% higher, which was not significant in the population analysis; mirabegron itself is a moderate CYP2D6 inhibitor.

### Reference values

The review does not print the values each covariate is divided by. Its own simulations of the covariates are anchored at 70 kg, 60 years and a creatinine clearance of 92 mL/min, and the effects it states follow from the exponents at exactly those values; 60 years and about 91 mL/min are also the medians of the patients. They are used as the references. The manufacturer's simulated typical woman at 50 mg once daily (the review's Figure 20) peaks near 12.8 ng/mL with a trough near 5.8; this model gives 13.1 and 5.9, and 9.4 and 4.3 for the typical man (figure: 9.4 and 4.1). The peak comes 3.6 hours after a dose (label: about 3.5 hours).

### What the curve describes

The bioavailability was fitted in patients taking the tablet as they do; 72% of the samples were recorded as taken after food. It is lower than the 35% at 50 mg that the label reports from a fasting study in healthy volunteers, and the curve is flatter. The oral data were sparse, one to three samples a visit, and the FDA reviewer noted that the oral absorption profile could not be characterised precisely, so the covariate effects should be read as effects on exposure. In healthy volunteers taking 50 mg once daily the steady-state peak was 54 to 58 ng/mL and the area over a day 413 to 471 ng·h/mL, against this model's 9 to 13 ng/mL and 161 to 224 ng·h/mL. Food explains part of the difference in the area (fasting raised the area by 17% over a high-fat meal and by about half over a low-fat one) and less of the difference in the peak. Read the curve as the manufacturer's description of exposure in patients with overactive bladder: its area and trough rather than its peak. A curve for a drug taken at home also assumes that every dose was taken ([Cautions](help:cautions)).

### Typical range

No band is drawn. No therapeutic or toxic concentration range has been established for mirabegron: it is dosed to symptoms, and neither the label nor any consensus guideline gives one. Its effect on the pulse rate rises with exposure (the review estimates half the maximum rise at an area of 1086 ng·h/mL over a day), but that is not a therapeutic range.

### Source

FDA Center for Drug Evaluation and Research. Clinical Pharmacology and Biopharmaceutics Review, NDA 202611 (mirabegron extended-release tablets), 2012, Reference ID 3129601: [accessdata.fda.gov](https://www.accessdata.fda.gov/drugsatfda_docs/nda/2012/202611Orig1s000ClinPharmR.pdf). Tables 7 and 8 and Figure 20 of its population pharmacokinetic appendix.
