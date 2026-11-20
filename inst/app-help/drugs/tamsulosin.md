### The model

Tamsulosin's parameters come from Tsuda, Tatami, Yamamura, Tadayasu, Sarashina, Liesenfeld, Staab, Schaefer, Ieiri and Higuchi (*Br J Clin Pharmacol* 2010;70:88–101), a population analysis of three Boehringer Ingelheim trials in **189 children aged 2 to 16 years** (12.1 to 92.2 kg, median 25 kg) with neuropathic or non-neuropathic bladder. They took 0.1 to 0.8 mg of tamsulosin hydrochloride once daily, the capsule's modified-release pellets sprinkled on a teaspoon of applesauce or yogurt, with food. The model is one compartment with first-order absorption after a lag: apparent clearance 2.28 L/h and apparent volume 37.5 L at 70 kg, an absorption rate constant of 0.368 /h, and a lag of 0.957 h.

Every parameter is **apparent**, divided by the unmeasured oral bioavailability, so only oral units are offered. The authors tested two compartments, zero-order absorption and sequential zero- and first-order absorption, and kept one compartment with first-order absorption after a lag. The capsule's slow release therefore appears as a slow absorption rate (half-time about 1.9 hours) and a lag of just under an hour. That is why the peak comes about 6 hours after the dose.

### A model fitted in children, used for adults

Tamsulosin is prescribed for men with benign prostatic hyperplasia, and the model was fitted in children. It is used for adults because it is the only population model of tamsulosin read in full, and because it reproduces the adult measurements on the US label, which were never part of its fit. The Flomax label's Table 2 gives, for 0.4 mg once daily after a light breakfast in 23 healthy men aged 18 to 32 at steady state:

| | Label | This model, 70 kg |
|---|---|---|
| Peak (Cmax) | 10.1 ng/mL | 10.25 ng/mL |
| Trough (Cmin) | 4.0 ng/mL | 3.87 ng/mL |
| Time to peak | 6.0 h | 5.96 h |
| Area under the curve over 24 h | 151 ng·h/mL | 175 ng·h/mL |

The area is 16% high. The label's older men (55 to 75 years, 0.8 mg) had 440 ng·h/mL, or 220 per 0.4 mg. The model's 175 lies between the young and the old men. Its elimination half-life of 11.4 hours sits inside the label's apparent 9 to 13 hours in healthy volunteers. The label gives 14 to 15 hours in patients. The paper made the same comparison with the same two adult studies in its Figure 5 and found the children's exposure comparable.

A population model in the patients the drug is for exists: Nikolic and colleagues (*World J Urol* 2024;42:427) studied 65 men with benign prostatic hyperplasia. It could not be read in full, and its abstract's clearance, 0.719 L/h, is a third of this model's and would give about three times the label's exposure. Until it is read, nothing here relies on it.

### Body size

The source's allometry is weight to the power 0.75 on clearance and weight itself on volume, at a 70 kg standard. That is also the scaling this library uses, so with *Adjust weight to fat-free mass* off the published model is reproduced exactly. With it on, the same exponents act on fat-free mass. The absorption rate and the lag are not scaled.

### Alpha-1-acid glycoprotein

Tamsulosin is 94 to 99% bound to plasma proteins, mainly alpha-1-acid glycoprotein (AAG). In this model AAG lowers both the apparent clearance and the apparent volume of **total** drug, as clearance to the power −0.844 and volume to the power −0.663 of AAG over 20 µM. The paper's simulations give three times the exposure at 40 µM as at 10 µM. The Patient Profile has no AAG field, so AAG is held at the paper's reference, 20 µM, and the terms drop out. AAG is higher in older men, in renal and hepatic impairment and after surgery. The label says that in renal and hepatic impairment the total concentration changes with AAG while the unbound (active) concentration does not. The curve is total tamsulosin.

### What the model does not include

- **Age.** The label reports about 40% more exposure at 55 to 75 years than at 20 to 32, from a cross-study comparison. No source fitted an age term, and this source studied children.
- **Food.** The source dosed with food, and the label doses the capsule half an hour after the same meal each day. Taken fasting, the label reports 30% more exposure (AUC) and a 40 to 70% higher peak, which this curve does not show.
- **CYP2D6.** Paroxetine, a strong CYP2D6 inhibitor, raised exposure 1.6-fold (label). The label *expects* a similar rise in CYP2D6 poor metabolisers but did not measure it, and no source quantifies a phenotype on a parameter. So this model does not read the Patient Profile's CYP 2D6 field.
- **Kidney function.** Under 10% of a dose is excreted unchanged (label); there is no renal term.

### Dose basis

The product is tamsulosin **hydrochloride**. The source doses it and measures it, and the label's concentrations are of it. So a dose is entered as labelled, with no conversion, and the curve is tamsulosin hydrochloride.

### Effect site and band

No concentration–effect relation is used, so the model has no effect site. No therapeutic plasma range is established for tamsulosin, so no band is drawn. The drug is dosed by symptoms and blood pressure, not by concentration.

### Where to be careful

The between-subject variability is large: 54% on clearance, 61% on volume and 117% on the absorption rate. A patient's curve can sit well away from the typical one. Tsuda's own visual predictive check under-predicted the median concentration 4 to 12 hours after a dose in the heaviest children (50 to 100 kg, only 16 patients), the group closest to adults.
