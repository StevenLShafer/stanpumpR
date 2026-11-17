### The model

**Intravenous:** the two-compartment model of **Steven L. Shafer**, Douglas G. Ririe, **James C. Eisenach** and colleagues (*Br J Anaesth* 2025;134:1513-1522, [doi](https://doi.org/10.1016/j.bja.2024.12.046)). It was fitted in 35 healthy men and nonpregnant women aged 21 to 68 years, given 13.7 to 16.7 mcg intravenously over 1 to 30 minutes. Venous plasma was measured by a specific liquid chromatography-tandem mass spectrometry (LC/MS) assay. This is the model the paper prints as its Table 1 (Model D):

- every volume and clearance is proportional to weight, normalised to 80 kg (V1 8.2 L, V2 5.9 L, CL2 0.22 L/min);
- clearance, 0.86 L/min at 80 kg and 33 years, falls with age as (age/33)^-0.17.

Its median absolute prediction error was 18%, about that of the remifentanil models.

**Intranasal oxytocin is not offered.** The same paper fits an intranasal model (an investigational product; no intranasal oxytocin is marketed in the United States), with an absolute bioavailability of only 0.66% and a median absolute error of 47%. stanpumpR does not use it.

**Weight of 1 kg or less (rats):** a two-compartment model (V1 0.10 L, V2 0.02 L) from Tanaka and colleagues (*Mol Pharm* 2018;15:1105-1111), a study of nasal oxytocin in rats with an LC/MS assay. It is reached only by entering a weight of 1 kg or less.

### Doses

Doses are entered in USP units, as the marketed injection is labelled: a bolus in U, an infusion in U/hr, U/min or, as labour infusions are written, mU/min. The model is the paper's, in micrograms, so each dose is converted at the paper's own equivalence, 600 units per mg (10 units is 16.7 mcg). Micrograms are still offered, for the doses the paper gives.

### Body size and age

The intravenous model's weight terms are evaluated at the pharmacokinetic weight (fat-free mass relative to the 70 kg reference man) with **Adjust to fat-free mass** on, and at total weight with it off. The paper has no sex term, so at the same weight a woman's volumes and clearances are smaller only with the switch on; the switch off reproduces the paper. Nobody studied had a BMI above 33.6. The age term is held at 18 years below 18, and children are outside the population studied.

### Effect site

The paper estimates no effect site. The time to peak effect of 5 minutes is the library's earlier value, described in the code as a guess.

### Typical concentrations

The shaded band is 0.05 to 0.2 ng/mL, a display band whose source is not recorded. For scale, 10 units (16.7 mcg) over 10 minutes peaks at about 1.3 ng/mL in the reference man. Oxytocin's uterotonic effect is the clinical end point, and the relation between concentration and uterine tone is not modelled.

### Pregnancy

The model comes from nonpregnant adults. Two studies of metabolic clearance found it unchanged at term (Leake 1980, Takeda 1989), so no pregnancy adjustment is applied. The paper itself notes that its results likely do not represent parturients.

### Where to be careful

Oxytocin is in the library to support research on its kinetics, not as a clinical dosing tool. Its concentration-dependent hypotension and the desensitisation of the receptor during prolonged exposure are not represented. Antibody-based (ELISA) assays read about 40% lower than LC/MS in this paper, so published concentrations from such assays are not comparable with these curves.
