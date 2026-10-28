### The model

Melin and colleagues' population analysis of 2744 people given dapagliflozin (*Br J Clin Pharmacol* 2024). The data came from 1497 patients with heart failure with reduced ejection fraction (the DAPA-HF trial, one trough each; 1869 before exclusions), 1193 adults with type 2 diabetes from four studies with serial sampling (1269 before exclusions), 24 adolescents with type 2 diabetes, and 30 healthy volunteers, at doses of 1 to 100 mg. It refits the sponsor's model, the one behind the drug's approval, and its NONMEM code is in the supplement.

- **Disposition:** two compartments. Apparent clearance 23.0 L/h, central volume 73.9 L, intercompartmental clearance 8.92 L/h, peripheral volume 143 L.
- **Absorption:** first order at 2.39/h after a lag of 0.248 hours.

Heart failure did not change clearance (a 4% difference was tested and dropped).

### Covariates

- **Renal function:** clearance × (eGFR/83)<sup>0.584</sup>. The eGFR is CKD-EPI 2009 in adults, from the entered serum creatinine or an [assumed normal](help:models/covariates) one. Under 18 it is the bedside Schwartz equation, 0.413 × height / creatinine, as in the paper. The data covered eGFRs of 27 to 154.
- **Sex:** clearance × 0.840 in women.
- **Weight:** central volume × (weight/89)<sup>0.570</sup>. With **Adjust weight to fat-free mass** ticked, the pharmacokinetic weight is used; the data covered 31 to 198 kg.

At 10 mg once daily, the reference man (89 kg, eGFR 83) has a steady-state exposure of 435 ng·h/mL over a day and a peak near 96 ng/mL about an hour after a dose.

### The other dapagliflozin model

[dapagliflozin (Kobuchi)](help:drugs/dapagliflozinKobuchi) fixed its volume and absorption rate to this paper's values and fitted only the clearance, to trough concentrations in 85 Japanese outpatients. Its clearance, 9.6 L/h at 77 kg, is less than half of this model's.

### What is not plotted

Concentration only, in ng/mL. See [Antidiabetic models and their limits](help:models/antidiabetics).
