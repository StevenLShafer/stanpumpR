### The model

Baron and colleagues' analysis of 2761 adults with type 2 diabetes (*Diabetes Ther* 2016): two compartments with first-order absorption after a lag fixed at 0.5 h; apparent clearance 10.6 L/h, central volume 3.14 L, intercompartmental clearance 6.34 L/h, peripheral volume 70.6 L, absorption 0.196/h (the paper prints its unit as L/h, a typographical error). The central volume is poorly identified and data from the first hour were excluded, so the early curve is uncertain; the terminal half-life is about 12 hours.

### Covariates

The supplement's Table S1 gives the full model. Each continuous covariate enters as a power of its value over the reference patient's: a 50-year-old man, BMI 25, eGFR 100 mL/min/1.73 m² by MDRD. Four are applied, the ones the Patient Profile carries:

- **Renal function:** clearance × (eGFR/100)<sup>0.333</sup>. The eGFR is the four-variable MDRD from the entered serum creatinine (a child's on the adult scale) or an [assumed normal](help:models/covariates) one. The paper reports exposure 18.5%, 49.2% and 88.1% higher at an eGFR of 60, 30 and 15, and the model reproduces all three within 0.2%.
- **Age:** clearance × (age/50)<sup>−0.241</sup>, with smaller terms on the volumes and absorption. Age is held at 18 and above.
- **BMI:** clearance × (BMI/25)<sup>0.320</sup>, central volume × (BMI/25)<sup>1.04</sup>, peripheral volume × (BMI/25)<sup>0.672</sup>. BMI is held within 20.8 to 42.4 kg/m². With **Adjust weight to fat-free mass** ticked it is calculated from the pharmacokinetic weight.
- **Sex:** in women, clearance × 0.886, central volume × 1.25, peripheral volume × 0.831 and absorption × 1.17.

The table writes the central volume's BMI term over 20, the equation below it over 25; the equation is followed. Smoking, race, total protein and the liver enzymes have no field and stay at the reference values; the paper finds each changes exposure by less than 25%.

### What is not plotted

Concentration only, in ng/mL. The fasting glucose and HbA1c models and urinary glucose excretion are endpoints of their own and are not drawn. See [Antidiabetic models and their limits](help:models/antidiabetics).
