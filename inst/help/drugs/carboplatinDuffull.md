### The model

Carboplatin comes from Duffull and colleagues (*Cancer Chemother Pharmacol* 1997;39:317-326), who measured **intact, unbound carboplatin** by HPLC in plasma ultrafiltrate in twelve women with ovarian cancer, aged 49 to 68, after 30-minute infusions. That species is the one the Calvert formula's target AUC refers to. Two compartments describe it, scaled by lean body weight (LBW) and renal function:

- clearance (L/h) = 0.15 × LBW^0.75 + 0.78 × creatinine clearance (in L/h);
- central volume 0.26 × LBW and steady-state volume 0.38 × LBW (so the peripheral volume is 0.12 × LBW);
- intercompartmental clearance 4.4 × (LBW/56)^0.75 L/h.

LBW is Pesola's, computed from height: 0.9 × (height − 150) + 45 kg, plus 5 kg for men. The creatinine clearance is Cockcroft-Gault, **adjusted** as the paper did: a serum creatinine below 60 µmol/L (0.68 mg/dL) is set to 60, and the weight in the equation is the lower of LBW and the actual weight. The creatinine is the Patient Profile's, or an assumed normal value for the age and sex when it is left blank. For the average patient of the study (LBW 56 kg, adjusted creatinine clearance 74 mL/min) the model gives a clearance of 6.5 L/h, a central volume of 14.6 L and a steady-state volume of 21.3 L, against the paper's 6.8, 14.8 and 21.6.

The paper contradicts itself on the intercompartmental clearance. Its equation prints 0.15 × LBW^0.75, the same coefficient as the non-renal clearance, which can't produce the 4.4 L/h mean (range 3.9 to 5.0) of its own Table 2. The model keeps the equation's form at Table 2's level. This shapes only the first hours after a dose: the AUC depends on clearance alone.

### What is plotted

Unbound plasma carboplatin in mcg/mL (mg/L). Doses are entered in mg or as an infusion rate. The dosing target is an **AUC**, by the Calvert formula dose = AUC × (GFR + 25), typically 4 to 7 mg/mL·min, which is an exposure rather than a concentration band. It is not drawn. The model's own clearance is its estimate of GFR + 25 in this sense, so a Calvert dose gives about the target AUC only where the two agree. There is no effect site.

### Where to be careful

- **Twelve women, 49 to 68.** Men differ only through Pesola's +5 kg and Cockcroft-Gault's sex term. Every other patient is an extrapolation.
- **Renal impairment.** The study covered adjusted creatinine clearances of 35 to 98 mL/min; below that the renal term is extrapolated. Carboplatin is cleared mainly by filtration, so renal function dominates its exposure, and a measured GFR (⁵¹Cr-EDTA, iohexol) is better than any estimate from creatinine.
- **Children.** Pesola's formula is an adult one, so height is held at 152 cm, the study's shortest, and a shorter patient's size is the lower of that LBW and the weight. That is an extrapolation guard, not a paediatric model.
- **Not a monitoring tool.** The curve is a population prediction for the covariates entered, not a Bayesian estimate from measured levels. The paper's own purpose, adapting the dose to measured concentrations, is not what is plotted.

Every parameter was read in the paper's PDF. The other carboplatin models considered (Ekhart 2006, Chatelut 1995, Schmitt 2009) are on the [chemotherapy](help:models/chemotherapy) page.
