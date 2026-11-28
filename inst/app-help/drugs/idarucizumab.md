### The model

Idarucizumab (Praxbind) is a humanised antibody fragment that binds dabigatran and reverses its anticoagulant effect. Its model is **derived by stanpumpR**, not published as a model. Glund and colleagues (*Clin Pharmacokinet* 2017) gave idarucizumab as a 5-minute infusion to 46 adults aged 45 to 80 years in seven groups: middle-aged (2.5 g and 5 g), elderly (1 g and 5 g), mild renal impairment (Cockcroft-Gault creatinine clearance 60 to 90 mL/min; 1 g and 5 g) and moderate renal impairment (30 to 60 mL/min; two 2.5 g doses an hour apart). The paper is non-compartmental: for each group it gives the geometric means of clearance, steady-state volume and the initial and terminal half-lives, and the mean creatinine clearance. It prints no model and no covariate equation.

Each group's four numbers define a two-compartment model exactly. The seven groups' distribution parameters show no trend with dose, age or renal function, so their averages (weighted by group size) are used:

- central volume V1 3.36 L, peripheral volume V2 3.69 L, intercompartmental clearance Q 5.58 mL/min;
- clearance **4.07 + 0.391 × creatinine clearance** mL/min (Cockcroft-Gault, mL/min).

The clearance line is **stanpumpR's own least-squares fit** through the seven groups' mean clearances and creatinine clearances, not an equation the paper prints. The groups' mean creatinine clearances ran from 59 to 110 mL/min; below that the line is an extrapolation. Glund and colleagues' 2018 study of its elimination explains the shape: idarucizumab is filtered by the glomerulus and then taken up and broken down in the proximal tubule, so its clearance follows filtration. At higher doses the tubular uptake saturates and more leaves unchanged in the urine, which balances the catabolism lost, so the clearance does not depend on the dose.

Checks against the paper: the line gives 47.2 mL/min at the 5 g middle-aged group's 110 mL/min (measured 47.1; the label 47.0); the groups' initial half-lives come out 0.73 to 1.16 h (measured 0.78 to 1.16 h); and a 5 g infusion over 5 minutes in that group's typical man peaks at about 26,400 nmol/L (1260 mcg/mL), against the measured 25,000, falling to about 2% of the peak by 6 hours, as the paper describes.

### Checked against patients

Glund and colleagues (*J Thromb Haemost* 2019) measured idarucizumab in 347 patients of the RE-VERSE AD study (mean age 77) given 5 g to reverse dabigatran for bleeding or urgent surgery. That analysis is also non-compartmental, and the population model it mentions is unpublished. Its exposures over the first 24 hours, by creatinine clearance, are the check on the line, which was fitted to volunteers:

| Renal function (creatinine clearance) | Measured AUC 0-24 h (nmol·h/L) | Model | Model / measured |
|---|---|---|---|
| Normal (80 mL/min or more) | 47,100 | 39,900 | 0.85 |
| Mild impairment (50 to 80) | 65,000 | 57,100 | 0.88 |
| Moderate impairment (30 to 50) | 89,700 | 83,200 | 0.93 |
| Severe impairment (below 30) | 116,000 | 125,400 | 1.08 |

The model is evaluated at creatinine clearances of 99, 65, 40 and 21 mL/min. The label gives the patients' medians as "21 to 99"; the middle two are assumed. The model is within 16% of the patients at every level of renal function. It underpredicts exposure by 15% with normal kidneys, where these older, ill patients cleared less than the volunteers (about 36 against 43 mL/min), and overpredicts it by 8% in severe impairment, below the range the volunteers covered. The label's rises in exposure (38%, 90% and 146% over normal) are the same numbers as ratios; against the patients' own normal group they look larger for the model (43%, 108% and 214%) mainly because that group cleared less. Steven L. Shafer decided (2026-11-28) to keep the line rather than refit it to these patients.

### Covariates

- **Renal function**: the Patient Profile's serum creatinine in the Cockcroft-Gault equation, or the library's assumed normal creatinine when blank.
- **Size**: Glund tested none, and the label says weight had no clinically important effect in its population analysis. The volumes, Q and the 4.07 mL/min intercept are scaled by fat-free mass from Glund's typical subject (a cohort of 27 men and 19 women, 50 to 72 years, 66 to 81 kg by group); the renal slope is not, its Cockcroft-Gault clearance already carrying weight.
- **Age**: only adults 45 to 80 years were studied, and the label has no data in children. A child is drawn with the adult model.
- **Pregnancy**: nothing measured (see the pregnancy section below); the creatinine clearance takes the rise in filtration of late pregnancy, an assumption.

### Product and doses

Praxbind, 5 g as two 2.5 g vials, given as two consecutive infusions or by bolus injection (`g`, `mg`, `mg/kg`, or `g/hr` and `mg/min` for an infusion; Glund's 5 g over 5 minutes is `60 g/hr` at 0 and `0 g/hr` at 5 minutes). The label says data on a second 5 g dose are limited.

### What is not plotted

Concentration only, in mcg/mL (Glund reports nmol/L; 1 mcg/mL is 20.9 nmol/L at 47,766 daltons). Idarucizumab's effect is the dabigatran it binds, and the model does not carry dabigatran, so it draws no band and no effect site. Dabigatran left outside the plasma can move back into it after idarucizumab has fallen, re-elevating the clotting times in some patients (label 5.2); this model does not show that.
