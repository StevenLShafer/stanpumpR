### The model

The curve is mycophenolic acid (MPA), the active form of mycophenolate, total (bound and unbound) in plasma. Its parameters come from van Hest and colleagues (*Clin Pharmacokinet* 2005;44:1083-1096), a population analysis of a concentration-controlled trial in 140 adult kidney transplant recipients in Rotterdam, all taking ciclosporin and corticosteroids, sampled on nine occasions from day 3 to day 140 after transplantation (6523 concentrations). Two compartments with first-order absorption after a lag of 13 minutes (absorption rate 4.1 per hour). The central volume and the clearance depend on renal function and plasma albumin, and the clearance on sex and the ciclosporin dose:

- central volume 91 L × (creatinine clearance / 48 mL/min)^−0.62 × (albumin / 30 g/L)^−1.13
- clearance 32.5 L/h × (creatinine clearance / 48)^−0.12 × (albumin / 30)^−1.07 × (ciclosporin / 450 mg/day)^0.31, × 1.11 in a man
- peripheral volume 237 L, intercompartmental clearance 35 L/h

The parameters are apparent oral values: the bioavailability cannot be separated from them. The reference man (70 kg, 35 years, normal creatinine, albumin 3.0 g/dL) has a clearance of 31.6 L/h and a central volume of 57 L.

### Dose: mycophenolate mofetil

Doses are entered as prescribed, in milligrams of mycophenolate mofetil (**mg PO mofetil**, also scheduled twice or three times daily), the dose CellCept and its generics are labelled in. The source fitted its model in milligrams of MPA, converting each dose of the mofetil ester by 0.739, and so does stanpumpR: 1000 mg of mycophenolate mofetil puts 739 mg of MPA into the model. Enteric-coated mycophenolate sodium (Myfortic), labelled in milligrams of MPA, is absorbed later and more slowly, and is not yet offered. Intravenous mycophenolate mofetil is not yet offered either: the source studied oral dosing only.

### Covariates

The **creatinine clearance** is Cockcroft-Gault, from the Patient Profile's serum creatinine; left blank, a normal creatinine for age and sex is assumed (0.9 mg/dL in a man, 0.8 in a woman). It is held within the 4-142 mL/min the source studied. The **serum albumin** is the Patient Profile's, held within the 2.2-5.3 g/dL studied. Left blank, it is the equations' reference, **3.0 g/dL**, which is low: the trial's median rose from 3.4 g/dL on day 3 to 4.0 g/dL on day 140. Enter the patient's albumin. A man clears MPA 11% faster than a woman.

Poor renal function and low albumin both *raise* the total clearance and central volume, because less MPA is bound to albumin and the metabolite MPAG, which accumulates in renal failure, displaces it. The unbound MPA, which is the active drug and which the source did not measure, need not fall with the total. A lower total concentration in such a patient is not by itself a reason to raise the dose.

The **ciclosporin dose** is held at the equations' reference, 450 mg a day, because the Patient Profile has no field for it. Every patient in the source took ciclosporin, which lowers MPA exposure by blocking the biliary excretion of MPAG.

### What the model does not show

- **Enterohepatic recirculation.** MPAG excreted in the bile is converted back to MPA in the gut and reabsorbed, giving a second peak 6 to 12 hours after a dose (CellCept label). The source did not model it, and its curve has no second peak. Bile-acid sequestrants (cholestyramine) cut MPA exposure by interrupting the recirculation, which the curve cannot show.
- **Tacrolimus.** With tacrolimus instead of ciclosporin, today's usual partner, the recirculation is intact and MPA exposure at the same dose is higher than this model draws.
- **Time after transplantation.** Clearance falls over the first months as renal function and albumin recover. The model follows that through the creatinine and the albumin entered, but not otherwise.
- **Missed and late doses.** The curve assumes every dose was taken at the time entered. A patient taking mycophenolate at home for months does not always do so, and adherence in practice is lower than in a monitored trial such as the source's (see *A curve assumes the drug was taken* on [Cautions](help:cautions)). A missed dose gives the patient less exposure than taking it would have. A late dose shifts the curve later in time: concentrations are lower than an on-time dose would give until it is taken and absorbed, and may be higher for some hours afterward. Either way, the patient's own curve may still sit above or below the one drawn, which is the typical patient's. To see the effect of the doses as they were taken, enter them that way.
- **Food.** In kidney transplant patients taking 1.5 g of mycophenolate mofetil twice daily, a meal (27 g of fat, 650 calories) left the MPA exposure (AUC) unchanged and lowered the peak by 40% (CellCept label, section 12.3). The model does not tell fed from fasting doses apart.

### Against the label

The CellCept label (Table 10) gives, for 1 g twice daily less than 40 days after kidney transplantation, an AUC over the dosing interval of 27.3 ± 10.9 mg·h/L and a peak of 8.16 ± 4.50 mcg/mL. A 70 kg man of 50 with a creatinine clearance of 50 mL/min and an albumin of 3.7 g/dL, the trial's day-28 medians, falls within one standard deviation of both. More than three months after transplantation, 1.5 g twice daily gives 65.3 ± 35.4 mg·h/L in the label, against 47.7 in the model at an albumin of 4.0 g/dL. In healthy volunteers, who take no ciclosporin, a single 1 g dose gives 63.9 mg·h/L in the label, against 36.1 in the model at an albumin of 4.5 g/dL. The difference is the ciclosporin and the recirculation, so the model is for the early months after transplantation with ciclosporin, the population it was fitted in.

### No band

MPA is monitored by its exposure, the area under the curve over a 12-hour dosing interval (30-60 mg·h/L with ciclosporin, the target the source cites), not by a concentration range, so no band is drawn. The label states no therapeutic range.

### Pregnancy

Mycophenolate is teratogenic: congenital malformations have been reported in 23-27% of live births after exposure in pregnancy (CellCept label, boxed warning). Women are switched to another immunosuppressant before conceiving. No pharmacokinetic study in pregnancy was found.
