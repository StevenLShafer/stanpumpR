### The model

Magnesium sulfate's parameters come from Du and colleagues (*J Clin Pharmacol* 2019;59:374-385), a population analysis of 623 serum samples from 92 women with preeclampsia at Stanford, every one given 4 g over 20 minutes and then 2 g/h, continued for 24 hours after delivery. Two compartments described the **rise** in serum magnesium above each woman's own baseline: clearance 3.72 L/h, central volume 15.4 L, intercompartmental clearance 3.66 L/h and peripheral volume 17.0 L for an 85 kg woman with a serum creatinine of 0.8 mg/dL, the study's medians.

### What is plotted

The rise in serum magnesium in **mmol/L**, above a baseline that is not modelled, exactly as [intravenous calcium](help:drugs/calcium) is plotted. Add the patient's own magnesium to read a total; the study's median was 0.74 mmol/L, and the paper's simulations add that figure. Doses are grams of **magnesium sulfate heptahydrate**, MgSO4·7H2O, as US injections are labelled (1 g is 4.06 mmol, 8.12 mEq, of magnesium): the drug is named for the salt so that a gram can never be read as elemental magnesium, a tenfold difference.

### Covariates

Weight, with the exponents fixed at 0.75 on the clearances and 1 on the volumes, and serum creatinine on clearance, (creatinine/0.8)^-0.731, from the Patient Profile's **Serum creatinine** field; blank means an assumed normal creatinine for age and sex. With [the scaling switch](help:models/fat-free-mass) on, the weight is the fat-free-mass weight.

### Checked against the paper's own simulations

The model reproduces the paper's simulated exposure, which the fit does not contain: the Zuspan regimen (4 g over 20 minutes, then 1 g/h) gives a 24-hour average total magnesium of 1.63 mmol/L at 85 kg and creatinine 0.8, against the paper's 1.6, and every corner of its table of weight- and creatinine-adjusted maintenance rates comes back to its printed 0.1 g/h.

### Where to be careful

These were **pregnant** women with preeclampsia, whose glomerular filtration is raised and whose volumes are expanded, so any other adult is an extrapolation. Only the intravenous route was studied; no intramuscular unit is offered. No band is drawn because the curve is a rise above an unknown baseline; the paper's putative therapeutic range of 1.5 to 2.5 mmol/L and its 3.5 mmol/L toxicity margin are **total** concentrations, so add the baseline before comparing. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
