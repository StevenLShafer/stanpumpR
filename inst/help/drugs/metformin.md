### The model

Chae and colleagues' analysis of 42 healthy adults given one 500 mg immediate-release tablet (*Br J Clin Pharmacol* 2012): one compartment, apparent clearance 52.6 L/h times creatinine clearance over 106.5 mL/min to the power 0.782, apparent volume 113 L, absorption 0.41/h, no lag. (Table 2 prints the volume's relative standard error as 57%, but the text and the bootstrap interval give about 4%.) The paper does not say how creatinine clearance was obtained; here it is Cockcroft-Gault, the usual method, from the entered serum creatinine (a child's on the adult scale) or an [assumed normal](help:models/covariates) one. Metformin is cleared by the kidney unchanged, so a lower creatinine clearance raises the curve. The subjects were young (21 to 31 years) with creatinine clearances of 90 to 123 mL/min, so the curve in renal impairment extrapolates the fitted power term.

### Routes

Immediate release by mouth (`mg PO`, once, twice or three times daily). The extended-release tablets have their own absorption, which no complete published model in hand describes, and are not offered.

### What is not plotted

Concentration only, in mcg/mL. The same paper's model of the single-dose glucose response in healthy adults (a maximum of 19.8 with a potency of 3.68 mcg/mL, through a transit delay of half an hour) is an endpoint of its own, not a chronic effect, and is not drawn. See [Antidiabetic models and their limits](help:models/antidiabetics).

### Pregnancy

With **Pregnant** ticked, the apparent clearance is multiplied by 1.27 and the apparent volume by 1.33, the differences between pregnant women with gestational diabetes and nonpregnant women with type 2 diabetes on 500 mg (Liao, *Drug Metab Dispos* 2020); metformin's renal clearance in late pregnancy was 1.31 times its postpartum value in an earlier study (Eyal 2010). The creatinine clearance in the model's renal term is not also raised, which would count the rise in filtration twice. The factors are meant for a blank **Serum creatinine**: a pregnant woman's measured creatinine is low and raises the clearance by itself, so with one entered the two together overstate it. See [Pregnancy](help:models/pregnancy).
