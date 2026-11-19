### The model

Overgaard and colleagues' population analysis of liraglutide for weight management (*Clin Pharmacokinet* 2016): 2,931 adults with overweight or obesity in two phase 3 trials of Saxenda (SCALE Obesity and Prediabetes, and SCALE Diabetes), given 1.8 or 3.0 mg once daily. The subjects weighed 106 kg on average, 72% were women and 20% had type 2 diabetes. It is the third model of liraglutide in the library, beside [liraglutide](help:drugs/liraglutide) (Røge 2015) and [liraglutideWatson](help:drugs/liraglutideWatson) (Watson 2010), and the only one with sex: the three are separate drugs and are not mixed.

One compartment, apparent volume 24.6 L (no size covariate in the paper; with **Adjust weight to fat-free mass** ticked it takes the library's fat-free-mass scaling, unticked it is the published value), absorption 0.0806/h (fixed from an earlier study). The apparent clearance is 0.86 L/h for the reference subject, a woman under 70 weighing 100 kg without diabetes.

### Covariates

All on the clearance, as in the paper:

- **Weight:** × (weight/100)<sup>0.68</sup>, at the pharmacokinetic weight with **Adjust weight to fat-free mass** ticked and at total weight with it unticked. With it unticked, exposure is 41% higher at 60 kg and 44% lower at 234 kg than at 100 kg, as published.
- **Sex:** men × 1.31 (exp 0.27), so with **Adjust weight to fat-free mass** unticked a man's exposure is 24% lower than a woman's of the same weight. With it ticked the weight term is evaluated at the pharmacokinetic weight, which is higher in a man than in a woman of the same weight and height, so the difference is larger (about 33% at 70 kg and 170 cm). Carlsson Petri 2015, in adults and adolescents with type 2 diabetes, found men × 1.44.
- **Age:** 70 or over × 0.90 (exp −0.10).

Not applied, for want of a field: type 2 diabetes (× 1.20, so exposure 16% lower), Black race (× 0.92), Hispanic ethnicity (× 1.08). The curve is therefore for a patient without diabetes, as in the Saxenda population. Mild to severe renal impairment and the injection site changed exposure by less than 5%. Adults only.

### What is not plotted

Concentration only, in ng/mL. See [Antidiabetic models and their limits](help:models/antidiabetics).
