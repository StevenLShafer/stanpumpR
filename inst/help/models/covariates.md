The four covariates in the Patient Profile reach the models in fixed units: age in years, weight in kilograms, height in centimetres, and sex. What each model does with them varies from nothing to a great deal. This page collects the size and maturation functions the models share; each drug's page says which ones it uses.

## Three kinds of model

**No covariates.** Alfentanil, sufentanil, midazolam, oliceridine and the adult dexmedetomidine model are fixed sets of parameters for a typical adult. A dose in milligrams produces the same curve in every patient. Dosing per kilogram makes the amount scale with weight, but that is an assumption of the user, not of the model.

**Linear weight scaling.** Etomidate, ketamine, lidocaine, morphine, pethidine, hydromorphone, rocuronium and naloxone take V1 (or every parameter) proportional to weight, with the rate constants fixed. Volumes and clearances then both scale linearly with weight. This is how the original papers reported them.

**Allometric and covariate models.** Methadone (Henthorn and Kharasch) scales only its deep peripheral volume, to weight to the power 1.23. Fentanyl scales volumes linearly and clearances to the 0.75 power of weight. Propofol (Eleveld), remifentanil (Eleveld, and Kim for obesity) and remimazolam (Eleveld) use allometric scaling together with age, sex and maturation terms.

These three kinds describe the models as published, which is what stanpumpR runs with *Adjust weight to fat-free mass* unticked. By default the box is ticked and most of them, the fixed-parameter models included, are rescaled to the patient's fat-free mass, as described at the end of this page and in [Scaling to fat-free mass](help:models/fat-free-mass).

## Allometric scaling

Metabolic rate, and hence clearance, scales across body sizes not with weight but approximately with weight to the three-quarter power, while volumes scale linearly:

```
CL = CL_70 × (weight / 70)^0.75
V  = V_70  × (weight / 70)
```

A 35 kg patient has half the volume of a 70 kg one but 59 per cent of the clearance, so a per-kilogram dose gives a higher concentration and a per-kilogram infusion is relatively too much. This is the main reason children appear to need more drug per kilogram.

## Body mass index, lean body mass and fat-free mass

Several models reason about size through a measure of lean tissue rather than total weight.

**BMI** = weight / (height/100)². Remifentanil switches models at BMI 30.

**Lean body mass (James, 1976).** Used by the Schnider propofol model (present in the code but superseded by Eleveld) and historically by the Minto remifentanil model:

```
men:   LBM = 1.10 weight - 128 (weight/height)²
women: LBM = 1.07 weight - 148 (weight/height)²
```

The James equation has a known flaw: it reaches a maximum and then falls as weight rises, so for very obese patients it gives a lean body mass that decreases with weight. The code comments call it "unfortunate" for this reason, and the models that relied on it have been replaced where better ones exist.

**Fat-free mass (Al-Sallami, 2015).** Used by the Eleveld propofol and remifentanil models. It is well behaved in obesity and includes a maturation term for children:

```
men:   FFM = [0.88 + 0.12 / (1 + (age/13.4)^-12.7)] × 42.92 × weight / (30.93 + BMI)
women: FFM = [1.11 - 0.11 / (1 + (age/7.1)^-1.1)]   × 37.99 × weight / (35.98 + BMI)
```

The Kim remifentanil model for obesity uses the Janmahasatian fat-free mass instead.

## Age and maturation

**Maturation** describes the rise of clearance from birth to adult values. The Eleveld propofol model uses a sigmoid in post-menstrual age, with half-maximal clearance at about 42 weeks post-menstrual age; the app has no gestational-age input, so the model takes post-menstrual age as 40 weeks plus the age entered. Intercompartmental clearance to the slow compartment matures separately.

**Ageing** describes the decline in adults. The Eleveld models reduce V2 and clearance exponentially with age; remifentanil's volumes and clearances decline with age; remimazolam's V3 grows with age. The adult dexmedetomidine, Minto-era and fixed-parameter models have no age term. [The age and propofol scenario](scenario:age-and-propofol) shows how large the effect is.

## Sex

Eleveld's propofol model gives women a higher clearance than men (2.10 against 1.79 L/min at reference size); Eleveld's remifentanil model increases clearance and V2 in women between puberty and the menopause; remimazolam's clearance and V3 are larger in women. Elsewhere sex enters only through lean or fat-free mass.

## CYP2D6, and the disabled covariates

**CYP2D6 phenotype** is now a live covariate: it scales the formation of the active metabolites of codeine, tramadol, hydrocodone and oxycodone. See [Active metabolites](help:models/metabolites). Pregnancy remains in the interface but unused.

## Renal function

**Serum creatinine** is a live, optional covariate: mannitol, vancomycin, gentamicin, cefazolin, sugammadex, gabapentin, pregabalin and oxycodone (whose metabolic clearance falls in renal failure) estimate renal function from it. Left blank, it is an **assumed normal creatinine** for the patient's age and sex, which represents the decline of renal function with age but not renal impairment. In adults the assumed value is 1.0 mg/dL in men and 0.8 in women. In children it is the median of healthy children of the same age and sex: Boer's reference values under one year, about 0.25 mg/dL from the second month (higher in the first weeks, while the mother's creatinine clears); from two years, the values of the European Kidney Function Consortium (Pottel), 0.35 at five, 0.5 at ten, and 0.8 in boys and 0.7 in girls at seventeen; between one and two, a smooth join of the two. At 18 it steps up to the adult value.

Cockcroft-Gault and CKD-EPI were developed in adults, and given a child's own creatinine they overestimate renal function: a healthy five-year-old boy of 20 kg at his normal 0.35 mg/dL has a Cockcroft-Gault clearance of 106 mL/min, about twice the normal GFR for his size. So the models built on those equations read a child's creatinine against the normal for age: divided by it, and multiplied by the adult value, after the age-adjusted creatinine of Björk and colleagues. A child at the normal creatinine for age gets the renal function the equation gives at an adult's normal creatinine and the child's own age and size. At twice the normal for age, the Cockcroft-Gault estimate halves and the CKD-EPI estimate (gentamicin) falls to about 0.43 of it. Pregabalin is the exception: its source estimated children's renal function from their own creatinine, with the Schwartz equation under 13 and Cockcroft-Gault from 13, so pregabalin uses a child's creatinine as it is.

## Fat-free mass as the default scaling

Most of the models above describe a 70 kg adult and scaled, if at all, with total body weight. By default stanpumpR now rescales them to the patient's fat-free mass instead, because clearance tracks lean tissue rather than fat. The switch, and the list of which models it affects, is on its own page: [Scaling to fat-free mass](help:models/fat-free-mass).

## References

James WPT. *Research on Obesity.* London: HMSO, 1976.

Al-Sallami HS, Goulding A, Grant A, Taylor R, Holford N, Duffull SB. Prediction of fat-free mass in children. *Clin Pharmacokinet* 2015;54:1169-1178.

Janmahasatian S, Duffull SB, Ash S, Ward LC, Byrne NM, Green B. Quantification of lean bodyweight. *Clin Pharmacokinet* 2005;44:1051-1065.

Anderson BJ, Holford NHG. Mechanism-based concepts of size and maturity in pharmacokinetics. *Annu Rev Pharmacol Toxicol* 2008;48:303-332.

Boer DP, de Rijke YB, Hop WC, Cransberg K, Dorresteijn EM. Reference values for serum creatinine in children younger than 1 year of age. *Pediatr Nephrol* 2010;25:2107-2113.

Pottel H, Björk J, Courbebaisse M, et al. Development and validation of a modified full age spectrum creatinine-based equation to estimate glomerular filtration rate: a cross-sectional analysis of pooled data. *Ann Intern Med* 2021;174:183-191.

Björk J, Nyman U, Larsson A, Delanaye P, Pottel H. Estimation of the glomerular filtration rate in children and young adults by means of the CKD-EPI equation with age-adjusted creatinine values. *Kidney Int* 2021;99:940-947.
