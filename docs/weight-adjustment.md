# Weight adjustment in stanpumpR

stanpumpR scales most of its pharmacokinetic models to the patient's **fat-free
mass** rather than to total body weight. This document explains why, how the
scaling is anchored so that it agrees with per-kilogram dosing for a typical
adult, what it does to the predictions for everyone else, which models it
applies to, and how to turn it off. It is also the specification every new
model added to the library must follow.

---

## The short version

- Drug clearance tracks lean tissue, not fat. A 120 kg patient does not clear
  drug 1.7 times as fast as a 70 kg patient.
- stanpumpR therefore computes the patient's fat-free mass (FFM) from weight,
  height, age and sex using the model of Al-Sallami and colleagues (2015), and
  scales each model's **volumes by FFM / FFM<sub>ref</sub>** and its
  **clearances by (FFM / FFM<sub>ref</sub>)<sup>0.75</sup>**.
- FFM<sub>ref</sub> is the fat-free mass of the reference patient, a **70 kg,
  170 cm man**, which is 54.5 kg. His scaling factors are exactly 1, so he
  receives every model's published parameters unchanged, and a package insert's
  per-kilogram dose for him is unchanged too.
- The switch is the checkbox **Adjust weight to fat-free mass** in the Patient
  Profile panel. It is on by default. Turning it off restores the scaling each
  model used before, exactly, except for the few models that have themselves
  changed since (see [What the switch does](#what-the-switch-does)).
- Doses typed in per-kilogram units (mg/kg, mcg/kg/min, ...) are **always**
  converted using total body weight, because that is what the clinician
  actually gives. The switch changes only the pharmacokinetics.

---

## Why total body weight misleads

Almost every pharmacokinetic model in the library was reported for a typical
adult, and most package inserts give doses per kilogram. Both implicitly assume
a patient of ordinary body composition: about 20 percent fat in a man, 25 to 30
percent in a woman. When the patient is heavier than that, the extra weight is
mostly fat. Fat is poorly perfused and metabolically quiet, so it contributes
little to drug clearance and, for most drugs, modestly to the volumes into which
the drug distributes.

Scaling a model linearly with total body weight therefore overpredicts
clearance in obese patients and recommends too much drug. Across many drugs,
fat-free mass has been found to be the body-size descriptor that best predicts
clearance (McLeay et al. 2012), and it is the descriptor Holford and colleagues
recommend for scaling pharmacokinetic parameters.

## Fat-free mass

Fat-free mass is total weight minus all fat. It is estimated from weight,
height and sex using the equations of Janmahasatian and colleagues (2005),
which were developed in adults ranging from 41 to 216 kg:

```
male    FFM = 9270 × WT / (6680 + 216 × BMI)
female  FFM = 9270 × WT / (8780 + 244 × BMI)
```

with WT in kg and BMI in kg/m². Al-Sallami and colleagues (2015) extended these
to children by multiplying by a sex-specific maturation function of age:

```
male    maturation = 0.88 + 0.12 / (1 + (age / 13.4)^-12.7)
female  maturation = 1.11 - 0.11 / (1 + (age / 7.1)^-1.1)
```

In men the maturation term is 1 from about 20 years, so for an adult man the
Al-Sallami FFM is the Janmahasatian FFM, and the reference man's 54.5 kg is
the same by either formula. In women the term approaches 1 only slowly: it is
1.03 at 18 years, 1.016 at 35, 1.012 at 50 and 1.007 at 80, so an adult
woman's Al-Sallami FFM is 1 to 3 percent above her Janmahasatian FFM. In
children it is about 0.88 in boys before puberty and 1.04 to 1.08 in girls
from 3 to 10 years. The model was built on children aged 3 to 29 years plus the
adult data. Below 3 years it is an extrapolation. O'Hanlon and colleagues (2023)
extended it down to premature neonates, but their model needs postmenstrual
age, which stanpumpR does not collect, so stanpumpR uses the 2015 model
throughout and flags the extrapolation here.

This is the same fat-free mass that the Eleveld propofol and remifentanil
models already use internally. The James lean body mass formula that appears in
older models is **not** used for this scaling: it is a parabola in weight
that peaks at a BMI of 43 in men and 36 in women and then falls, giving
nonsense for heavy patients (Bouillon & Shafer, Anesthesiology 1998). Aoyama's
meloxicam model takes the Al-Sallami fat-free mass in its place, with its
published centring value. The Schnider propofol and Minto remifentanil
companions (`R/alternativeModels.R`), istradefylline and tolcapone, whose
sources used James, read it held at its peak (`lbmJamesCapped()`,
`R/lbmJames.R`; Shafer's decision of 2026-10-31), so that past the peak a
heavier patient is never credited with less lean tissue.

## How stanpumpR applies it

### The reference patient

Every scaled model is anchored to a reference patient: **70 kg, 170 cm, 35
years, male**. His fat-free mass by the formula above is **54.5 kg**. This
number is computed from the formula, never typed in, so the reference patient's
factors are exactly 1.

Anchoring matters. If the per-kilogram dose on a label were applied to
fat-free mass directly, the typical 70 kg man would receive 54.5 mg of a drug
labelled 1 mg/kg instead of the 70 mg the label intends: a 22 percent
underdose at the very patient the label was written for. Dividing by the
reference fat-free mass instead of by 70 kg keeps him at 70 mg and scales
everyone else from there.

### Anchoring at the source's cohort

**Steven L. Shafer, 2026-11-18, a standing rule.** A model whose published
parameters are fixed values with no size covariate (none tested, or none
retained) describes the **typical patient of the cohort it was fitted in**,
not the 70 kg reference man. Treating its values as the reference man's gives
a 70 kg patient the parameters of whoever the cohort was: Ghimire 2026's
bupropion cohort, the case that raised the rule, had a median weight of about
100 kg and a BMI of 34. So such a model is anchored at its cohort: with the
switch on, each published value is divided by the size factor of the cohort's
typical patient, so that **a patient like the cohort's receives the published
values**, and the reference man, and everyone else, are scaled from there.

- **The typical patient** is the cohort's printed central weight, height and
  age (median or mean, whichever the source prints). Several groups (dose
  arms, studies, sexes printed apart) are entered as `groups =`, each with its
  own values and numbers, and pooled in the geometric mean below. Means are
  computed from printed individual values where the source gives them
  (atropine, midazolam); a weight implied only by a per-kilogram dose, or a
  meta-analysis of study means, is not a cohort (Shafer, 2026-11-18).
- **A missing height** (Shafer, 2026-11-18: "BSA if printed, else typical
  height"), in this order:
  1. a printed BMI: height = 100 x sqrt(weight / BMI);
  2. a printed body-surface area, solved back through the formula the paper
     names, else Du Bois (height = (BSA / (0.007184 x weight^0.425))^(1 /
     0.725) cm), or Mosteller (height = 3600 BSA^2 / weight), rounded to
     0.1 cm;
  3. otherwise the **sex-typical adult height of the cohort's population**:
     the NCD Risk Factor Collaboration's mean height at 18 for the country,
     sex and birth year (*eLife* 2016;5:e13410, the file
     `NCD_RisC_eLife_2016_height_age18_countries.csv` from ncdrisc.org's
     archive), the birth year being the year of data collection (or, where it
     is not printed, of publication) less the typical age, held within the
     1896-1996 the collaboration estimated, less the 0.2 cm (men) and 0.3 cm
     (women) a cohort loses per decade of age after 18 that the same paper
     reports. A cohort of both sexes is then two groups, each with its sex's
     height. A cohort of several countries has no single population, and is
     left for a decision (opicapone).
- **Sex.** A cohort of both sexes has no single fat-free mass at its median
  weight: a 100 kg man's is about a fifth above a 100 kg woman's. The factor
  is the geometric mean of a man's and a woman's at the typical weight, height
  and age, weighted by the numbers of men and women. For a single-sex cohort
  it is that sex's factor.
- **The code**: `cohortSizeFactors(weight, age, men, women, height =, bmi =, groups =)`
  returns the typical patient's factors, and
  `pkSizeFactors(..., cohort = )` divides them out with the switch on and
  ignores them with it off, so the switch off still gives the published values
  unscaled. A model's header gives Table/page, the values read and the
  resulting divisors; its test pins that the cohort's typical patient (the
  geometric mean of its man and woman) receives the published values.
- **A renal term is not anchored** (Shafer, 2026-11-18: "including weight in
  creatinine clearance already accounts for it. You would wind up double
  correcting clearance"). Where a clearance is a creatinine-clearance or eGFR
  term with weight inside it, that term takes no size factor at all; the
  volumes, the intercompartmental clearances and any non-renal clearance are
  anchored (cefazolin, mannitol, metformin, dabigatranHip, etoposide).
- **A parameter with its own sex term** (an intercept fitted for men, with a
  factor for women) is anchored at the typical patient of the patient's own
  sex, not at the pooled one (`cohortSexSizeFactors()`; etoposideKroghMadsen's
  central volume, fluoxetine's CL/F). Anchored at the pooled patient, or at
  one sex for both, the fat-free-mass difference between the sexes is applied
  on top of the source's own sex factor, which already holds it, and a cohort
  patient of one sex or both misses the published value (Carbon's second
  repository audit, 2026-11-19, A2-F1: fluoxetine's cohort man 14% high, its
  woman 5% low; etoposide's cohort woman's Vc 18% low).
- **A metabolite fitted jointly with its parent** shares the parent's cohort
  (norfluoxetine, desvenlafaxine, dehydroaripiprazole, desethylamiodarone,
  hydroxybupropion).
- **An absorption rate calibrated to an observed peak** on the published
  disposition (Shafer, 2026-11-18: "re-anchor and recalibrate ka") is refitted
  so that the calibration study's own typical patient, whose disposition is
  now the anchored model's scaled to that patient, reproduces the peak
  (ropivacaine against Vainionpaa 1995, bupivacaine against Robison 1991,
  with the UK's heights). The rate fitted on the published disposition is
  kept for the switch off, which gives everyone the published disposition:
  the refitted rate there put the calibration peaks 22% and 14% low (Carbon's
  second repository audit, 2026-11-19, A2-F2). Where the peak came from the disposition's own
  subjects the rate does not move (diazepam IM), and where the calibration
  study prints no typical patient it stays, said in the header (diazepam
  oral, Hogan 2020's weight bands).
- **Where it does not apply**: a parameter with its own size covariate (it
  already says whom its value belongs to); a model published per kilogram, whose
  values were normalised by the source; and a cohort whose demographics the
  source does not print, which keeps the reference anchor and says so in its
  header. A covariate on one parameter does not exempt the others (Shafer,
  2026-11-19, after Carbon's second repository audit, A2-F4):
  doxorubicinJoerger's clearance carries BSA, and its V1, Q and V2, scaled
  from the reference man, gave Joerger's own typical woman a V1 18% low. The
  library's other models with a covariate on some parameters only have not
  been reviewed against this on 2026-11-19 (75 models; the review's
  per-model findings are summarised below): those whose cohort is printed
  were anchored, and the rest wait on a source or a decision.
- **Every parameter is scaled, whatever the source tested** (Steven L.
  Shafer, 2026-11-19, a standing rule, from first principles). A covariate is
  retained when it lowers the objective function by 3.84 (p < 0.05, one degree
  of freedom); one that does not is "not significant", which is often read as
  evidence that the effect does not matter. It is not: if every patient weighs
  about the same, weight cannot be a covariate, and the study says nothing
  about how the pharmacokinetics depend on size. Most sources describe the
  data they collected and give no thought to extrapolation; Eleveld's
  comprehensive models, built to be extrapolated, are among the few
  exceptions. It is a priori nonsensical not to scale volumes with size. So:
  1. a parameter with **no size term in its source** takes the library's
     factors, volumes FFM / FFM<sub>ref</sub> and clearances (intercompartmental
     clearances, a Michaelis-Menten Vmax and a non-renal clearance intercept
     included) that ratio to the 0.75, anchored at the source cohort as above,
     **even where the source tested weight on it and did not retain it**
     ("tested and rejected" no longer exempts a parameter: methadone, whose
     source kept weight on V3 only, and paclitaxelJoerger's V1, held fixed on
     Shafer's decision of 2026-11-06, which this rule supersedes);
  2. a **size term the source estimated** (weight, body-surface area, BMI,
     lean or fat-free mass, or a renal term with weight inside), on any
     parameter, is kept as fitted and takes nothing more;
  3. a **clearance whose covariates are not size** (age, sex, a genotype, a
     dose, an eGFR indexed to 1.73 m<sup>2</sup>, which has no weight inside)
     keeps them and takes the anchored clearance factor as well, at the
     patient's own sex where it carries the source's sex term;
  4. a renal term with weight inside (Cockcroft-Gault, an eGFR de-indexed by
     body-surface area) is still never scaled, and rate constants (ka, ke0,
     Km, a lag) are not scaled.

  Applied on 2026-11-19/20 (each header names the cohort, its table and the
  factor): methadone (every parameter but V3, at Henthorn's 64 subjects, the
  CYP2B6 factors kept), paclitaxelJoerger (V1, at Joerger 2006's cohort),
  paclitaxelCrombag (V1, V3, VMTR and Q; the header's reason, that Crombag
  tested size, was wrong: she evaluated only age), oxaliplatinBastian (V2, at
  Bastian 2003's Table 1, height solved from its BSA; the oxaliplatin part of
  A2-F5), olanzapine (Vp and Q), cefepime (Q and V2, Barreto's own estimates
  held without variability), colistimethate and colistin (the non-renal
  intercepts, V2, CLD1 and V3/fm, at Garonzik 2011's cohort), meropenem (CL
  on top of its indexed eGFR, and CLd, at Trang 2021's Table 1, of which the
  FDA review's fitting population is about nine tenths), suzetrigine and
  M6SUZ (Q3P and Q3M, at the review's median subject as transcribed),
  and the four Miura 2026 models, sildenafilMiura, tadalafilMiura, ambrisentanMiura and bosentan (V/F, which the paper did not explore for covariates, at each cohort's children and adults as four groups, with Japanese heights: the JSPE 2000 growth reference for the children, NCD-RisC for the adults; Shafer, 2026-11-20. Miura's V/F is large: a 70 kg man receives about 900 L of sildenafil, against about 256 L in the FDA's adult model, which the sildenafil group gives adults). On 2026-11-20, their sources now archived and digested (papers/pending-work.csv), every
  parameter without a size term of amitriptyline (Koh 2019's 24 Korean men,
  height printed; the genotype clearances kept, the nortriptyline formed
  following nortriptyline's own scaling), capecitabine and capecitabine5FU
  (Gieschke 2003's three studies, heights solved from each study's BSA),
  sirolimus (Dansirikul 2006, height from BMI; the age term kept), tacrolimus
  (Woillard 2011's two studies, French heights; the card's `[decision]`,
  reference man or cohort, answered by the standing rule), amikacin (Tod
  1998's two groups, French heights; the non-renal intercept, V1, CLD and Vt),
  ganciclovir (Caldés 2009, Spanish heights; V1, Q and V2), quinidine
  (Fattinger 1991, Swiss heights; the non-renal clearance, V1, Q and V2) and
  vancomycinKidneyTransplant (Yu 2026's transplant and non-transplant groups,
  heights printed; V); the renal terms of the last four, with weight inside,
  take none. Amitriptyline's anchoring moved its checks of the nortriptyline
  formed against unfitted data (Gupta 1999's area +7.6%, Hart 2026's trough
  per dose +6.6%, from +4.2% and +3.5%), the formation following
  nortriptyline's clearance while amitriptyline's fell 3.1% at the reference
  man. At the reference anchor, the cohort printing no sex split,
  no weight, no country or a multinational population without heights: the
  eight Tham insulins (CL/F, the human insulins' dose term kept), exenatide
  (Cl on top of its indexed eGFR, Cld, Vp and Vmax), empagliflozin (Q/F) and
  dapagliflozin (Vp/F and Q/F; its CL/F, which carries the source's term for
  women, waits on a decision with apixaban's, since anchoring it at the
  reference man would count sex twice for women).
- **Applied** (2026-11-18, each source's demographics read on the PDF; the
  model's header gives the table, the values and the divisors, and its test
  recomputes them from Al-Sallami's equation written out again,
  `cohortFactorsIndependent()` in `tests/testthat/helpers.R`):
  amantadineER, amiodarone and desethylamiodarone, aripiprazole and
  dehydroaripiprazole, atropine, bendamustine, bupivacaine, bupropion and
  hydroxybupropion, calcium, carvedilol, cefazolin, cenobamate (at the FDA
  review's medians, 77 kg and BSA 1.9 m2, an even sex mix assumed),
  cisatracurium, cladribine, cyclophosphamideJoerger, dabigatranHip,
  dexamethasone, diazepam, diltiazem, droperidol, epirubicin,
  etoposideKroghMadsen, fluoxetine and norfluoxetine, glyburide,
  haloperidolIV, heparin, ifosfamide, lamotrigine, liothyronine, LSD, mannitol, metformin,
  methylprednisolone, metoprolol, midazolam, paroxetine (CL/F only),
  pioglitazone, pramlintideIV, psilocybin, ropivacaine, sufentanil (and so
  carfentanil), vecuronium, venlafaxine and desvenlafaxine, vildagliptin,
  ceftriaxone (its weight from the printed BMI and the typical height),
  vinorelbine, zolpidem and zonisamide; and (2026-11-19) chlorthalidone (Tsai
  2016's Table S1, its sex-term CL/F and V1/F at each sex's subject),
  doxorubicinJoerger (V1, Q and V2) and doxorubicinolJoerger (V/fm), at
  Joerger 2007's 59 women. Also on 2026-11-19, the covariate-free parameters
  of models with a covariate on others (each header names its cohort, table
  and factor): acenocoumarol and fluindione (Verstuyft 2012, one cohort),
  argatroban, buprenorphine, cisplatin, citalopram (CL_R, which carries the
  source's sex term, at each sex's subject), clindamycin, docetaxel,
  enoxaparin, epinephrine, fluorouracilTan, fondaparinux, gentamicin,
  heparinCPB, melphalanNath (V2 and Q; its renal term, normalised to 70 kg,
  is a decision), meloxicam (the oral fit), mirtazapine (V and CL; the BMI
  step is not a size covariate), morphine (all but Q3, which keeps BSA^4; the
  oral F follows the switch, 0.290 off and 0.3031 on, calibrated at
  Atrux-Tallau's subject; codeine's formation follows the reference man's
  clearance, now 78.5 L/h), naloxone, pemetrexed (V2, Q and the non-renal
  clearance intercept), semaglutide, tolcapone, vancomycin (Q) and
  vincristineIgarashi. A cohort without a printed height took NCD-RisC's
  (clindamycin, vancomycin, enoxaparin, epinephrine, argatroban). And
  (2026-11-23) lorazepam, whose disposition became Barr 2001's absolute
  Table 4 (no covariates), at Barr's Table 2 lorazepam group (85 kg, 176 cm,
  74 y; 13 men and 1 woman); it had been a per-kilogram model. And
  (2026-11-25, Gemini's chemotherapy audit of 2026-11-23, F22)
  fluorouracilBriki (V1 and Q at Briki 2026's Table 2, three studies as
  three groups, heights printed; at the same time the model became the
  authors' alternative of supplement Table S3, on Shafer's decision, whose
  Vmax carries its own lean-body-mass term, (LBM/55)^0.484, in place of
  sex: a sex term on a clearance with no weight term stands in for size,
  and the lean-mass exponent reproduces it, where FFM^0.75 would overstate
  it by about a tenth; CL keeps its BSA term and V2 its weight term) and
  cytarabine's adult branch (Krogh-Madsen 2012's four parameters at
  etoposideKroghMadsen's cohort, the PDF confirming one set of 23 patients
  for both drugs; the children's branch keeps Periclou's BSA term).
- **Partial-covariate models not yet anchored** (2026-11-19 review). *A
  decision for Shafer*: apixaban and apixabanVTE (a sex term on CL_NR/F, a
  multinational cohort without heights), dabigatran (Q/F and V3/F fixed from
  Dansirikul's unarchived analysis), dapagliflozinKobuchi (its V/F is Melin's,
  with Melin's own weight term), lisinopril (no sex mix or height printed),
  azilsartan's fixed Q/F and V2/F, gabapentin (under 1%), melphalanNath's
  renal term, and aspirin (k34 x V3 counts weight twice with the switch on).
  *The source is not in the archive or prints no demographics*: aspirin and
  salicylate, pimavanserin, levodopa and levodopaIntestinalGel, pramipexole
  and pramipexoleER, tranexamicAcid, tobramycin, sotalol,
  hydrocodone and haloperidol (amikacin, ganciclovir,
  vancomycinKidneyTransplant and quinidine were anchored on 2026-11-20, and
  fluorouracilBriki on 2026-11-25). *Exempt*: hydrochlorothiazide (a meta-analysis).
- **Not anchored, and why** (2026-11-18). *The source's paper is not in the
  archive* (papers/pending-work.csv): duloxetine and everolimus (sirolimus,
  tacrolimus, amitriptyline, capecitabine, amikacin, ganciclovir, quinidine
  and vancomycinKidneyTransplant were anchored on 2026-11-20, their papers
  archived). *The source, archived on 2026-11-20, prints no weight*, so the
  reference anchor stays until Shafer decides (their pending-work rows stay
  open): nortriptyline (Vos 2025 prints sex and age only, for 48 of the 85
  patients modelled), clozapine (Li 2012: weight and BSA recorded, not
  summarised) and scopolamine (Liem-Moolenaar 2011: 90 men of 18-55 years and
  BMI 18-28.5, ranges only).
  *The source prints no usable demographics*, so the reference anchor stays:
  daunorubicin, doxorubicinLiposomal and doxorubicinLiposomalFree (scaled
  only since 2026-11-24), droperidolIM, risperidone and hydroxyrisperidone,
  nitroglycerin, phenylephrine, vasopressin, prednisolone and prednisone,
  diclofenacEC, alfentanil, oliceridine, mepivacaine, glycopyrrolate,
  neostigmine, clonazepam, apomorphine, sildenafilFDA, benazepril (no height or age), irinotecan (weight only),
  cetagliptin (Lu 2024, since 2026-11-19; one of its pooled studies, Wang 2021, is not yet in the archive).
  *Excluded by the rule on partial data*: sertraline and hydrochlorothiazide
  (meta-analyses of study means), propranolol (a weight implied by
  per-kilogram doses). *Waiting on a decision of Shafer's*, each written as a
  `[decision]` in its header: mirtazapine (a BMI step on clearance),
  doxorubicinPerezBlanco (volumes fixed from Kontny 2013), fosinopril (a
  BSA-indexed creatinine clearance on the whole clearance), gabapentin (V
  re-anchored to the label's 58 L), tobramycin (height on clearance),
  paroxetine's volume (17 L/kg x 60 kg from the literature) and opicapone (a
  multinational cohort).

### Why the reference is a man

The reference patient is an **anchor, not a claim** about which patient is
typical. Its only job is to make the factors exactly 1 for somebody, so that
each model's published parameters are reproduced somewhere and a label's
per-kilogram dose is left alone somewhere. A man was chosen because the
"70 kg adult" of the pharmacokinetic literature and of per-kilogram label
dosing is conventionally male. Nothing in the arithmetic requires it.

One consequence is that a woman is scaled down by construction. At the
reference weight, height and age — 70 kg, 170 cm, 35 years — a woman's
Al-Sallami fat-free mass is 44.9 kg against the man's 54.5 kg, so
`pkSizeFactors()` multiplies her volumes by 0.824 and her clearances by 0.865.
(The worked-examples table below uses age 50, where those factors are 0.820 and
0.862.) Sex therefore enters every scaled model, including models whose own
authors tested a sex term and rejected it.

Those two numbers are the **generic** factors, and a model with its own weight
covariate does not necessarily use them: as
[Models with their own weight or renal covariates](#which-models-are-scaled)
explains, such a model evaluates its published terms at the **pharmacokinetic
weight** instead. Alprazolam is one of them — DeVane's per-kilogram clearance
and volume are both linear in `pkWeight`, so that woman's clearance ratio is
**0.824**, the fat-free-mass ratio itself, not 0.865. Do not carry 0.865 into a
model's own covariate.

Whether that is right was put to the test in October 2026 by Greenblatt and
colleagues' 1993 concentration-response study of alprazolam in panic disorder
(`papers/digests/PMID_8357297.md`), which measured a steady-state slope of
**11.8 ng/mL per mg/day** in 237 patients of whom two thirds were women at a
mean 69.1 kg. `R/drugs_alprazolam.R` predicts **11.90** for its reference man
and **14.46** for a woman at 70 kg, 170 cm and 37 years — so the measurement
agrees with the model's prediction for the wrong sex, and the FFM-driven sex
difference looks too large for this drug. The natural fix would be to normalise
each patient's fat-free mass to a **same-sex** reference, which would keep the
obesity correction while removing the across-sex rescaling.

**Steven L. Shafer decided on 2026-10-30 to keep the single male reference.**
The reasons, in order of weight:

1. **A same-sex reference would give two patients with the same fat-free mass
   different pharmacokinetics**, which is the one thing this scaling exists to
   prevent. A woman of 99.2 kg at 170 cm and 35 years has a fat-free mass of
   54.5 kg — the reference man's, exactly. Today they share a volume factor of
   1.000. Against a female reference she would get 1.214, so 21 percent larger
   volumes than a man with the same lean tissue. The mechanism the scaling rests
   on is that lean tissue clears drug; it does not know the patient's sex.
2. **It would not remove a sex assertion, it would substitute a different
   one.** Against a female reference the 70 kg, 170 cm woman's factors become
   exactly 1.000, so she receives the published parameters unchanged and clears
   drug at the same rate as a man of her weight and height despite 9.6 kg less
   lean tissue. That is a claim of parity at the reference size, and it
   contradicts the premise of fat-free-mass scaling in exactly the comparison
   where the premise is doing the most work. Neither option is neutral: for
   alprazolam the three available positions disagree in direction as well as
   size, DeVane's own fitted sex term putting a woman at 7.5 ng/mL per mg/day,
   parity at 11.9 and the current scaling at 14.5.
3. **The evidence does not reach.** One study, on one drug, bears on this, and
   it does not cite the library or report the heights that fat-free mass is
   computed from; its sampling times were uncontrolled, compliance was
   unverified and some concentrations were 0 ng/mL. Changing the reference
   changes every scaled model in the table below for every female patient. The
   argument for turning the switch off entirely is weaker still, at least for
   this drug: Abernethy and colleagues (1984) found alprazolam's apparent oral
   clearance was **not** higher in obese patients — lower, though not
   significantly, at 66.4 against 88.0 mL/min — and concluded that its dose
   should be adjusted for ideal rather than total body weight. Those data
   therefore **argue against** scaling its clearance on total weight; because
   the difference was not significant they cannot *exclude* a modest change,
   and the stronger claim is the authors' dosing conclusion rather than a result
   of the measurement (`papers/digests/PMID_6143633.md`).

What is not resolved is the tension itself, and the honest statement of it is
that for alprazolam the default appears to over-predict a largely female
cohort's exposure-response slope by about 19 percent, where the switch off
would over-predict it by about 3 percent. **Both figures are a sensitivity
scenario, not a cohort prediction**: Greenblatt reports only a mean weight and
that two thirds were women, with no heights, no sex-specific weights and no
per-patient data, so fat-free mass — which needs height — cannot be computed
for this cohort at all. The two percentages come from assuming heights of
163 cm for the women and 178 cm for the men at age 37 and sweeping the
male-female weight gap from 0 to 20 kg about the reported 69.1 kg mean. The
conclusion is insensitive to that gap and is **not** insensitive to the assumed
heights, which only the authors could supply.

Either the slope is biased low by the study's own limitations, or fat-free-mass
scaling asserts a sex difference larger than this drug's data support. Settling
that needs a study that reports heights. The decision above is about which
default is more defensible across the whole library, not a finding that 14.46
is right.

### The scaling

For a patient with fat-free mass FFM:

| Parameter | Multiplier |
|---|---|
| Every volume (V1, V2, V3) | FFM / 54.5 |
| Every clearance (CL1, CL2, CL3) | (FFM / 54.5)<sup>0.75</sup> |

The 0.75 exponent is the allometric exponent for clearance. A consequence for
models that were published as rate constants with a per-kilogram V1 (ketamine,
etomidate, morphine and others) is that the rate constants are no longer fixed:
k<sub>10</sub> = CL/V now varies as (FFM / 54.5)<sup>-0.25</sup>. That is
intended. The rate constants were only ever fixed because the original study
scaled everything linearly with weight.

### Which models are scaled

The table covers the original anaesthesia library. The drug classes added since (antiseizure,
antidiabetic, anticoagulant, antihypertensive, vasoactive, antipsychotic, stimulant, antiemetic,
local anaesthetic and illicit drugs) say how they scale in each model's header and on its help page.

| Scaled to fat-free mass | Not scaled (own covariates) |
|---|---|
| alfentanil, dexmedetomidine at one year or under (Zuppa 2019), etomidate, fentanyl (Bae 2020, on its own weight exponents at the pharmacokinetic weight), hydromorphone, ketamine, lidocaine, mannitol, calcium (Ansari 2025, parturients; anchored at the source cohort, CALCIUM_COHORT), midazolam, morphine (Lötsch 2002, absolute values; its slow intercompartmental clearance keeps the paper's own covariate, the fourth power of body surface area bounded to 1.5 to 2.2 m², in both switch positions, and the other parameters inherit the scaling), oliceridine, oxycodone, pethidine, remimazolam, vecuronium, cisatracurium, sufentanil, codeine, hydrocodone (its source's body-surface-area and creatinine-clearance covariates at the pharmacokinetic weight, Vp/F and Q/F scaled; formation follows hydromorphone), oxymorphone, tramadol, desmetramadol; the antibiotics cefazolin, clindamycin, cefalexin, ceftriaxone, vancomycin, metronidazole, gentamicin; the steroids hydrocortisone, methylprednisolone, dexamethasone, prednisolone, prednisone; sugammadex, neostigmine, glycopyrrolate; amiodarone, desethylamiodarone, amiodaroneIV; the oral analgesics gabapentin and pregabalin, on their own covariates, and naproxen; and the sedatives diazepam, lorazepam, clonazepam, zolpidem and temazepam, and alprazolam on its own weight covariate; the antidepressants escitalopram, sertraline, paroxetine, duloxetine, mirtazapine (with its source's BMI step on clearance), bupropion and hydroxybupropion (anchored at Ghimire 2026's cohort, 2026-11-18), fluoxetine, norfluoxetine, venlafaxine and desvenlafaxine, and citalopram on its own weight covariate for S-citalopram clearance; methadone (Henthorn 2025) on its own weight covariate on V3, its other parameters scaled to fat-free mass although the source tested weight on them and did not retain it (since 2026-11-19, *Every parameter is scaled, whatever the source tested*) | **propofol** (Eleveld), **remifentanil** (Eleveld, Kim) and **dexmedetomidine** above one year (Morse 2020: clearances on fat-free mass, volumes on normal fat mass) already contain the Al-Sallami or Janmahasatian fat-free mass as a covariate and are used as published. **oxytocin** (Shafer 2025) has its own weight terms, every volume and clearance proportional to weight about 80 kg, evaluated at the pharmacokinetic weight with the switch on and at total weight with it off. **rocuronium** (Kleijn 2011, since 2026-11-23) likewise: volumes proportional to weight and clearances to its 0.75 power about 70 kg, at the pharmacokinetic weight with the switch on and total weight with it off. **sugammadex**, from the same fit, is evaluated at the same weights but scales differently: its elimination clearance has a linear weight term about 74.5 kg (capped at 200 kg), its central volume carries a linear weight term as well as proportionality, and only its intercompartmental clearance is on the 0.75 power. **naloxone** (Dowling 2008) has its own lean-body-weight covariate on clearance. Dowling used the Janmahasatian lean body weight; stanpumpR uses the Al-Sallami fat-free mass, which is the same in adult men, the population Dowling studied, and 1 to 3 percent higher in adult women. Its other parameters inherit the scaling. **acetaminophen** (Morse 2022) has its own normal-fat-mass covariate on clearance (fat-free mass plus 0.816 of fat mass); its volumes and intercompartmental clearance inherit the scaling. **ibuprofen** (Morse 2022) has its own normal-fat-mass covariates on clearance (0.863 of fat mass) and on both volumes (0.718); its intercompartmental clearance inherits the scaling. |

**Models with their own weight or renal covariates.** Several antibiotic and
reversal-agent models write a body-weight term into some parameters
(vancomycin's volumes, gentamicin's central volume, every sugammadex and rocuronium parameter,
pregabalin's clearance and volume, alprazolam's clearance and volume,
methadone's deep peripheral volume, citalopram's S-enantiomer clearance)
or carry weight into a renal covariate through Cockcroft-Gault or body surface
area. With the switch on, those terms are evaluated at the **pharmacokinetic
weight**, 70 kg × FFM / FFM<sub>ref</sub>, so that the reference man receives the
published values and everyone else is scaled on lean rather than total weight;
parameters the source left without a size term take the library factors above.
With the switch off, total body weight enters every published term as written
and the size-free parameters are fixed. `R/renalFunction.R` holds the renal
estimators, which run at the patient's serum creatinine or, when none is
entered, an assumed normal one.

Each scaled model's source file (`R/drugs_<name>.R`) carries a comment stating
what it did before and what it does now.

### What the switch does

The checkbox **Adjust weight to fat-free mass** in the Patient Profile panel is
on by default. With it off, each model uses its published scaling, which for
every model whose parameters have not changed since is exactly the scaling it
used before fat-free mass was introduced (the exceptions are listed below the
table):

| Model type | Behaviour with the switch off |
|---|---|
| Fixed published parameters (alfentanil, sufentanil, midazolam, neostigmine, vecuronium, cisatracurium, oliceridine, oxycodone, mannitol, calcium, ceftriaxone, methylprednisolone, dexamethasone, prednisolone, prednisone, glycopyrrolate, amiodarone, desethylamiodarone, zolpidem, clonazepam, diazepam, escitalopram, sertraline, paroxetine, duloxetine, mirtazapine, bupropion, hydroxybupropion, fluoxetine, norfluoxetine, venlafaxine, desvenlafaxine) | no scaling at all |
| V1 per kilogram with fixed rate constants (ketamine, etomidate, hydromorphone, pethidine, lidocaine, amiodaroneIV, lorazepam, temazepam) | volumes and clearances both × weight / 70 |
| Morphine (Lötsch 2002, Model A) | the published absolute values; Q3 = 19.5 L/h × (BSA / 1.83)<sup>4</sup>, the paper's own covariate, at the body surface area bounded to 1.5 to 2.2 m², in both switch positions |
| Allometric on total weight (remimazolam, infant dexmedetomidine, cefalexin, hydrocortisone, clindamycin with its published 0.497 exponent, naproxen about its source's median child) | volumes × weight / 70, clearances × (weight / 70)<sup>0.75</sup> (or the published exponent) |
| Metronidazole (da Silva Neto 2021) | volume 0.556 L/kg × the patient's adjusted body weight on Devine ideal weight (total body weight under 18 years, or where the ideal weight is not positive), clearance × (weight / 70)<sup>0.75</sup> on total weight, as published |
| Own weight or renal covariates (vancomycin, gentamicin, sugammadex, rocuronium, cefazolin, naloxone, gabapentin, pregabalin, acetaminophen, ibuprofen, alprazolam, methadone, citalopram, hydrocodone, fentanyl: Bae 2020's volumes × (weight / 70)<sup>1.23</sup>, clearances × (weight / 70)<sup>0.313</sup>) | the published equations on total body weight; size-free parameters fixed |

Propofol, remifentanil, dexmedetomidine above one year (Morse 2020, since 2026-11-17), tiagabine (Ingwersen 2000, whose clearance and volume are powers of height, its own size covariate), tirzepatide and warfarin (each with its own size covariates) do not respond to the switch. (The Tham insulins do respond, but clamp the weight each switch position selects to the source's studied range, 52.6-86 kg for the lispro insulins, so the two positions give the same result only when both selected weights clamp to the same end of the range.) Nor does meloxicam's intravenous (ANJESO) system, which keeps its own published weight covariate on total weight in both positions; its oral system does respond.

The switch exists for three reasons: to show, live, how far per-kilogram dosing
departs from fat-free-mass dosing in an obese or female patient; to regenerate
simulations and figures made before the change; and to reproduce a source
study exactly where it scaled on total weight. Simulation links saved before
the switch existed restore with it off, so their output is unchanged, except
for a drug whose model has itself changed since the link was saved: the switch
selects the current model's switch-off behaviour, which is neither the old
model nor, always, the old scaling. Known cases, all from October 2026:
hydrocodone, whose source's covariates were added and apply in both switch
positions; lidocaine, moved from Schnider 1996's interim to its final
parameter set (#610); neostigmine, moved from Calvey 1979's single patient to
Heier 2002's population model, whose published values are fixed, so with the
switch off it no longer scales with weight / 70; ketamine, refitted to Domino
1984's concentrations (#602); sufentanil, whose volumes and clearances became
Gepts 1995's printed values (under 0.5%); adult dexmedetomidine, whose time to
peak effect became 14.9 min (and whose model became Morse 2020's, which the
switch does not change, on 2026-11-17); and intranasal hydromorphone, whose
bioavailability became 0.37 (#611). A simulation of any of these saved before
the change does not reproduce with the switch off.

### Per-kilogram doses

A dose entered as 1 mg/kg for a 120 kg patient is converted to 120 mg. The
switch does not change that. The simulation then shows what 120 mg does to a
patient whose fat-free mass is 71 kg, which is the point: the concentrations
come out higher than the same per-kilogram dose produces in the reference man.

---

## Worked examples

All patients are 170 cm and 50 years old.

| Patient | FFM | Volume factor | Clearance factor | Total-weight factor |
|---|---|---|---|---|
| 70 kg man | 54.5 kg | 1.000 | 1.000 | 1.000 |
| 70 kg woman | 44.7 kg | 0.820 | 0.862 | 1.000 |
| 120 kg man | 71.1 kg | 1.305 | 1.221 | 1.714 |

The woman has the same weight as the reference man but 18 percent less
fat-free mass, so her volumes are 18 percent smaller and her clearances 14
percent lower. Per-kilogram dosing treats them identically. The 120 kg man has
71 percent more weight but only 31 percent more fat-free mass. Per-kilogram
dosing would scale his loading dose by 1.71; fat-free mass says 1.30.

### Choosing a dose from a per-kilogram label

The simulator applies the scaling to the pharmacokinetics, not to the dose you
type. If you want to use the scaling to choose a dose, think of it as a
**dosing weight**: the weight you would plug into the label's per-kilogram dose
to get the fat-free-mass-consistent amount.

- A **bolus** is sized by volume, so the dosing weight is 70 kg × (FFM / 54.5).
- An **infusion rate** is sized by clearance, so the dosing weight is
  70 kg × (FFM / 54.5)<sup>0.75</sup>.

| Patient | Dosing weight for a bolus | Dosing weight for an infusion |
|---|---|---|
| 70 kg man | 70 kg | 70 kg |
| 70 kg woman | 57 kg | 60 kg |
| 120 kg man | 91 kg | 85 kg |

For a drug labelled 1 mg/kg, the 120 kg man's bolus is 91 mg, not 120 mg.

---

## Limitations

- **Below 3 years** the Al-Sallami maturation function is extrapolated. The
  infant dexmedetomidine model is affected. The extrapolation is smooth and the
  factors are close to the weight / 70 values the model used before, but they
  are not validated.
- **Pregnancy.** The formula has not been validated in parturients, and
  pregnancy weight is mostly fat-free. For any drug in a pregnant patient,
  consider turning the switch off.
- **Peripheral volumes of lipophilic drugs.** The scaling applies the same
  fat-free-mass ratio to every volume. The deep peripheral volume of a lipophilic
  drug such as fentanyl tracks fat mass and is larger, not smaller, in an obese
  patient. Fat-free-mass scaling therefore underestimates V3 for such drugs in
  obesity. Clearance, which governs the maintenance rate and the eventual
  decline, is the better-supported part of the scaling.
- **Extreme covariates.** The Janmahasatian equations were developed between 41
  and 216 kg and BMI 17 to 70. Outside that range they are extrapolated.
- **Sex.** Because the reference patient is a man, sex enters every scaled
  model, including models whose own authors tested a sex term and rejected it.
  That is deliberate and the alternatives are worse, but for at least one drug
  the resulting sex difference looks too large against the only data that bear
  on it. See [Why the reference is a man](#why-the-reference-is-a-man).

---

## Requirement for new models

Every model added to the library must do one of two things:

1. **Include its own body-size covariate**, as the Eleveld models do. Document
   it in the source file and in `docs/adding-a-drug.md`'s table, and have the
   function accept and ignore `adjustToFFM`.
2. **Inherit this scaling.** Express the published parameters for the 70 kg
   reference, call `pkSizeFactors(weight, height, age, sex, adjustToFFM, ...)`,
   multiply volumes by `$volume` and clearances by `$clearance`, and pass the
   `legacyVolume` / `legacyClearance` arguments that reproduce the published
   scaling when the switch is off. Fixed published values with no size
   covariate belong to the source's cohort, not the reference man: pass
   `cohort = cohortSizeFactors(...)` from the cohort's printed demographics
   ([Anchoring at the source's cohort](#anchoring-at-the-sources-cohort)).

Either way the function signature is
`<name>(weight, height, age, sex, adjustToFFM = TRUE)` and the unit test pins
both switch positions. See `docs/adding-a-drug.md` for the procedure.

---

## References

- Al-Sallami HS, Goulding A, Grant A, Taylor R, Holford N, Duffull SB.
  Prediction of fat-free mass in children. *Clin Pharmacokinet* 2015;54:1169-78.
  [PMID 25940825](https://pubmed.ncbi.nlm.nih.gov/25940825/)
- Janmahasatian S, Duffull SB, Ash S, Ward LC, Byrne NM, Green B. Quantification
  of lean bodyweight. *Clin Pharmacokinet* 2005;44:1051-65.
  [PMID 16176118](https://pubmed.ncbi.nlm.nih.gov/16176118/)
- O'Hanlon CJ, Holford N, Sumpter A, Al-Sallami HS. Consistent methods for
  fat-free mass, creatinine clearance, and glomerular filtration rate to
  describe renal function from neonates to adults. *CPT Pharmacometrics Syst
  Pharmacol* 2023;12:401-12, with corrigendum 2024;13:181-2.
  [PMID 36691877](https://pubmed.ncbi.nlm.nih.gov/36691877/)
- McLeay SC, Morrish GA, Kirkpatrick CM, Green B. The relationship between drug
  clearance and body size: systematic review and meta-analysis of the literature
  published from 2000 to 2007. *Clin Pharmacokinet* 2012;51:319-30.
  [PMID 22439649](https://pubmed.ncbi.nlm.nih.gov/22439649/)
- Holford NHG, Anderson BJ. Allometric size: the scientific theory and extension
  to normal fat mass. *Eur J Pharm Sci* 2017;109S:S59-64.
  [PMID 28552478](https://pubmed.ncbi.nlm.nih.gov/28552478/)
- Greenblatt DJ, Harmatz JS, Shader RI. Plasma alprazolam concentrations:
  relation to efficacy and side effects in the treatment of panic disorder.
  *Arch Gen Psychiatry* 1993;50:715-22.
  [PMID 8357297](https://pubmed.ncbi.nlm.nih.gov/8357297/) — the study behind
  [Why the reference is a man](#why-the-reference-is-a-man); digested in
  `papers/digests/PMID_8357297.md`.
- Abernethy DR, Greenblatt DJ, Divoll M, Smith RB, Shader RI. The influence of
  obesity on the pharmacokinetics of oral alprazolam and triazolam.
  *Clin Pharmacokinet* 1984;9:177-83.
  [PMID 6143633](https://pubmed.ncbi.nlm.nih.gov/6143633/) — in 12 obese
  patients pair-matched with 12 controls, alprazolam's apparent oral clearance
  was **lower, though not significantly** (66 against 88 mL/min), and the
  authors conclude that dosage should be "adjusted for ideal rather than total
  bodyweight". Its apparent volume, however, was much **larger** (114 against
  73 L), which lean-mass scaling under-predicts — the limitation noted above
  for lipophilic drugs' peripheral volumes. The clearance half is what bears on
  the reference question.
