The first panel in the left sidebar. These covariates are passed to every drug's pharmacokinetic model, so they change the predictions. Which covariates a given model actually uses is recorded on its page under [Drug library](help:drugs/index); some use all four, some none.

## The fields

| Field | Units | Range | Notes |
|---|---|---|---|
| Age | years or months | 0 to 90 | Toggle the unit beside the field. Ages of 90 and above are entered as 90 (see below). |
| Weight | kg or lb | 0.1 to 500 kg | |
| Height | in or cm | 10 to 200 cm | |
| Sex | male or female | | |
| CYP 2D6 | phenotype | | Scales the active metabolites of codeine, tramadol, hydrocodone and oxycodone, and the clearance of aripiprazole, haloperidol and risperidone. |
| CYP 2C19 | phenotype | | Changes the clearance of escitalopram and citalopram. |
| CYP 2C9 | diplotype | | Changes the clearance of meloxicam and the maximal elimination rate of phenytoin. |
| CYP 2B6 | phenotype | | Changes the clearance of methadone. |
| VKORC1 | genotype (-1639G>A) | AA by default | Sets the sensitivity of warfarin, fluindione and acenocoumarol: the A allele lowers the C50, so the INR rises on less drug. AA, the most sensitive genotype, is assumed until a genotype is entered, so that the curve errs toward the larger INR; see **VKORC1 and ancestry** below. |
| CYP4F2 | genotype (rs2108622) | CC by default | Sets warfarin's sensitivity: a C allele lowers the C50. CC, the most sensitive genotype, is assumed until a genotype is entered, so that the curve errs toward the larger INR. |
| Adjust weight to fat-free mass | checkbox | | On by default; scales most models to fat-free mass. |
| Serum creatinine | mg/dL | 0.2 to 15, or blank | Optional. Used by canagliflozin, cefazolin, dapagliflozin, empagliflozin, ertugliflozin, exenatide, gabapentin, gentamicin, lacosamide, levetiracetam, lixisenatide, mannitol, intravenous meloxicam, metformin, oxycodone, pregabalin, sugammadex and vancomycin. Blank means an assumed normal creatinine for the patient's age and sex: 0.9 in men and 0.8 in women, and in a child the normal for age (about 0.25 in infancy, 0.35 at five, 0.5 at ten). See [Renal function](help:models/covariates). |
| Baseline serum osmolality | mOsm/kg | 200 to 400 | The patient's starting value, 280 by default. Read only by [mannitol](help:drugs/mannitol), which is plotted as the serum osmolality it produces on top of this value. |

If Age, Weight or Height is blank or not a number (a lone `.`, say), the plot area says which field needs a number, and the plot returns as soon as it has one. A number outside the range is reported in a message.

Changing a covariate re-simulates every drug at once; there is no Apply step for the patient.

Hovering over the genotype fields or the Serum creatinine field shows a tip naming the drugs that read it; every other drug ignores the field. The tips are built from the models themselves, so a drug added later that reads a field appears in its tip.

**Adjust weight to fat-free mass** scales most of the models to the patient's fat-free mass rather than total body weight; it is on by default. **CYP 2D6**, with the phenotypes the genotyping laboratories report (poor, intermediate, normal, ultrarapid), scales the formation of the active metabolites, and the clearance of three antipsychotics. **CYP 2C19** (poor, intermediate, normal, rapid, ultrarapid) changes the clearance of escitalopram and citalopram. **CYP 2C9**, entered as the diplotype of the common \*1, \*2 and \*3 alleles (\*1/\*1 by default), changes the clearance of [meloxicam](help:drugs/meloxicam) and the maximal elimination rate of [phenytoin](help:drugs/phenytoin). **CYP 2B6** (the same five phenotypes as CYP 2C19) changes the clearance of [methadone](help:drugs/methadone); [the methadone genotype scenario](scenario:methadone-cyp2b6) is a worked example. See [Antidepressant models and their limits](help:models/antidepressants), [Scaling to fat-free mass](help:models/fat-free-mass) and [Active metabolites](help:models/metabolites).

## VKORC1 and ancestry

**VKORC1**'s default is chosen for safety, not for how common the genotype is — in two of the three groups in the table below it is *not* the commonest genotype. The −1639A allele lowers the half-maximal concentration of every vitamin-K antagonist in the library, so the AA patient reaches a given INR on the least drug: ×0.725 on the S-warfarin C50 of the [library warfarin model](help:drugs/warfarin), the lowest EC50 of the three in the [Hamberg companion](help:models/hamberg) (2.20 against 4.61 mg/L for GG), and the sources' own sensitive reference for [fluindione](help:drugs/fluindione) and [acenocoumarol](help:drugs/acenocoumarol), where a G allele raises the C50 ×1.30 and ×2.29. **AA is therefore assumed until a genotype is entered**, by the same rule the CYP4F2 field follows: with the genotype unknown the curve should show the larger INR rise, so a dose read off it errs toward underdosing. An underdose at the start of therapy is corrected by raising the dose; an overdose risks bleeding, which is these drugs' primary toxicity and may need reversal with vitamin K.

So the genotype is worth entering from the patient when it is known — and from their ancestry when it is not, because this allele's frequency runs from a tenth of alleles to nine tenths between populations. In the International Warfarin Pharmacogenetics Consortium cohort of 4886 chronic warfarin users the −1639A allele frequency was 91.0% in 641 Asian patients, 37.8% in 681 White patients and 10.1% in 368 Black patients (Limdi and colleagues, *Blood* 2010;115:3827-3834, Table 3, [DOI 10.1182/blood-2009-12-255992](https://doi.org/10.1182/blood-2009-12-255992); the paper's own group labels). The genotype proportions those frequencies imply under Hardy-Weinberg are:

| Group | −1639A allele | AA | GA | GG |
|---|---|---|---|---|
| Asian | 91.0% | 83% | 16% | 1% |
| White | 37.8% | 14% | 47% | 39% |
| Black | 10.1% | 1% | 18% | 81% |

The AA default is therefore also the commonest genotype in Asian patients — the 264 Chinese patients the library's warfarin model was fitted in were 82.6% AA, 17.0% GA and 0.4% GG, within a point of the expectation above — while in a Black patient **GG** is the genotype to expect, and in a White patient **GA**. Entering the patient's own genotype is always better than either: VKORC1 explains more of the warfarin dose requirement than any other single variant, and the same genotype predicts a similar dose requirement in every group — what differs between populations is how common each genotype is.

## What the models do with them

Internally every model receives age in years, weight in kilograms, height in centimetres and sex. From these the models derive what they need: body mass index, fat-free mass by whichever equation the model's authors used (Al-Sallami's for the Eleveld models, Janmahasatian's for the Kim remifentanil model; Al-Sallami's, too, in place of the James lean body mass the meloxicam model was published with), the James lean body mass for the Schnider and Minto models, which keep it as the pumps do, post-menstrual age for the maturation functions, and allometric size scaling. Unless the fat-free-mass box is unticked, the models that do not carry their own body-size covariate are scaled to the patient's fat-free mass. See [Covariates and body size](help:models/covariates) and [Scaling to fat-free mass](help:models/fat-free-mass) for the equations.

A few models switch between parameter sets on a covariate:

- **Dexmedetomidine** uses an infant model (with cardiopulmonary-bypass events) at age ≤ 1 year and an adult model above it.
- **Remifentanil** uses one model at BMI below 30 and another at BMI of 30 and above.
- **Oxytocin** switches to a rat model if the weight is 1 kg or less; this is a research setting, not a clinical one.

## Age 90 and above

An age of 90 or above is protected health information under the HIPAA Safe Harbor rule, so the field stops at 90 and a note appears if you reach it. For the purposes of these models the difference between 90 and 95 is small.

## Pregnant, and the fields only some drugs read

**Pregnant** (shown for a woman of 12 to 59 years) applies the pharmacokinetic changes of late pregnancy to every drug whose change has been measured, each by its source's figure: among them morphine, remifentanil, methadone, buprenorphine, midazolam, the antiseizure drugs, enoxaparin, metformin and the renally cleared antibiotics. Every other drug is unchanged, and the References panel names the source while the field is changing a drug. A drug's own page says what is known of it in pregnancy; see [Pregnancy](help:models/pregnancy). With the creatinine left blank, the renal models add the 50% rise in filtration of pregnancy; a measured creatinine already carries it. The **Serum creatinine** field replaced the greyed-out Renal Function selector: leave it blank and the renal models assume a normal creatinine, which represents renal decline with age but not renal impairment. **CYP 2D6** is no longer among them: it is now active, scaling the active metabolites described under [Active metabolites](help:models/metabolites).

## Default patient

The app opens with a 50-year-old woman, 60 kg, 66 inches (168 cm). The teaching scenarios each set their own patient.
