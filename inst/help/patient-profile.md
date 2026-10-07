The first panel in the left sidebar. These covariates are passed to every drug's pharmacokinetic model, so they change the predictions. Which covariates a given model actually uses is recorded on its page under [Drug library](help:drugs/index); some use all four, some none.

## The fields

| Field | Units | Range | Notes |
|---|---|---|---|
| Age | years or months | 0 to 90 | Toggle the unit beside the field. Ages of 90 and above are entered as 90 (see below). |
| Weight | kg or lb | 0.1 to 500 kg | |
| Height | in or cm | 10 to 200 cm | |
| Sex | male or female | | |
| CYP 2D6 | phenotype | | Scales the active metabolites of codeine, tramadol, hydrocodone and oxycodone. |
| Adjust weight to fat-free mass | checkbox | | On by default; scales most models to fat-free mass. |
| Baseline serum osmolality | mOsm/kg | 200 to 400 | The patient's starting value, 280 by default. Read only by [mannitol](help:drugs/mannitol), which is plotted as the serum osmolality it produces on top of this value. |

Changing a covariate re-simulates every drug at once; there is no Apply step for the patient.

**Adjust weight to fat-free mass** scales most of the models to the patient's fat-free mass rather than total body weight; it is on by default. **CYP 2D6**, with the phenotypes the genotyping laboratories report (poor, intermediate, normal, ultrarapid), scales the formation of the active metabolites. See [Scaling to fat-free mass](help:models/fat-free-mass) and [Active metabolites](help:models/metabolites).

## What the models do with them

Internally every model receives age in years, weight in kilograms, height in centimetres and sex. From these the models derive what they need: body mass index, lean body mass by the James equation, fat-free mass by whichever equation the model's authors used (Al-Sallami's for the Eleveld models, Janmahasatian's for the Kim remifentanil model), post-menstrual age for the maturation functions, and allometric size scaling. Unless the fat-free-mass box is unticked, the models that do not carry their own body-size covariate are scaled to the patient's fat-free mass. See [Covariates and body size](help:models/covariates) and [Scaling to fat-free mass](help:models/fat-free-mass) for the equations.

A few models switch between parameter sets on a covariate:

- **Dexmedetomidine** uses an infant model (with cardiopulmonary-bypass events) at age ≤ 1 year and an adult model above it.
- **Remifentanil** uses one model at BMI below 30 and another at BMI of 30 and above.
- **Oxytocin** switches to a rat model if the weight is 1 kg or less; this is a research setting, not a clinical one.

## Age 90 and above

An age of 90 or above is protected health information under the HIPAA Safe Harbor rule, so the field stops at 90 and a note appears if you reach it. For the purposes of these models the difference between 90 and 95 is small.

## The disabled fields

**Pregnant** (shown for women of child-bearing age) and **Renal Function** are present but greyed out. They were added ahead of the models that will use them, so that the interface shows the intent, and no drug responds to them. Renal function is estimated by four models (cefazolin, vancomycin, gentamicin and sugammadex) from age, weight and sex at an **assumed normal creatinine**, because stanpumpR collects none; renal decline with age is represented, renal impairment is not. **CYP 2D6** is no longer among them: it is now active, scaling the active metabolites described under [Active metabolites](help:models/metabolites).

## Default patient

The app opens with a 50-year-old woman, 60 kg, 66 inches (168 cm). The teaching scenarios each set their own patient.
