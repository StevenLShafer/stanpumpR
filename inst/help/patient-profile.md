The first panel in the left sidebar. These covariates are passed to every drug's pharmacokinetic model, so they change the predictions. Which covariates a given model actually uses is recorded on its page under [Drug library](help:drugs/index); some use all four, some none.

## The fields

| Field | Units | Range | Notes |
|---|---|---|---|
| Age | years or months | 0 to 90 | Toggle the unit beside the field. Ages of 90 and above are entered as 90 (see below). |
| Weight | kg or lb | 0.1 to 500 kg | |
| Height | in or cm | 10 to 200 cm | |
| Sex | male or female | | |
| CYP 2D6 | phenotype | | Scales the formation of opioid active metabolites and the clearance (and in one case the oral bioavailability) of the drugs the enzyme clears; [Cautions](help:cautions) lists them. |
| CYP 2C19 | phenotype | | Changes the clearance of the drugs the enzyme clears; [Cautions](help:cautions) lists them. |
| CYP 2C9 | diplotype | \*1/\*1 by default | Changes the clearance (for phenytoin, the maximal elimination rate) of the drugs the enzyme clears, the vitamin-K antagonists among them; [Cautions](help:cautions) lists them. See **The CYP2C9 default** below. |
| CYP 2B6 | phenotype | | Changes the clearance of the drugs the enzyme clears; [Cautions](help:cautions) lists them. |
| VKORC1 | genotype (-1639G>A) | AA by default | Sets the sensitivity of warfarin, fluindione and acenocoumarol: the A allele lowers the C50, so the INR rises on less drug. AA, the most sensitive genotype, is assumed until a genotype is entered, so that the curve errs toward the larger INR; see **VKORC1 and ancestry** below. |
| CYP4F2 | genotype (rs2108622) | CC by default | Sets warfarin's sensitivity: a C allele lowers the C50. CC, the most sensitive genotype, is assumed until a genotype is entered, so that the curve errs toward the larger INR. |
| Adjust weight to fat-free mass | checkbox | | On by default; scales most models to fat-free mass. |
| Serum creatinine | mg/dL | 0.2 to 15, or blank | Optional. Used by the drugs with a renal covariate; the field's hover tip and [Cautions](help:cautions) list them. Blank means an assumed normal creatinine for the patient's age and sex: 0.9 in men and 0.8 in women, and in a child the normal for age (about 0.25 in infancy, 0.35 at five, 0.5 at ten). See [Renal function](help:models/covariates). |
| Pregnant | Yes / No | women of 12 to 59 | Applies the documented changes of late pregnancy to the drugs whose models have them; see [Pregnancy](help:models/pregnancy). |
| Gestational age at birth | weeks | 22 to 44, or blank | Blank means a term birth (40 weeks). Read by the neonatal models with a maturation term, which add the postnatal age to it; [Cautions](help:cautions) lists them. |
| Baseline serum osmolality | mOsm/kg | 200 to 400 | The patient's starting value, 280 by default. Read only by [mannitol](help:drugs/mannitol), which is plotted as the serum osmolality it produces on top of this value. |
| Opioid tolerance | none, mild, somewhat, highly tolerant | none by default | Multiplies every opioid's MEAC by 1, 1.5, 2 or 3, so the % MEAC panel and overlay, the propofol-opioid interaction and the opioid-MAC interaction treat the opioid as less effective. The opioid's shaded band and its time-until-threshold level rise by the same factor. An opioid without a MEAC (codeine, tramadol, buprenorphine) is unchanged, but the active metabolite it forms is not: the morphine from codeine and the desmetramadol from tramadol are scaled like any opioid with a MEAC, and so is their contribution to the % MEAC and both interactions. See [MEAC: comparing opioids](help:models/meac). |

Age, weight, height, sex and the fat-free-mass box are always shown. Every other field is shown only while a drug that reads it is in the dose table: a genotype, the serum creatinine or the gestational age while a drug whose model reads it is, Pregnant while a drug with a measured change in pregnancy is, the baseline osmolality while mannitol is, and Opioid tolerance while an opioid is. A hidden field keeps its value for when it returns.

If Age, Weight or Height is blank or not a number (a lone `.`, say), the plot area says which field needs a number, and the plot returns as soon as it has one. A number outside the range is reported in a message.

Changing a covariate re-simulates every drug at once; there is no Apply step for the patient.

Holding the mouse over the question mark beside a genotype field or the Serum creatinine field shows a tip naming the drugs that read it; every other drug ignores the field. The tips are built from the models themselves, so a drug added later that reads a field appears in its tip.

**Adjust weight to fat-free mass** scales most of the models to the patient's fat-free mass rather than total body weight; it is on by default. **CYP 2D6** takes the phenotypes the genotyping laboratories report (poor, intermediate, normal, ultrarapid); **CYP 2C19** and **CYP 2B6** take poor, intermediate, normal, rapid and ultrarapid; **CYP 2C9** takes the diplotype of the common \*1, \*2 and \*3 alleles (\*1/\*1 by default). Which drugs read each one is the table under **Which drugs each field reaches** on [Cautions](help:cautions), built from the models, so it stays current as drugs are added; [the methadone genotype scenario](scenario:methadone-cyp2b6) is a worked example. See [Antidepressant models and their limits](help:models/antidepressants), [Scaling to fat-free mass](help:models/fat-free-mass) and [Active metabolites](help:models/metabolites).

## VKORC1 and ancestry

**VKORC1**'s default is chosen for safety, not for how common the genotype is — in two of the three groups in the table below it is *not* the commonest genotype. The −1639A allele lowers the half-maximal concentration of every vitamin-K antagonist in the library, so the AA patient reaches a given INR on the least drug: ×0.725 on the S-warfarin C50 of the [library warfarin model](help:drugs/warfarin), the lowest EC50 of the three in the [Hamberg companion](help:models/hamberg) (2.20 against 4.61 mg/L for GG), and the sources' own sensitive reference for [fluindione](help:drugs/fluindione) and [acenocoumarol](help:drugs/acenocoumarol), where a G allele raises the C50 ×1.30 and ×2.29. **AA is therefore assumed until a genotype is entered**, by the same rule the CYP4F2 field follows: with the genotype unknown the curve should show the larger INR rise, so a dose read off it errs toward underdosing. An underdose at the start of therapy is corrected by raising the dose; an overdose risks bleeding, which is these drugs' primary toxicity and may need reversal with vitamin K. That is the reasoning behind the assumption, not dosing guidance: these INR models are research and teaching models, not validated clinical decision support, and a simulated INR is not a dose recommendation — see the cautions on the [warfarin](help:drugs/warfarin), [fluindione](help:drugs/fluindione) and [acenocoumarol](help:drugs/acenocoumarol) pages.

So the genotype is worth entering from the patient when it is known — and, when it is not, a genotype chosen from their ancestry is a better starting point than the deliberately conservative default, because this allele's frequency runs from a tenth of alleles to nine tenths between populations. What follows is population context, not a finding about any patient. In the International Warfarin Pharmacogenetics Consortium cohort of 4886 chronic warfarin users the −1639A allele frequency was 91.0% in 641 Asian patients, 37.8% in 681 White patients and 10.1% in 368 Black patients (Limdi and colleagues, *Blood* 2010;115:3827-3834, Table 3, [DOI 10.1182/blood-2009-12-255992](https://doi.org/10.1182/blood-2009-12-255992); the paper's own group labels). Its Asian group **excludes South Indians**: the paper's 91 Indian subjects "exhibited substantially different VKORC1 allele frequencies compared with other Asians and were therefore excluded from all analysis", so the Asian row below does not describe them, and this cohort gives no figure for a South Indian or other South Asian patient. Within each of the three groups, though, the paper found the haplotype distribution consistent across geographically separated populations. The genotype proportions those frequencies imply under Hardy-Weinberg are:

| Group | −1639A allele | AA | GA | GG |
|---|---|---|---|---|
| Asian | 91.0% | 83% | 16% | 1% |
| White | 37.8% | 14% | 47% | 39% |
| Black | 10.1% | 1% | 18% | 81% |

The AA default is therefore also the commonest genotype in that Asian group — the 264 Chinese patients the library's warfarin model was fitted in were 82.6% AA, 17.0% GA and 0.4% GG, within a point of the expectation above — while the single most likely genotype is **GG** in a Black patient and **GA** in a White one. These are group frequencies, not predictions about a person: a White patient has a 14% chance of being AA and a Black patient a 19% chance of carrying an A allele, so entering the patient's own genotype is always better than reading one off the table. VKORC1 explains more of the warfarin dose requirement than any other single variant, and the same genotype predicts a similar dose requirement in every group — what differs between populations is how common each genotype is.

## The CYP2C9 default

**CYP 2C9** defaults to **\*1/\*1**, two normal-function alleles, because it is the most common diplotype. In Moridani and colleagues, *Clin Biochem* 2006;39:606-612 ([doi:10.1016/j.clinbiochem.2006.01.023](https://doi.org/10.1016/j.clinbiochem.2006.01.023)), \*1/\*1 was 77% of the 178 controls in its Table 2 (59% of the White, 96% of the Asian and 94% of the African-American controls) and 67% of 189 patients attending an anticoagulation clinic.

That is a different rule from VKORC1's. For warfarin, fluindione and acenocoumarol, \*1/\*1 is not a sensitive diplotype: none of the six predicts a smaller INR rise. In the same cohort, patients with \*1/\*3 or two variant alleles needed 32 to 67 per cent less warfarin than \*1/\*1 patients. The library's warfarin model distinguishes only \*1/\*3, the one CYP2C9 covariate its source retained, so its other diplotypes behave as \*1/\*1; fluindione and acenocoumarol distinguish any \*3 carrier. **A patient taking warfarin should be genotyped**, and the measured diplotype entered here.

Whether a default should be the most common genotype or the most sensitive one is a judgement, not a measurement, and the choices here are Steven Shafer's: the most sensitive genotype for VKORC1 and CYP4F2, which only the vitamin-K antagonists read, and the most common for CYP2C9, which phenytoin and meloxicam read too. Clinicians who titrate warfarin, phenytoin and the other drugs these fields affect are invited to say whether these defaults are the right ones: contact Steven Shafer (steven.shafer@stanford.edu).

## What the models do with them

Internally every model receives age in years, weight in kilograms, height in centimetres and sex. From these the models derive what they need: body mass index, fat-free mass by whichever equation the model's authors used (Al-Sallami's for the Eleveld models, Janmahasatian's for the Kim remifentanil model; Al-Sallami's, too, in place of the James lean body mass the meloxicam model was published with), the James lean body mass for the Schnider and Minto models, which keep it as the pumps do, post-menstrual age for the maturation functions, and allometric size scaling. Unless the fat-free-mass box is unticked, the models that do not carry their own body-size covariate are scaled to the patient's fat-free mass. See [Covariates and body size](help:models/covariates) and [Scaling to fat-free mass](help:models/fat-free-mass) for the equations.

A few models switch between parameter sets on a covariate:

- **Dexmedetomidine** uses an infant model (with cardiopulmonary-bypass events) at age ≤ 1 year and an adult model above it.
- **Remifentanil** uses Eleveld's model, and Kim's in an adult (18 or over) with a BMI of 30 or more, unless one of Kim's volumes or clearances comes out zero or negative for the patient, when it stays on Eleveld's.
- **Oxytocin** switches to a rat model if the weight is 1 kg or less; this is a research setting, not a clinical one.

## Age 90 and above

An age of 90 or above is protected health information under the HIPAA Safe Harbor rule, so the field stops at 90 and a note appears if you reach it. For the purposes of these models the difference between 90 and 95 is small.

## Pregnant, and the fields only some drugs read

**Pregnant** (shown for a woman of 12 to 59 years) applies the pharmacokinetic changes of late pregnancy to every drug whose change has been measured, each by its source's figure: among them morphine, remifentanil, methadone, buprenorphine, midazolam, the antiseizure drugs, enoxaparin, metformin and the renally cleared antibiotics. Every other drug is unchanged, and the References panel names the source while the field is changing a drug. A drug's own page says what is known of it in pregnancy; see [Pregnancy](help:models/pregnancy). With the creatinine left blank, the renal models add the 50% rise in filtration of pregnancy; a measured creatinine already carries it. The **Serum creatinine** field replaced the greyed-out Renal Function selector: leave it blank and the renal models assume a normal creatinine, which represents renal decline with age but not renal impairment. **CYP 2D6** is no longer among them: it is now active, scaling the active metabolites described under [Active metabolites](help:models/metabolites).

## Default patient

The app opens with a 50-year-old woman, 60 kg, 66 inches (168 cm). The teaching scenarios each set their own patient.
