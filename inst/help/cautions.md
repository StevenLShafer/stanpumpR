stanpumpR is an advisory program for understanding the implications of published pharmacokinetic models. How those models apply to an individual patient is a matter of clinical judgement by the health care provider. These cautions are worth reading once, and worth remembering whenever a curve looks precise.

## Predictions, not measurements

Every curve is what a published model expects for a *typical* patient with the covariates entered. Between-patient variability in pharmacokinetics is typically 20 to 40 per cent for clearance and often larger for volumes; pharmacodynamic variability is larger still. A real patient's concentration could easily be half or double the line, and the plot gives no visual hint of that uncertainty. Nothing here replaces clinical observation or monitoring.

## Models have domains

Each model was fitted to a particular population: often healthy volunteers or elective surgical patients of a particular age range, size and condition. Entering covariates outside that range produces numbers, but the numbers deserve less confidence than the plot's crispness suggests. The very young, the very old, the very large, and the critically ill are where extrapolation is most likely. Each drug's page under [Drug library](help:drugs/index) says what population its model came from.

Some models have no covariates at all: the same dose in milligrams gives the same curve in a 20 kg child and a 120 kg adult. The drug page says so. Dose in per-kilogram units if you want size taken into account, and remember that per-kilogram dosing is itself an assumption the model did not test.

## Which drugs each field reaches

The Patient Profile fields beyond age, weight, height and sex reach only the models that name them, and a field left blank or at its default silently gives the typical value, so it is worth knowing which drugs each one affects. The table is built from the models themselves, so it lists exactly the drugs that read each field; for every other drug the field has no effect.

<!-- generated: field-readers-table -->

What each field does:

- **Pregnant** applies the change in late pregnancy that has been measured for most of these drugs. For a few it is an extrapolation rather than a measurement: mannitol, hydrochlorothiazide, procainamide, pramipexole (immediate- and extended-release) and extended-release amantadine take the 50% rise in glomerular filtration on their renal terms, and cefalexin takes a 1.5-fold clearance by analogy with cephradine's measured ratio and that rise. The first table of [Pregnancy](help:models/pregnancy) gives each one's factor and source. A drug whose change is known only in direction is not adjusted; each drug's page says what is known. The factors describe the third trimester and term, and the rise in filtration is applied to a renal term only when the serum creatinine is left blank. The Minto remifentanil and Schnider propofol companion models read it too.
- **Serum creatinine**, left blank, means a normal creatinine for the patient's age and sex, and a patient with impaired kidneys will then clear these drugs more slowly than the plot shows. Lisinopril is a second case worth knowing: renal function sets the height of its whole curve, but because its absorption is slower than its elimination the terminal slope you could measure off the plot is an absorption half-life and barely moves, so impaired kidneys raise that curve without appearing to lengthen it. Fosinopril is a third: renal function is its only covariate, but it acts as a two-plateau ramp rather than a slope — clearance halves from normal kidneys to impaired ones and is then flat across the whole impaired range, because the biliary route takes over as filtration falls — so a large change in creatinine can move its curve not at all, and a small one near the threshold can move it a good deal. It matters most for hydrochlorothiazide, whose only covariate is renal function: its total clearance falls about fourfold across the creatinine clearances its source tabulates, 120 down to 30 mL/min, and nearly sixfold from a normal creatinine to the highest value this program accepts.
- **Gestational age at birth** reaches the neonatal dobutamine model, whose clearance matures with postmenstrual age, the gestational age at birth plus the age. Left blank, the birth is taken as at term (40 weeks), and a premature infant's clearance is then overstated, so the plotted concentration is too low.
- **Baseline serum osmolality** reaches mannitol, which is plotted as the osmolality it produces on top of it.
- **CYP 2D6** reaches the four opioids through their active metabolites, the antipsychotics through their clearance, and metoprolol through its clearance and oral bioavailability.
- **CYP 2C9**: warfarin distinguishes only the \*1/\*3 diplotype from \*1/\*1; fluindione and acenocoumarol lower clearance for any \*3 carrier, so the INR rises. See [The CYP2C9 default](help:patient-profile).
- **CYP 2B6** lowers methadone's non-renal clearance of each enantiomer in reduced-function phenotypes. The rapid and ultrarapid categories are given the normal metaboliser's clearance, because the only group carrying the increased-function allele was four subjects of two different CPIC phenotypes, with no significance test. CPIC does not recommend changing methadone dosing on CYP2B6 genotype; the field shows the pharmacokinetic difference, which is measured, not a dosing rule.
- **VKORC1** and **CYP4F2** default to the most sensitive genotype, so that an unentered genotype predicts the larger INR rise; see [VKORC1 and ancestry](help:patient-profile).

## Provisional parameters in the newer drugs

Several of the recently added opioids carry parameters that are explicitly provisional and uncited: the time to peak effect of hydrocodone and oxymorphone, the effect-site rate constant of desmetramadol, and the minimum effective concentrations of hydrocodone and oxymorphone. Their pages say so. They are working values, not validated ones.

## Illicit-drug models are research estimates

The research models of drugs of abuse (diamorphine, cocaine, methamphetamine, LSD, psilocybin and MDMA) are hidden until **Show illicit drugs**, in the *Dose table* panel of the left sidebar, is ticked. Their curves are plasma-exposure estimates for research and teaching, not dosing advice or safety thresholds. Each is plasma only, with no effect site, and each offers only the routes whose input model the human evidence identifies. See [Research models of drugs of abuse](help:models/illicit-drugs).

## The shaded band is orientation, not a target

The "typical" range comes from the drug library and is a rough published range for a typical indication. It is not tailored to your patient, your stimulus, or your co-administered drugs. It can be edited under Settings.

## The interaction panel is one model of one stimulus

The propofol-opioid interaction surface describes the probability of no response to laryngoscopy in the population Bouillon and colleagues studied. It is not a depth-of-anesthesia monitor and says nothing about other stimuli or other drug pairs. See [Propofol-opioid interaction](help:models/interaction).

## The opioid-MAC interaction is approximate

The reduction of MAC by opioids is modelled with parameters that are a rough fit to a small number of published points which disagree with one another. It is expected to be replaced. See [Opioid reduction of MAC](help:models/opioid-mac).

## Weak citations are flagged, not hidden

Two models rest on unpublished or incompletely cited data (oxytocin, and the infant dexmedetomidine model), and several effect-site parameters are described in the code as guesses. The drug pages say so. See [Bibliography](help:references).

## Suggest Dosing is a search, not a proof

The regimen it proposes is found by non-linear regression. It will be good; a better one may exist. Decreasing targets are not supported. See [Suggest Dosing](help:suggest-dosing).

## Teaching scenarios are not dosing recommendations

The scenarios use ordinary doses chosen to make a pharmacokinetic point visible. They are not recommendations for any patient.

## Privacy

stanpumpR does not collect protected health information. An age of 90 or above is treated as protected health information and is entered as 90. The emailed slide carries only what you put in it; the comment box asks you to confirm that your comment contains none.
