stanpumpR is an advisory program for understanding the implications of published pharmacokinetic models. How those models apply to an individual patient is a matter of clinical judgement by the health care provider. These cautions are worth reading once, and worth remembering whenever a curve looks precise.

## Predictions, not measurements

Every curve is what a published model expects for a *typical* patient with the covariates entered. Between-patient variability in pharmacokinetics is typically 20 to 40 per cent for clearance and often larger for volumes; pharmacodynamic variability is larger still. A real patient's concentration could easily be half or double the line, and the plot gives no visual hint of that uncertainty. Nothing here replaces clinical observation or monitoring.

## Models have domains

Each model was fitted to a particular population: often healthy volunteers or elective surgical patients of a particular age range, size and condition. Entering covariates outside that range produces numbers, but the numbers deserve less confidence than the plot's crispness suggests. The very young, the very old, the very large, and the critically ill are where extrapolation is most likely. Each drug's page under [Drug library](help:drugs/index) says what population its model came from.

Some models have no covariates at all: the same dose in milligrams gives the same curve in a 20 kg child and a 120 kg adult. The drug page says so. Dose in per-kilogram units if you want size taken into account, and remember that per-kilogram dosing is itself an assumption the model did not test.

## A curve assumes the drug was taken

"Drugs don't work in patients who don't take them" (C. Everett Koop, US Surgeon General). Every curve assumes that each dose in the dose table was given at the time written. In the operating room that is true. For a drug a patient takes at home, it often is not:

- **Adherence varies as much as pharmacokinetics.** Blaschke, Osterberg, Vrijens and Urquhart's review (*Annu Rev Pharmacol Toxicol* 2012) cites Harter and Peck's estimate that variable adherence "vies with pharmacokinetics as the leading source of variance in drug response in ambulatory care settings." They add a corollary to Koop: "Drugs work erratically in patients who take them erratically." A pharmacokinetic model describes the second source of variance and knows nothing of the first.
- **Doses stop and pause.** In the same review, 4,783 patients were taking once-daily antihypertensive drugs with few side effects. Only 5% went through their first year without an interruption in dosing. About half stopped altogether within the year, and most of the rest took repeated drug holidays.
- **Trials do better than practice, and still not well.** Across 95 clinical studies, almost 40% of participants who started treatment had stopped within a year (Blaschke 2012). Clinical trials of chronic treatment report average adherence of 43 to 78 percent (Osterberg and Blaschke, *N Engl J Med* 2005). Adherence is better in closely monitored trials than in clinical practice (Glasser, *J Clin Hypertens* 2018).
- **Failed treatment is often missed doses.** In a selected group of patients whose hypertension was thought refractory to treatment, monitoring adherence electronically brought blood pressure under control in more than 30 percent, and showed lapsed adherence in another 20 percent (Osterberg and Blaschke 2005). Poor adherence accounts for 33 to 69 percent of medication-related hospital admissions in the United States. When a patient responds less than expected, the most common response is to raise the dose (Blaschke 2012).
- In 2003 the World Health Organization judged that better adherence "may have a far greater impact on health ... than any improvement in specific medical treatments" (quoted by Glasser).

Two things follow for stanpumpR.

**Many oral models describe ideal dosing.** They come from studies in which every dose was given, timed and recorded, often under observation and fasting. For patients at home such a curve is likely to overstate the typical exposure. It is still a typical prediction, not a maximum: an individual patient may sit above it or below it. Sometimes a model fitted to patients dosing themselves exists alongside a supervised one, describing the same product and population. Then stanpumpR uses the outpatient model, so the default curve is what patients actually achieve. The first case is [oral semaglutide](help:drugs/semaglutideOral), whose outpatient exposure in the PIONEER trials was half that of supervised dosing.

**When a drug seems not to work, ask whether it is being taken** before raising the dose. A level below the curve, or a response short of the expected one, is often missed doses rather than a drug that failed. stanpumpR can show what a missed dose does: [Skip next dose](help:dose-table) removes one dose of a schedule. [Bayesian updating](help:models/bayesian) assumes every dose in the table was taken, so it reads missed doses as a faster clearance or a larger volume. Enter the doses the patient actually took before fitting a level. The same reviews recommend "forgiving" drugs for patients who miss doses: drugs with long half-lives, depot preparations and transdermal patches, whose effect a delayed or missed dose changes little. The plot shows how forgiving a regimen is: skip a dose and watch how far the curve falls.

## Which drugs each field reaches

The Patient Profile fields beyond age, weight, height and sex reach only the models that name them, and a field left blank or at its default silently applies that field's own assumption (a normal creatinine for the patient's age and sex, a term birth, each model's reference albumin, the normal-metaboliser phenotype for CYP 2D6, CYP 2C19 and CYP 2B6, the most common diplotype, \*1/\*1, for CYP 2C9, and for VKORC1 and CYP4F2 the most sensitive genotype), so it is worth knowing which drugs each one affects. The table is built from the models themselves, so it lists exactly the drugs that read each field; for every other drug the field has no effect.

<!-- generated: field-readers-table -->

What each field does:

- **Pregnant** applies the change in late pregnancy that has been measured for most of these drugs. For a few it is an extrapolation rather than a measurement: mannitol, hydrochlorothiazide, procainamide, pramipexole (immediate- and extended-release) and extended-release amantadine take the 50% rise in glomerular filtration on their renal terms, and cefalexin takes a 1.5-fold clearance by analogy with cephradine's measured ratio and that rise. The first table of [Pregnancy](help:models/pregnancy) gives each one's factor and source. A drug whose change is known only in direction is not adjusted; each drug's page says what is known. The factors describe the third trimester and term, and the rise in filtration is applied to a renal term only when the serum creatinine is left blank. The Minto remifentanil and Schnider propofol companion models read it too.
- **Serum creatinine**, left blank, means a normal creatinine for the patient's age and sex, and a patient with impaired kidneys will then clear these drugs more slowly than the plot shows. Lisinopril is a second case worth knowing: renal function sets the height of its whole curve, but because its absorption is slower than its elimination the terminal slope you could measure off the plot is an absorption half-life and barely moves, so impaired kidneys raise that curve without appearing to lengthen it. Fosinopril is a third: renal function is its only covariate, but it acts as a two-plateau ramp rather than a slope — clearance halves from normal kidneys to impaired ones and is then flat across the whole impaired range, because the biliary route takes over as filtration falls — so a large change in creatinine can move its curve not at all, and a small one near the threshold can move it a good deal. It matters most for hydrochlorothiazide, whose only covariate is renal function: its total clearance falls about fourfold across the creatinine clearances its source tabulates, 120 down to 30 mL/min, and nearly sixfold from a normal creatinine to the highest value this program accepts.
- **Gestational age at birth** reaches the models whose clearance matures with postmenstrual age, the gestational age at birth plus the age (neonatal dobutamine, and the busulfan, dexmedetomidine and piperacillin models; the table above lists them). Left blank, the birth is taken as at term (40 weeks), and a premature infant's clearance is then overstated, so the plotted concentration is too low.
- **Serum albumin** reaches the two methotrexate models, docetaxel, protein-bound paclitaxel and tolcapone (the table above lists them). Left blank, each holds its own reference (for methotrexate 4 g/dL in the adult model, a normal albumin in the children's), and a hypoalbuminaemic adult, the majority of the adult model's own cohort (median 3.3 g/dL), then clears methotrexate more slowly than the plot shows: 11% at 3.3 g/dL, 24% at 2.5.
- **Serum AST** reaches epirubicin, whose clearance falls as AST rises. Left blank, a normal 20 U/L is used, and a patient with liver injury then clears epirubicin more slowly than the plot shows.
- **Pulmonary hypertension** reaches sildenafil and tadalafil, drugs given both for pulmonary arterial hypertension and for another use, whose patients' pharmacokinetics differ. Left at No, the model of the other use is drawn.
- **Infection** is read by the antibiotic models whose clearance or volumes were fitted separately in each kind of infection (the table above lists them). Left at None, the healthy volunteers' pharmacokinetics are drawn, while a patient given an antibiotic is usually infected: pneumonia, for example, roughly doubles ceftolozane's central volume, so the plot then overstates the concentration a patient with pneumonia reaches; imipenem goes the other way, its patients with pneumonia clearing it 38% more slowly, so None understates their exposure.
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

One model rests on incompletely cited data (the infant dexmedetomidine model), and several effect-site parameters are described in the code as guesses. The drug pages say so. See [Bibliography](help:references).

## Suggest Dosing is a search, not a proof

The regimen it proposes is found by non-linear regression. It will be good; a better one may exist. Decreasing targets are not supported. See [Suggest Dosing](help:suggest-dosing).

## Teaching scenarios are not dosing recommendations

The scenarios use ordinary doses chosen to make a pharmacokinetic point visible. They are not recommendations for any patient.

## Privacy

Running a simulation does not require a patient's name or record identifier: the inputs are covariates, doses and events, not identifiers. An age of 90 or above is treated as protected health information and is entered as 90. That is a limit on what the simulation itself needs, not a guarantee about every path data can take: a bookmark URL encodes the full patient and dose-table state, and the app's access logs, the Export panel's files and email (see its own Privacy note) and the Simulation API can all carry whatever covariates, doses, comments or identifiers a user puts into them. Keeping any of that out of a URL, an exported file, an email, a log or an API request is the responsibility of whoever enters it and whoever operates that deployment, not a property the software enforces.
