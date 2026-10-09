stanpumpR is an advisory program for understanding the implications of published pharmacokinetic models. How those models apply to an individual patient is a matter of clinical judgement by the health care provider. These cautions are worth reading once, and worth remembering whenever a curve looks precise.

## Predictions, not measurements

Every curve is what a published model expects for a *typical* patient with the covariates entered. Between-patient variability in pharmacokinetics is typically 20 to 40 per cent for clearance and often larger for volumes; pharmacodynamic variability is larger still. A real patient's concentration could easily be half or double the line, and the plot gives no visual hint of that uncertainty. Nothing here replaces clinical observation or monitoring.

## Models have domains

Each model was fitted to a particular population: often healthy volunteers or elective surgical patients of a particular age range, size and condition. Entering covariates outside that range produces numbers, but the numbers deserve less confidence than the plot's crispness suggests. The very young, the very old, the very large, and the critically ill are where extrapolation is most likely. Each drug's page under [Drug library](help:drugs/index) says what population its model came from.

Some models have no covariates at all: the same dose in milligrams gives the same curve in a 20 kg child and a 120 kg adult. The drug page says so. Dose in per-kilogram units if you want size taken into account, and remember that per-kilogram dosing is itself an assumption the model did not test.

## Disabled covariates

Pregnancy appears in the interface but does not influence any prediction. Renal function is estimated by mannitol, vancomycin, gentamicin, cefazolin, sugammadex, gabapentin and pregabalin from the **Serum creatinine** field; if it is left blank they assume a normal creatinine, and a patient with impaired kidneys will then clear these drugs more slowly than the plot shows. CYP2D6 phenotype is now active, but only for the four drugs with modelled active metabolites; for every other drug it has no effect.

## Provisional parameters in the newer drugs

Several of the recently added opioids carry parameters that are explicitly provisional and uncited: the time to peak effect of hydrocodone and oxymorphone, the effect-site rate constant of desmetramadol, and the minimum effective concentrations of hydrocodone and oxymorphone. Their pages say so. They are working values, not validated ones.

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
