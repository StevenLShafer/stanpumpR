Pregnancy changes how the body handles drugs. Plasma volume rises by about half, cardiac output and renal blood flow rise with it, glomerular filtration is up by half from the second trimester, albumin falls, and several hepatic enzymes (CYP2D6, CYP3A4, CYP2B6, CYP2C9 and the glucuronosyltransferases) are induced while CYP1A2 is inhibited. Because volume and clearance can both rise, the half-life of a drug may lengthen, shorten or stay the same, and each drug has to be studied on its own. The review stanpumpR follows is by Ansari, Carvalho, Shafer and Flood (*Anesth Analg* 2016;122:786-804), which collects what is known for the drugs anesthesiologists give to pregnant women and parturients.

## The Pregnant field

**Pregnant** in the Patient Profile is offered for a woman of 12 to 59 years. Ticked, it applies the review's figures to the five drugs for which it gives one, and its 50% rise in glomerular filtration to three renally cleared antibiotics, all listed in the first table below, and does nothing to any other drug. The adjustments describe **late pregnancy and labour**, where nearly all of the review's data were collected; the field makes no distinction between trimesters, and a woman early in pregnancy is closer to the unticked state. While the field is changing a drug, the References panel names the review beside that drug's own citation.

The weight entered should be the woman's actual weight. Pregnancy weight is mostly fat-free, which is why the fat-free-mass scaling may be turned off for her; see [Scaling to fat-free mass](help:models/fat-free-mass).

A control hidden by a change of age or sex leaves the flag off: a man, or a woman outside that age range, is never simulated as pregnant whatever the control last said. The flag travels with a bookmark and with the [scripting interface](help:scripting) (`pregnant = TRUE` in `simulateDrugsWithCovariates()` and `getDrugPK()`, and `patient.pregnant` in the simulation API), but not yet with the [TCI API](help:tci-api), whose patient block has no pregnancy field.

## What the review found, and what the model does

<!-- generated: pregnancy-table -->

## How each factor was chosen

The review gives most of its findings as a direction and some as a figure, and the library applies only the figures, each in the simplest way that reproduces the review's statement:

- **Morphine.** "Volume of distribution unchanged, clearance >70%" (Table 3; an increase is meant). Elimination clearance is multiplied by 1.7; the volumes and distribution clearances are as published, so the half-life falls, as the review says.
- **Remifentanil.** "Clearance more than doubles." Elimination clearance is doubled, on the Eleveld and Kim models and on the Minto companion model, so the two lines of the panel move together. "More than" is not carried, because the review gives no further figure.
- **Oxycodone.** The review's figure is a pair of elimination half-times, 3.8 hours in nonpregnant women and 2.6 in labour. Elimination clearance is multiplied by 3.8/2.6 = 1.46, which reproduces that ratio when the volumes are unchanged. The renal factor already in the model is not touched (it is capped at normal renal function), and oxymorphone formation is left as it is.
- **Methadone.** A half-life of 8 to 20 hours against 24 hours when not pregnant. The clearance of both enantiomers is multiplied by 24/14 = 1.71, the nonpregnant half-life over the midpoint, before the racemic reduction described on the [methadone page](help:drugs/methadone) is fitted again.
- **Dexamethasone.** "Clearance doubled in single gestation." Elimination clearance is doubled; a singleton pregnancy is assumed.
- **Cefazolin, gentamicin and vancomycin.** The review reports a higher clearance for each, with a larger volume of distribution, but prints no factor for any of them. Its physiological statement is that glomerular filtration is "increased 50% by the second trimester", and each of these models computes its clearance from an estimate of renal function (creatinine clearance or eGFR, from the serum creatinine or an assumed normal one). That estimate is multiplied by 1.5. Gentamicin's clearance is proportional to it, so rises by 1.5; cefazolin's follows a power of 0.586, so rises by 1.27; vancomycin's is linear with a small negative intercept, so rises by slightly more than 1.5. The larger volumes are not applied, because the review gives no figure, so the simulated **peaks** of these antibiotics in a parturient are too high, while the rate of decline is about right.

## What is not represented

- **Volumes of distribution.** The review describes a larger volume for the hydrophilic antibiotics, and lower peaks in general for water-soluble drugs, without figures. No volume is changed.
- **Protein binding.** Albumin falls to 70 to 80 per cent of its prepregnancy value by term, so the free fraction of highly bound drugs rises; the review names midazolam, phenytoin, valproate and the local anesthetics, whose toxicity at a given total concentration rises for that reason. The library plots total concentrations (unbound cefazolin excepted), and the free fraction is not an input.
- **Direction without magnitude.** Buprenorphine, codeine, hydrocodone, midazolam, clindamycin and aspirin are described by the review as changed but without a figure, and are left as published. For codeine the change is a faster, higher formation of morphine by induced CYP2D6, on top of whatever the **CYP 2D6** field says.
- **Other renally cleared drugs.** The rise in glomerular filtration applies to every renally cleared drug, but only the three antibiotics the review names read the field. Mannitol, sugammadex, gabapentin, pregabalin, lacosamide, levetiracetam, meloxicam, metformin, canagliflozin, dapagliflozin, empagliflozin, ertugliflozin, exenatide and lixisenatide do not, because the review does not describe them.
- **The fetus and the placenta.** Placental transfer, fetal metabolism and the fetal-to-maternal ratios the review tabulates are not modelled; the curves are maternal.
- **Labour.** Pain, anxiety and opioids delay gastric emptying during labour, which slows oral absorption; the oral absorption parameters are unchanged.
- **After delivery.** The changes reverse over weeks to months (renal function by three months postpartum); the field is simply on or off.

## Scenario

[Remifentanil in labour: the parturient's doubled clearance](scenario:remifentanil-labour) shows the largest of the adjustments on the drug most often given to a labouring woman.
