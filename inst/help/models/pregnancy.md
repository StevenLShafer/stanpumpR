Pregnancy changes how the body handles drugs. Plasma volume rises by about half, cardiac output and renal blood flow rise with it, glomerular filtration is up by half from the second trimester, albumin falls, and several hepatic enzymes (CYP2D6, CYP3A4, CYP2B6, CYP2C9 and the glucuronosyltransferases) are induced while CYP1A2 and CYP2C19 are inhibited. Because volume and clearance can both rise, the half-life of a drug may lengthen, shorten or stay the same, and each drug has to be studied on its own. The review stanpumpR first followed is by Ansari, Carvalho, Shafer and Flood (*Anesth Analg* 2016;122:786-804), which collects what is known for the drugs anesthesiologists give to pregnant women and parturients. In October 2026 the literature was reviewed again for **every** drug in the library, and what was found for each is on that drug's own page, under **Pregnancy**, and in the tables below.

## The Pregnant field

**Pregnant** in the Patient Profile is offered for a woman of 12 to 59 years. Ticked, it applies the changes of pregnancy to the drugs listed in the first table below, each by the amount its source measured, and does nothing to any other drug. The adjustments describe **late pregnancy**, the third trimester and term, where most of the data were collected; where a source reports each trimester, the third-trimester value is the one applied. The field makes no distinction between trimesters, and a woman early in pregnancy is closer to the unticked state for most drugs (lamotrigine and levetiracetam are exceptions: their clearance rises early). While the field is changing a drug, the References panel names the source beside that drug's own citation.

The weight entered should be the woman's actual weight. Pregnancy weight is mostly fat-free, which is why the fat-free-mass scaling may be turned off for her; see [Scaling to fat-free mass](help:models/fat-free-mass).

A control hidden by a change of age or sex leaves the flag off: a man, or a woman outside that age range, is never simulated as pregnant whatever the control last said. The flag travels with a bookmark and with the [scripting interface](help:scripting) (`pregnant = TRUE` in `simulateDrugsWithCovariates()` and `getDrugPK()`, and `patient.pregnant` in the simulation API), but not yet with the [TCI API](help:tci-api), whose patient block has no pregnancy field.

## What is applied, and what is not

A drug is adjusted when its change in pregnancy has been **measured in women** with enough strength to apply: a population model with a pregnancy covariate, a study of the same women during pregnancy and after it, a comparison with nonpregnant women, or a therapeutic-drug-monitoring series of useful size. A few factors are **derived** from such a measurement with arithmetic their drug's page gives: midazolam's intravenous factor from oral data, metoprolol's split between clearance and first-pass, and the changes in total phenytoin and valproate that follow from their higher free fraction. A drug cleared mostly unchanged by filtration, whose model reads renal function, takes the **50% rise in glomerular filtration** of pregnancy where nothing contradicts it (see below).

A change known only in direction, or measured too weakly (a few women, no comparison group, a PBPK prediction alone), is **not** applied, and neither is a drug reported as unchanged or never studied; each drug's page says which. Models fitted in pregnant women (calcium, magnesium sulfate and oxytocin) already describe her, and the field does nothing to them.

<!-- generated: pregnancy-table -->

## The rise in filtration and the serum creatinine

Serum creatinine falls in pregnancy, to about 0.5 to 0.6 mg/dL, because filtration rises. The renally cleared models estimate renal function from the **Serum creatinine** field, or from an assumed normal creatinine when the field is blank. The assumed value is the normal for a woman who is *not* pregnant, so when the field is blank and Pregnant is ticked, the renal function the model reads is multiplied by 1.5. When a creatinine is entered, the estimate made from it already carries the rise, and nothing is added: entering the woman's measured creatinine is the better choice when it is known.

## Total and unbound concentrations

Albumin falls to 70 to 80 per cent of its prepregnancy value by term, so the free fraction of highly bound drugs rises. The library plots **total** concentrations (unbound cefazolin and prednisolone excepted). For phenytoin and valproate this matters: the total concentration falls much more than the unbound, active concentration, so in late pregnancy the plotted total falls below the therapeutic range while the unbound concentration is near its prepregnancy value. Their pages say so. For the local anesthetics, bupivacaine's free fraction rises by about half with an unchanged total, so toxicity at a given total concentration rises.

## What is not represented

- **Trimesters.** The field is the third trimester. Earlier in pregnancy most changes are smaller.
- **After delivery.** The changes reverse over days (lamotrigine, levetiracetam) to months (renal function by three months postpartum); the field is simply on or off.
- **Volumes of distribution**, where the source gives no figure. Most applied changes are to clearance; the volumes change only for acetaminophen, dexamethasone, enoxaparin, glyburide, ketorolac and metformin.
- **The anesthetic gases.** MAC falls by about 28% in early pregnancy and in the first hours after delivery (isoflurane; Gin and Chan 1994), a pharmacodynamic change the library records on each gas's page and does not apply.
- **The fetus and the placenta.** Placental transfer, fetal metabolism and fetal-to-maternal ratios are not modelled; the curves are maternal.
- **Labour.** Pain, anxiety and opioids delay gastric emptying during labour, which slows oral absorption; the oral absorption parameters are unchanged.

## Scenario

[Remifentanil in labour: the parturient's doubled clearance](scenario:remifentanil-labour) shows the largest of the anesthetic adjustments on the drug most often given to a labouring woman.
