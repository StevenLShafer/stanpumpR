### The model

Methadone's kinetics are from Henthorn and Kharasch (*Clin Pharmacol Ther* 2026;119:739-750), who gave 6 mg of intravenous methadone hydrochloride to 64 healthy adults and measured the two enantiomers separately for 96 hours. R(-)-methadone, which carries almost all of the opioid effect, and S(+)-methadone each have their own three-compartment model. S(+) has 0.60 times the volumes of R(-) and 0.77 times its intercompartmental clearances, which the authors attribute to its weaker binding to alpha-1-acid glycoprotein, and a slightly lower clearance (0.091 against 0.103 L/min). Its concentrations therefore start higher and fall faster: the typical terminal half-lives are 48 hours for R(-) and 33 hours for S(+).

stanpumpR plots **racemic methadone, R(-) plus S(+)**, because that is what clinical assays, MEAC and the prescribing literature describe. Each enantiomer receives half of every dose. The sum of two three-compartment models has six exponentials and the simulator runs three, so for each patient the sum is refitted as a single three-compartment model with exactly the same total exposure. The fit is within about 2.5% of the two-enantiomer model for the first week after a dose and within 1% through repeated dosing; it falls short only in the far tail, a week or more after the last dose.

Doses are methadone hydrochloride, as dispensed; concentrations are methadone base.

### Oral methadone

Henthorn studied the intravenous route only. Oral bioavailability is 0.70, measured by Kharasch and colleagues (*Clin Pharmacol Ther* 2004;76:250-269) from simultaneous oral and intravenous doses in healthy volunteers. Henthorn's paper quotes about 85% in passing, and a review by Eap and colleagues reports a mean of about 0.75 with a range of 0.36 to 1. Absorption is set so the plasma concentration peaks 3 hours after an oral dose, the middle of the 2.5 to 4 hours that review reports. An oral dose of 10 mg therefore gives about 70% of the exposure of 10 mg intravenously, arriving over hours rather than minutes.

Methadone oral solution is entered as **mg PO liquid**, or **mg/kg PO liquid** for weight-based paediatric dosing, with the same qd, bid, tid and qid schedules as tablets. It uses the tablet's absorption and bioavailability: methadone hydrochloride is freely soluble, and Blume and colleagues (*Int J Clin Pharmacol Ther* 2015;53:335-344) found levomethadone tablets and oral solution bioequivalent (peak concentration 104% and exposure 99 to 101% of the solution's). That study was of the R(-) enantiomer alone; no comparable racemic study was found. Doses are milligrams of methadone hydrochloride, not millilitres, because oral solutions come in more than one strength (1, 2 and 10 mg/mL).

### Covariates

Weight is the only covariate, on the deep peripheral volume (weight to the power 1.23, taking the published 208 L as the value at 70 kg; the paper does not state its reference weight, and its subjects averaged 74 kg). The authors tested weight on every other parameter and found no effect, so clearance does not change with weight: a heavier patient has more tissue to fill but eliminates methadone no faster. With *Adjust weight to fat-free mass* ticked the volume term uses the fat-free-mass weight; unticked, total body weight. Age, sex and race had no effect. CYP2B6 genotype changed clearance to the EDDP metabolite; the model uses the wild-type (\*1/\*1) values, as the authors recommend for practical use.

### Effect site

Henthorn's study measured no effect. The time to peak effect is 10 minutes after an intravenous bolus, from Kharasch and colleagues (*Clin Pharmacol Ther* 2004;76:250-269), whose volunteers reached peak pupil constriction 10 to 15 minutes after intravenous methadone; 10 minutes is the early edge of that window. It also lies inside the range Inturrisi and colleagues (*Clin Pharmacol Ther* 1987;41:392-401) found for analgesia in cancer patients: their typical equilibration half-time was 3.6 minutes, but individual values ran from 1.3 to 23 minutes, and with Inturrisi's own kinetics a 10-minute peak corresponds to 15 minutes, toward the slow end. ke0 is solved for Henthorn's disposition from that time to peak. (Until October 2026 the model used 11.3 minutes, attributed to Inturrisi but not in the paper.) The combination is unusual: an effect site that equilibrates within minutes, like fentanyl's, attached to a disposition that takes days to clear. A dose of methadone works quickly and then stays. See [the methadone scenario](scenario:methadone-bolus).

### MEAC and typical concentrations

MEAC is 60 ng/mL of racemic methadone (0.06 mcg/mL in the plotted units); the shaded band is 48 to 120 ng/mL. It is Gourlay's postoperative value, as quoted by Inturrisi; Inturrisi's own concentration for half-maximal analgesia, in opioid-tolerant cancer patients, was 290 ng/mL.

### Where to be careful

The study population was young healthy volunteers, 50 to 100 kg, taking no drugs that affect CYP2B6 or CYP3A4. Inducers (rifampin, carbamazepine, phenytoin, efavirenz) can more than double methadone clearance and inhibitors can reduce it, and none of that is represented. Interindividual variability in clearance was 36% for R(-)- and 67% for S(+)-methadone. QT prolongation (mostly an S(+) effect) and NMDA antagonism are pharmacodynamic and not represented. Children are an extrapolation.
