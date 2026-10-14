### The model

Methadone's kinetics are from Henthorn and Kharasch (*Clin Pharmacol Ther* 2026;119:739-750), who gave 6 mg of intravenous methadone hydrochloride to 64 healthy adults and measured the two enantiomers separately for 96 hours. R(-)-methadone, which carries almost all of the opioid effect, and S(+)-methadone each have their own three-compartment model. S(+) has 0.60 times the volumes of R(-) and 0.77 times its intercompartmental clearances, which the authors attribute to its weaker binding to alpha-1-acid glycoprotein, and a slightly lower clearance (0.091 against 0.103 L/min). Its concentrations therefore start higher and fall faster: the typical terminal half-lives are 48 hours for R(-) and 33 hours for S(+).

stanpumpR plots **racemic methadone, R(-) plus S(+)**, because that is what clinical assays, MEAC and the prescribing literature describe. Each enantiomer receives half of every dose. The sum of two three-compartment models has six exponentials and the simulator runs three, so for each patient the sum is refitted as a single three-compartment model with exactly the same total exposure. The fit is within about 2.5% of the two-enantiomer model for the first week after a dose and within 1% through repeated dosing; it falls short only in the far tail, a week or more after the last dose.

Doses are methadone hydrochloride, as dispensed; concentrations are methadone base.

### Oral methadone

Henthorn studied the intravenous route only. Oral bioavailability is 0.70, measured by Kharasch and colleagues (*Clin Pharmacol Ther* 2004;76:250-269) from simultaneous oral and intravenous doses in healthy volunteers. Henthorn's paper quotes about 85% in passing, and a review by Eap and colleagues reports a mean of about 0.75 with a range of 0.36 to 1. Absorption is set so the plasma concentration peaks 3 hours after an oral dose, the middle of the 2.5 to 4 hours that review reports. An oral dose of 10 mg therefore gives about 70% of the exposure of 10 mg intravenously, arriving over hours rather than minutes.

Methadone oral solution is entered as **mg PO liquid**, or **mg/kg PO liquid** for weight-based paediatric dosing, with the same qd, bid, tid and qid schedules as tablets. It uses the tablet's absorption and bioavailability: methadone hydrochloride is freely soluble, and Blume and colleagues (*Int J Clin Pharmacol Ther* 2015;53:335-344) found levomethadone tablets and oral solution bioequivalent (peak concentration 104% and exposure 99 to 101% of the solution's). That study was of the R(-) enantiomer alone; no comparable racemic study was found. Doses are milligrams of methadone hydrochloride, not millilitres, because oral solutions come in more than one strength (1, 2 and 10 mg/mL).

### Covariates

Weight is the only size covariate, on the deep peripheral volume (weight to the power 1.23, taking the published 208 L as the value at 70 kg; the paper does not state its reference weight, and its subjects averaged 74 kg). The authors tested weight on every other parameter and found no effect, so clearance does not change with weight: a heavier patient has more tissue to fill but eliminates methadone no faster. With *Adjust weight to fat-free mass* ticked the volume term uses the fat-free-mass weight; unticked, total body weight. Age, sex and race had no effect.

### CYP2B6

Methadone is cleared mainly by CYP2B6-catalysed N-demethylation to EDDP, and CYP2B6 is highly polymorphic. Henthorn's typical values are those of a normal metaboliser, which the authors recommend for practical use. The **CYP 2B6** field in the [Patient Profile](help:patient-profile) moves each enantiomer's clearance away from them, using the study that measured the genotype effect directly in these same volunteers: Kharasch and colleagues (*Anesthesiology* 2015;123:1142-1153) gave intravenous and oral methadone to cohorts of \*1/\*1 (21 subjects), \*1/\*6 (20) and \*6/\*6 (17) healthy volunteers.

Their hepatic clearances in the poor metabolisers were 0.615 of the normal metaboliser's for S(+)-methadone and 0.769 for R(-)-methadone. Those are the multipliers here, and they are applied to **non-renal clearance only**, because that study found renal clearance unaffected by genotype. Methadone's hepatic extraction ratio is low, 0.08 in normal metabolisers and 0.05 in poor ones, so it is restrictively cleared: hepatic clearance tracks the amount of enzyme almost one for one, and there is no flow-limited reserve to absorb the loss of an allele.

The intermediate metaboliser's multipliers, 0.701 and 0.835, are not measurements. Kharasch reported no hepatic clearance for the heterozygotes, so their effect is set to the fraction of the homozygote effect his apparent oral clearances showed. That fraction depends on which measure it is taken from: 0.78 from the percentages in his abstract, which are the values used here, 0.64 from the oral clearances in his Results, and 0.56 from the systemic clearances, which is almost exactly linear in allele count. Apparent oral clearance amplifies the genotype effect, because it falls both with the clearance and with the rising bioavailability that less first-pass metabolism gives. The most conservative of the three is kept, and a single-dose study understates the steady state in any case, since CYP2B6 is less inducible in carriers of the reduced-function allele and methadone induces its own clearance. The table above gives the resulting clearance and terminal half-life at each phenotype; racemic clearance falls to 0.80 of normal in an intermediate metaboliser and 0.74 in a poor one.

The rapid and ultrarapid categories carry the **normal** metaboliser's clearance. Kharasch does report figures for his \*4 carriers, including systemic clearances of 2.4 and 2.7 mL/kg/min against 1.5 for S(+) in normal metabolisers, but they cannot set a multiplier for a phenotype. That group is four subjects, pooling one \*1/\*4 with three \*4/\*6, which CPIC classes as two different phenotypes, and the paper calls the differences numerically greater without testing them.

Two limits are worth stating plainly. First, the genotype here scales systemic clearance, so it moves the oral and intravenous curves by the same proportion; Kharasch found the genetic influence larger by mouth, through first pass, and that is not represented. Second, CPIC's guideline for CYP2B6 and methadone (Robinson and colleagues, *Clin Pharmacol Ther* 2024;116:932-938) recommends standard dosing, titration and monitoring at every phenotype, because the exposure difference has not been shown to change analgesia, treatment outcome or QTc prolongation. The field shows a measured pharmacokinetic difference; it is not a dosing rule. Genotype is also only part of CYP2B6 activity, which is induced by rifampin, carbamazepine, phenobarbital, efavirenz and by chronic methadone itself. See [the methadone genotype scenario](scenario:methadone-cyp2b6).

### Effect site

The time to peak effect is 11.3 minutes, kept from the earlier model, as Henthorn's study measured no effect. The combination is unusual: an effect site that equilibrates within minutes, like fentanyl's, attached to a disposition that takes days to clear. A dose of methadone works quickly and then stays. See [the methadone scenario](scenario:methadone-bolus).

### MEAC and typical concentrations

MEAC is 60 ng/mL of racemic methadone (0.06 mcg/mL in the plotted units); the shaded band is 48 to 120 ng/mL.

### Where to be careful

The study population was young healthy volunteers, 50 to 100 kg, taking no drugs that affect CYP2B6 or CYP3A4. Inducers (rifampin, carbamazepine, phenytoin, efavirenz) can more than double methadone clearance and inhibitors can reduce it, and none of that is represented. Interindividual variability in clearance was 36% for R(-)- and 67% for S(+)-methadone. QT prolongation (mostly an S(+) effect) and NMDA antagonism are pharmacodynamic and not represented. Children are an extrapolation.

### Pregnancy

With **Pregnant** ticked, the clearance of both enantiomers is multiplied by 24/14 = 1.71 before the racemic reduction above is fitted again. The review the library follows (Ansari, Carvalho, Shafer and Flood, *Anesth Analg* 2016;122:786-804) reports an elimination half-life of 8 to 20 hours in pregnancy against 24 hours when not pregnant, from induction of CYP3A4 and CYP2B6, so that methadone maintenance needs a higher dose and a shorter interval by the third trimester; the factor is the nonpregnant half-life over the midpoint of that range, applied to every clearance component alike, with the volumes unchanged. See [Pregnancy](help:models/pregnancy).

The two covariates multiply: pregnancy scales all three clearance components, and the CYP2B6 phenotype then scales the non-renal part. A pregnant reduced-function metaboliser is the softest corner of this model. The review attributes pregnancy's higher methadone clearance partly to induction of CYP2B6 itself, and CYP2B6 is less inducible in carriers of the reduced-function allele, so the product of the two factors probably overstates her clearance. No source quantifies that interaction, so none is applied.
