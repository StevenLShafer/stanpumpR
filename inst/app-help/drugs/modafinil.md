### What is plotted

The plasma concentration of **total modafinil, R + S**, in mcg/mL, after oral doses of modafinil (Provigil) as labelled. Modafinil is a 1:1 racemate. Its two enantiomers do not interconvert and are cleared at different rates: R-modafinil (armodafinil, the l-(−) enantiomer) about three times more slowly than S-modafinil (d-(+)). The label says the two have similar pharmacological actions in animals, and both are counted here. No single compartment model can hold two independent systems, so stanpumpR simulates R and S side by side, each given half of every dose, and adds them. The parameter tables above are R-modafinil's; S-modafinil's are under *Parallel systems*. Modafinil acid and modafinil sulfone, the two metabolites that reach appreciable concentrations, are inactive and are not plotted.

### The source

No population model of modafinil could be read in full. Darwish and colleagues (*J Clin Pharmacol* 2012;52:1328-1342), Tao and colleagues (*Ther Drug Monit* 2010;32:189-193) and Seng and colleagues (*Ther Drug Monit* 2011;33:719-729) are paywalled and not yet in the archive. The model is instead taken from the FDA's **Clinical Pharmacology and Biopharmaceutics Review of NDA 20-717** (Provigil; reviewer Rae Yuan, 1997), a scanned document read page by page, which reports the sponsor's enantiomer-specific results.

In **study CEP-2101**, 32 healthy young men took 200, 400, 600 or 800 mg once daily for 7 days, sampled densely on Days 1 and 7 with an enantioselective assay. R-modafinil fitted a one-compartment model with first-order absorption. S-modafinil had too few measurable points to fit, but was absorbed alike (time to peak about 2 hours for both). The means at steady state (Day 7) across 200 to 600 mg are:

| | CL/F (mL/min) | V/F (L) | ka (/h) | half-life (h) |
|---|---|---|---|---|
| R-modafinil | 29 ± 8 | 38 ± 8 | 1.4 ± 0.6 | 16 ± 2 |
| S-modafinil | 100 ± 27 | 39 ± 7 | (R's) | 4.7 ± 0.8 |

Kinetics were linear from 200 to 600 mg per day. The Day 1 means differ slightly (R-modafinil's clearance was 15% higher), and both sets are printed. The Day 7 set is used because once-daily use is the clinical situation and because it reproduces the study's own measurements better. Against the observed mean peak concentrations the model is within −13% to +1% on Days 1 and 7. Against the observed troughs on Days 2 to 7 at all three doses it is −2% on average. The Day 1 set ran 17% low on those troughs.

### Checks against other data

- The **label** (Provigil, 12.3): exposure to R about three times that to S at steady state (the model: 3.4); the trough 90% R-modafinil (the model: 95%); an effective half-life of about 15 hours; a time to peak of 2 to 4 hours (the model: 2.1 hours after one dose); an apparent volume of about 0.9 L/kg.
- The review's **food study** (MOD-022), 12 fasted men given 200 mg once: a mean peak of 3.84 mcg/mL at 2 hours and 0.58 mcg/mL at 24 hours, against the model's 4.26 and 0.98. Those men cleared modafinil faster than CEP-2101's (an area under the curve of 52 against 74 mcg·h/mL for the same dose). The difference is between cohorts, not in the model's shape.

### Oral only

Modafinil is too insoluble to give intravenously, so its absolute bioavailability has never been measured. The parameters are apparent (CL/F, V/F), and only **mg PO** is offered. Food delays the peak by about an hour without changing exposure; that delay is not modelled.

### Body size and other covariates

The published values carry no size covariate, so stanpumpR scales them to fat-free mass ([the fat-free-mass switch](help:models/fat-free-mass)). They are those of CEP-2101's men, who weighed about 78 to 81 kg (implied by the per-kilogram columns). Their age and height are not printed, so the values are not anchored at that cohort. The label reports **no effect of sex**. Clearance is **halved in severe hepatic impairment** (cirrhosis, Child-Pugh B and C), and the label halves the dose. Clearance is about 20% lower at 63 years, and plasma levels were about doubled at 82 years in patients taking many other drugs. None of these is a covariate here, and the Patient Profile has no hepatic field. Severe renal failure did not change modafinil's own kinetics.

Modafinil **induces CYP3A4**. It lowers the concentrations of steroidal contraceptives (the label advises another method of contraception during treatment and for a month after) and of other CYP3A substrates, and it inhibits CYP2C19. Its own trough falls slightly after weeks at 400 mg/day (auto-induction). None of these interactions is modelled.

### Effect site and typical range

No effect site: the curve is plasma only. The shaded band, **1.0 to 1.7 mcg/mL**, is the therapeutic reference range of the AGNP consensus guidelines' 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5, p. 204, doi [10.1055/a-2860-7861](https://doi.org/10.1055/a-2860-7861)). It applies to the **trough, 24 hours after a dose of 200 mg/day at steady state** (reached in about 3 days), with a laboratory alert level of 3.4 mcg/mL and a level of recommendation of 3. It comes from a pharmacokinetic study of healthy Chinese men at steady state (Xu and colleagues, 2008). It describes the concentrations that dose produces, not a window of efficacy: Hart notes that therapeutic concentrations in patients are not known, and that troughs of at least 3 mcg/mL have been suggested to sustain wakefulness and attention. The typical value, 1.35 mcg/mL, is the midpoint. On this model 200 mg once daily gives a steady-state trough of 1.43 mcg/mL. Nothing on this plot is a dose recommendation.

### Pregnancy

No study has measured modafinil's pharmacokinetics in pregnancy (see the registry entry below). The label reports intrauterine growth restriction and spontaneous abortion in association with modafinil and armodafinil, and maintains a pregnancy registry.
