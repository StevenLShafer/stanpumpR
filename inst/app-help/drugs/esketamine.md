### The model

Esketamine is S(+)-ketamine, one of the two enantiomers of the racemic ketamine in the library. Its parameters are from Sigtermans and colleagues (*Anesthesiology* 2009;111:892-903, [doi:10.1097/ALN.0b013e3181b437b1](https://doi.org/10.1097/ALN.0b013e3181b437b1)). They gave 20 healthy volunteers (10 men and 10 women, 18 to 30 years) a two-hour intravenous S(+)-ketamine infusion, stepped up to reach 320 ng/mL. Arterial plasma was sampled during the infusion and for three hours after it. The model has three compartments, per 70 kg:

- volumes 15.4, 26.6 and 121 L;
- clearance 75.1 L/h in men and 97.3 L/h in women;
- intercompartmental clearances 80.6 and 146 L/h.

This is the combined S(+)-ketamine and S(+)-norketamine model of the paper's Table 2. Norketamine itself is not drawn.

### Why esketamine is not drawn with the ketamine model

Esketamine given alone is cleared faster than the S(+)-enantiomer in racemic ketamine, because R(-)-ketamine slows the elimination of S(+)-ketamine when the two are given together. The library's racemic model (Domino 1984) has a clearance of 55 L/h at 70 kg. Sigtermans' clearance is 36% higher in men and 76% higher in women. The racemic model would therefore overpredict an esketamine dose by roughly 1.4- to 1.8-fold.

### Covariates

- **Weight.** Every volume and clearance is per 70 kg and scales linearly with weight, as the authors fitted it. With *Adjust weight to fat-free mass* ticked (the default), they are evaluated at the pharmacokinetic weight, which follows fat-free mass; unticked, at total body weight.
- **Sex.** Women's clearance is higher than men's: 30% in this model. The paper's text says 20%, which is the figure from its ketamine-only fit.

Doses are entered as esketamine base, as the products are labelled. The paper gives its doses in mg of S(+)-ketamine without naming the salt.

### Effect site

The effect site equilibrates with a half-time of about 11 seconds (ke0 3.78 per minute). Sigtermans and colleagues did not estimate this. They observed that the effects followed the plasma concentration almost instantly, and agreed with Herd's estimate of 11 seconds (electroencephalogram, racemic ketamine, in children). That literature value is used here. The library's racemic ketamine assumes a time to peak effect of 3 minutes instead.

### Typical concentrations

No band is drawn. The paper's concentration-effect relations are for experimental end points:

| End point | S(+)-ketamine concentration |
|---|---|
| 50% reduction in heat pain intensity | 373 ng/mL |
| 50% rise in electrical pain tolerance | 2.2 mcg/mL |
| 20% rise in cardiac output | 134 ng/mL |

The authors quote at least 800 ng/mL for anesthesia (Schüttler, electroencephalogram). Every volunteer was hypertensive by the end of the infusion, and most were nauseated and dizzy.

### Where to be careful

- The volunteers were young and healthy and received a single slow infusion with no bolus.
- Norketamine, which accumulates during long infusions, is not modelled.
- Intravenous esketamine has no US prescribing information. Spravato, the US product, is a nasal spray, and this model does not describe nasal absorption.
- No study has measured esketamine pharmacokinetics in pregnancy.
