*A commentary for the [Open TCI initiative](https://opentci.org), by Steven L. Shafer. The history behind it is in [From CATIA to stanpumpR](help:history).*

The data we pooled were meant to outlive the models we first built from them. stanpumpR is one place where that has happened.

## Why the data were shared

When Thomas Schnider and Charles Minto built their models of propofol (1998, 1999) and remifentanil (1997) at Stanford, they did what every group did then: fitted the data they had collected, published the models, and moved on. Those models went into the target-controlled infusion pumps, where most of them still are.

I uploaded all of the data from those studies to the Open TCI initiative, along with further data from my laboratory at Stanford. Other investigators contributed theirs. The idea was simple. A model is limited by the population that one group happens to study. Pooled, the data could support better models than any of us could build alone, and we hoped someone would come along and build them.

## Someone did

Douglas Eleveld built general-purpose models of both drugs from that pool: propofol from 30 studies and 1,033 people, from premature neonates to the elderly and from 0.68 to 160 kg (*Br J Anaesth* 2018), and remifentanil from a pool that includes Minto's data (*Anesthesiology* 2017). Both rest primarily on the same Stanford data as the Schnider and Minto models. They are not rivals to those models. They are their successors: the same science on a larger set of data, extended to the children, the elderly and the obese whom the Stanford volunteers did not include.

## Where the original models fail

The Schnider and Minto models take body size from the James lean body mass (1976), with weight and lean body mass in kg and height in cm:

```
men:   LBM = 1.10 weight - 128 (weight/height)²
women: LBM = 1.07 weight - 148 (weight/height)²
```

At a given height this is a parabola in weight. It peaks at a body mass index of 43 in men and 36 in women, whatever the height, and then falls, so a heavier patient is credited with less lean tissue. Thomas Bouillon and I described this in 1998. It is why the pumps limit these models to a BMI below 42 in men and 35 in women. Eleveld's models scale to the Al-Sallami fat-free mass, which rises with weight at every weight.

<!-- generated: james-lbm-figure -->

## What stanpumpR is

stanpumpR is an open-source descendant of STANPUMP, the program my laboratory wrote at Stanford from 1987 to 1997. STANPUMP drove infusion pumps. stanpumpR does not: it is a free web simulator, written in R, that shows the expected plasma and effect-site concentrations from any dosing regimen, with a [library of drugs](help:drugs/index) and [teaching scenarios](help:scenarios/index). It is released under the MIT License.

Its own propofol and remifentanil models are Eleveld's. A single checkbox runs the Schnider and Minto models beside them, so a clinician can see what the pumps compute, where it agrees with Eleveld, and where it does not ([The Minto and Schnider models](help:models/minto-schnider)). In a 70 kg man, holding an effect-site target of 3 mcg/mL for an hour, the two propofol pumps differ by about 6 per cent. In a 150 kg woman of 160 cm, under the same target, the Schnider pump gives 1,665 mg of propofol against the Eleveld pump's 917. This scenario shows it:

[Obesity: why the Schnider and Minto models fail at the extremes of weight](scenario:james-lean-body-mass)

stanpumpR also simulates [target-controlled infusion](help:tci) with the effect-site algorithm Keith Gregg and I published in 1992, and offers that controller to pump host computers through an open web interface, [the TCI API](help:tci-api).

## In the spirit of Open TCI

Open TCI was built on the belief that pharmacokinetic data belong to the field, not to the group that collected them. stanpumpR rests on the same belief, applied to the models and the software. Both aim at the same two things: to bring the science of clinical pharmacology to the clinicians who give the drugs, and to bring its benefits to their patients.

Contributions are welcome: drugs, data sets, models and algorithms (see [Contributing a drug or a model](help:contributing)). My hope is that each drug in the library will one day be looked after by an investigator who keeps its pharmacokinetics current, and that the pump makers will run the models that the pooled data support.

- The app: [https://stanpumpr.io](https://stanpumpr.io)
- The source: [https://github.com/StevenLShafer/stanpumpR](https://github.com/StevenLShafer/stanpumpR)
- Open TCI: [https://opentci.org](https://opentci.org)

## References

- Bouillon T, Shafer SL. Does size matter? *Anesthesiology* 1998;89:557-560.
- Eleveld DJ, Colin P, Absalom AR, Struys MMRF. Pharmacokinetic-pharmacodynamic model for propofol for broad application in anaesthesia and sedation. *Br J Anaesth* 2018;120:942-959.
- Eleveld DJ, Proost JH, Vereecke H, et al. An allometric model of remifentanil pharmacokinetics and pharmacodynamics. *Anesthesiology* 2017;126:1005-1018.
