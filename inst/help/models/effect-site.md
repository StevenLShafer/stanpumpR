Drug effect lags the plasma concentration. After a propofol bolus the plasma concentration peaks within seconds, yet the patient closes their eyes a minute or more later; as the plasma concentration falls, the effect is still deepening. The effect site is the model of that lag.

## The effect compartment

Sheiner, Stanski and colleagues proposed in 1979 a hypothetical **effect compartment**, linked to the plasma by a first-order rate constant and receiving a negligible amount of drug. Its concentration Ce follows the plasma concentration Cp by

```
dCe/dt = ke0 (Cp - Ce)
```

ke0 is the rate constant for equilibration between plasma and effect site. Its half-time, ln(2)/ke0, is how long Ce takes to close half of any gap with Cp. A drug with a short equilibration half-time (remifentanil, about a minute) tracks the plasma closely; a drug with a long one (morphine, tens of minutes) lags far behind, and its effect-site concentration is smoother, lower in peak and later than its plasma concentration.

Because the effect site adds one more first-order process, the effect-site concentration after a bolus is a sum of **four** exponentials: the three disposition exponents and ke0.

## Why the effect site matters more than the plasma

The plasma concentration is what a blood sample measures; the effect-site concentration is what the patient responds to. Clinical correlates, the concentration for loss of consciousness, for adequate analgesia, for return of neuromuscular function, are effect-site concentrations. This is why stanpumpR draws the effect site solid and hides the plasma by default, and why the shaded therapeutic band and the recovery thresholds apply to the effect site.

## Time to peak effect, and how ke0 is found

ke0 cannot be measured directly. It is estimated from the time course of a measured effect, usually the processed EEG, during a bolus or a rapid infusion. A model-independent way to report the result is the **time to peak effect** after a bolus, tPeak: the moment at which the rising effect-site concentration meets the falling plasma concentration.

Shafer and Varvel showed in 1991 that tPeak is the right quantity to carry from one pharmacokinetic model to another. ke0 itself depends on which disposition model it was fitted with, so taking a published ke0 and pairing it with a different model gives the wrong time course. tPeak does not have that problem. stanpumpR therefore stores **tPeak** for each drug and solves for the ke0 that, with this drug's disposition parameters for this patient, makes the effect site after a bolus peak at exactly tPeak. The search is a one-dimensional optimisation in `getDrugPK()`, repeated for every patient because the disposition parameters change with the covariates. Each drug's page shows the resulting ke0 and its half-time at six reference patients, and the tPeak it was solved from.

| Drug | tPeak (min) | Source noted in the code |
|---|---|---|
| propofol | 1.6 | Schnider 1999 |
| remifentanil | 1.6 | opioid simulation spreadsheet |
| alfentanil | 1.4 | Shafer/Varvel t_peaks analysis |
| fentanyl | 3.7 | Shafer/Varvel t_peaks analysis |
| sufentanil | 5.8 | Shafer/Varvel t_peaks analysis |
| morphine | 93.8 | |
| methadone | 11.3 | |
| hydromorphone | 19.6 | |
| rocuronium | 2.2 | Cortínez 2007 |
| midazolam | 4 | |
| etomidate | 1.6 | |
| ketamine, dexmedetomidine, oxytocin, naloxone | 3, 10, 5, 1 | described in the code as guesses or clinical observation |

The drug pages carry the current values; this table is illustrative. Where the code says a tPeak is a guess, the drug page says so too.

## A drug with no effect site

A tPeak of zero means no effect site: ke0 is left at zero and the effect-site concentration is not computed. Codeine and tramadol do this: they are modelled as prodrugs, and their effect appears on the row of the metabolite formed from them. See [Active metabolites](help:models/metabolites).

For a drug given orally, the time to peak effect is observed after an oral dose, so ke0 is solved against the oral plasma curve rather than an intravenous bolus; hydrocodone and pregabalin are the cases. The time is counted from the dose, so an absorption lag, such as pregabalin's 19 minutes, is part of it. Desmetramadol, which is never dosed directly, supplies its ke0 to the engine ready-solved, because the curve its peak was observed against (the metabolite formed from oral tramadol) is not one the effect-site solver can build.

## What to look for on the plot

Turn the plasma line on (Graph Options) and give a bolus. The plasma falls at once; the effect site rises, crosses the plasma at tPeak, and then falls more slowly than the plasma. A drug with a slow ke0 given as a bolus never achieves an effect-site concentration near its plasma peak: much of the bolus has redistributed before the effect site catches up. [The propofol bolus scenario](scenario:propofol-bolus) and [the opioid MEAC scenario](scenario:opioid-meac) show the two extremes.

## References

Sheiner LB, Stanski DR, Vozeh S, Miller RD, Ham J. Simultaneous modeling of pharmacokinetics and pharmacodynamics: application to d-tubocurarine. *Clin Pharmacol Ther* 1979;25:358-371.

Shafer SL, Varvel JR. Pharmacokinetics, pharmacodynamics, and rational opioid selection. *Anesthesiology* 1991;74:53-63.

Minto CF, Schnider TW, Gregg KM, Henthorn TK, Shafer SL. Using the time of maximum effect site concentration to combine pharmacokinetics and pharmacodynamics. *Anesthesiology* 2003;99:324-333.
