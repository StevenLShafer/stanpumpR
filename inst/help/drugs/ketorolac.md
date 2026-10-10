### The model

Ketorolac kinetics are from Cloesmeijer and colleagues (*Br J Clin Pharmacol* 2021;87:1443-1454), who pooled 1020 concentrations of the **S and R enantiomers** from 80 subjects given a single intravenous dose: 33 infants (median 10 months), 2 children and 45 adults. They fitted each enantiomer separately, S with three compartments and R with two. Per 70 kg:

| | CL (L/h) | V1 (L) | Q2 (L/h) | V2 (L) | Q3 (L/h) | V3 (L) |
|---|---|---|---|---|---|---|
| S | 3.97 | 4.03 | 1.86 | 43.3 | 19.7 | 5.90 |
| R | 1.45 | 4.43 | 1.90 | 5.18 | | |

Clearances scale with weight to the 0.75 power and volumes linearly. No maturation term was identified and renal function was not tested.

### What is plotted

The curve is **total ketorolac (S + R)** as free acid. No single compartment model can hold two independent systems, so stanpumpR simulates the S and R systems side by side on the same doses and adds them. The parameter tables above are the **S** enantiomer's; the R enantiomer's are under *Parallel systems* above. The S enantiomer carries nearly all the analgesic activity. Its large, slowly equilibrating second compartment gives it a long, low tail (terminal half-time about 24 hours), whose size is uncertain. The R enantiomer's half-times are 0.7 and 5.8 hours.

**Doses are ketorolac tromethamine**, as labelled. Each dose is converted to free acid (255.273 / 376.409) and split equally between the enantiomers, so 30 mg gives 10.17 mg of each.

### Routes

**mg** is intravenous. **mg PO** is **provisional**: it uses the intravenous disposition with complete bioavailability and the 3.8-minute absorption half-time reported by Mroszczak and colleagues (*Pharmacotherapy* 1990;10:33S-39S), treated as first-order. Neither was fitted with this model. The result peaks early and high: 10 mg by mouth peaks at about 1.0 mcg/mL at 12 minutes, while Jung and colleagues' crossover (*Eur J Clin Pharmacol* 1988;35:423-425) found a mean peak of about 0.8 mcg/mL at a mean of 0.9 hours. Food delays absorption further.

### Threshold

There is no effect site and no shaded band. The *time until threshold* level is **0.37 mcg/mL** of racemic ketorolac in plasma, the adult analgesic EC50 that Cloesmeijer and colleagues cite. Because the S:R ratio changes with age, they estimate that an infant needs about 0.41 mcg/mL of racemate for the same S concentration.

### Where to be careful

Ketorolac is cleared by the kidney, and the label limits dosing in renal impairment and the elderly. Renal function is not in the model. Target-controlled infusion is not offered, because the controller inverts a single system. There is no CYP2D6 term, because none was fitted.
