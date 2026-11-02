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

**mg** is intravenous. **mg PO** is **provisional**: it uses the intravenous disposition with complete bioavailability (read from the crossovers, not fitted) and the absorption model Mroszczak and colleagues fitted to the mean levels of 12 men given a 10 mg tablet (*Pharmacotherapy* 1990;10:33S-39S): a first-order rate of 11 per hour (an absorption half-time of 3.8 minutes) **together with a 12-minute (0.2 hour) lag**, the two estimated jointly. Neither was fitted with this model's disposition. 10 mg by mouth peaks here at about 1.0 mcg/mL at 24 minutes. Against Mroszczak's own mean levels the curve is close at 40 minutes and 1 hour (2-3% low), but 47% high at 20 minutes and about 16% low at 2-4 hours: this model's central volume, fitted to intravenous doses, is about half that of Mroszczak's oral fit, so the peak is higher and sharper than the observed mean peaks (0.86 mcg/mL at 35 minutes in Mroszczak's study; 0.81 mcg/mL at 0.9 hours in Jung and colleagues' crossover, *Eur J Clin Pharmacol* 1988;35:423-425). That is disposition, which a different absorption rate or lag cannot correct. Jung's volunteers also had a higher exposure after both the tablet and the intravenous dose than this model predicts, a difference in clearance rather than in absorption. Food delays absorption further.

### Threshold

There is no effect site and no shaded band. The *time until threshold* level is **0.37 mcg/mL** of racemic ketorolac in plasma, the adult analgesic EC50 that Cloesmeijer and colleagues cite. Because the S:R ratio changes with age, they estimate that an infant needs about 0.41 mcg/mL of racemate for the same S concentration.

### Where to be careful

Ketorolac is cleared by the kidney, and the label limits dosing in renal impairment and the elderly. Renal function is not in the model. Ketorolac is highly bound to albumin, which falls in pregnancy, so with **Pregnant** ticked the unbound concentration may change less than the total plotted. Target-controlled infusion is not offered, because the controller inverts a single system. There is no CYP2D6 term, because none was fitted.
