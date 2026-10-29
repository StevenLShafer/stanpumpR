Opioids differ in potency by four orders of magnitude: a typical analgesic concentration is about 8 ng/mL for morphine and 0.056 ng/mL for sufentanil. On a concentration axis they cannot be compared. The **MEAC panel** compares them on an axis of effect instead.

## MEAC

The **minimum effective analgesic concentration** (MEAC) is, in the studies that defined it, the concentration at which a patient in moderate postoperative pain has just been titrated to adequate relief. Austin, Stapleton and Mather characterised it for pethidine in 1980, and Gourlay and others studied fentanyl, morphine and other opioids with patient-controlled analgesia. Those studies measured two different things, which are easily confused:

- the **minimum effective concentration** (MEC), sampled when pain returns, often immediately before a patient-controlled demand; and
- the **MEAC** proper, sampled after titration has restored analgesia, which is higher.

Gourlay's 0.63 ng/mL for fentanyl, for example, is an MEC; Bae and colleagues measured both after abdominal surgery and found a median MEC of 0.72 ng/mL and a MEAC of 0.99. Either is a population-typical value: individual values vary several-fold, and depend on the stimulus.

stanpumpR holds one value per opioid in the drug library, in the column named MEAC. It is the **divisor** the MEAC panel uses, and its provenance differs from row to row: a few are measured values, most are the library's own working values, and some are derived from dose-potency ratios, which are not concentration ratios. The table says which is which.

| Opioid | MEAC | Units | Basis |
|---|---|---|---|
| remifentanil | 1 | ng/mL | Library value; the unit of the interaction panel |
| fentanyl | 0.6 | ng/mL | Library value, near Gourlay 1988's MEC (0.63 ng/mL, blood, before a PCA demand) |
| alfentanil | 39 | ng/mL | Library value |
| sufentanil | 0.056 | ng/mL | Library value |
| morphine | 0.008 | mcg/mL (8 ng/mL) | Library value; Dahlström 1982 measured an MEC of 16 ± 9 ng/mL (plasma) |
| pethidine | 0.46 | mcg/mL | Measured: Austin 1980's MEAC (blood, intramuscular dosing) |
| hydromorphone | 1.5 | ng/mL | Library value |
| methadone | 0.06 | mcg/mL | Gourlay's postoperative value, as quoted by Inturrisi |
| oxycodone | 12 | ng/mL | Library value, set for consistency with morphine; Kokki 2012 measured an MEC of 21 and a MEAC of 45 to 50 ng/mL |
| oliceridine | 27.9 | ng/mL | Measured in healthy volunteers: Dahan 2020's effect-site potency for a 50% rise in cold-pressor tolerance |
| hydrocodone | 12 | ng/mL | Provisional: set equal to oxycodone's, from equianalgesic oral doses |
| oxymorphone | 0.8 | ng/mL | Provisional: a tenth of morphine's, from an intramuscular dose-potency ratio |
| desmetramadol | 84 | ng/mL | Working value: Grond 1999's mean total M1 before PCA demands, during racemic tramadol |

The drug pages give each value's source and caveats in full. The prodrugs codeine and tramadol have no MEAC of their own: their μ-opioid effect is counted on the morphine and desmetramadol rows, and tramadol's monoaminergic analgesia is not counted anywhere. Buprenorphine, a high-affinity partial agonist that does not add to full agonists, has none and is not on the panel. Nor are the opt-in illicit opioids (diamorphine and carfentanil), which are plotted as plasma concentrations only, or naloxone, an antagonist: giving it does not lower the total.

## The panel

With *Graph Options → Additional plots → MEAC* ticked, each opioid's effect-site concentration is plotted as a percentage of its MEAC, and a **total opioid** line adds them: a patient at 60 per cent of a fentanyl MEAC and 60 per cent of a morphine MEAC is taken to be at 120 per cent, as if the two were a single opioid. This assumes the opioids act additively at the same receptor, which is a reasonable first approximation for μ agonists, and that equal multiples of each row's divisor are equally effective, which the mixed provenance of the divisors above does not guarantee. Read the total as an exploratory comparison, not a validated measure of analgesia.

The same sum, as a fraction rather than a percentage, is the opioid "U" that drives the [propofol-opioid interaction](help:models/interaction) (through remifentanil equivalents) and the [opioid reduction of MAC](help:models/opioid-mac). Both were fitted with remifentanil (or, for the MAC reduction, from the published reductions of individual opioids); carrying every other opioid into them through its divisor is stanpumpR's extension, so for any opioid but remifentanil those panels are exploratory too.

## % MEAC on each opioid's own panel

*Graph Options → % MEAC on opioid panels* draws the same percentage as a thin black line on each opioid's own panel, read against labels at the panel's right-hand edge. All the opioid panels share **one** scale: the highest % MEAC any opioid reaches sets its top, rounded up, and each panel maps that scale onto its own height. A line twice as high is twice as many MEACs, whichever panel it is on. This is what shows which moiety carries the analgesia when an opioid forms an active metabolite of very different potency, as oxycodone forms oxymorphone and hydrocodone forms hydromorphone: on their concentration axes the metabolite's curve and the parent's cannot be compared, but their % MEAC lines can. Hovering over an opioid panel adds its % MEAC at that moment. The right-hand edge is also where *Time until threshold* puts its labels, so ticking either turns the other off. See [the hydrocodone scenario](scenario:hydrocodone-hydromorphone-meac).

## The shaded band and the threshold

For the opioids from remifentanil to methadone in the table, the drug library's typical range is expressed in MEACs: 0.8 to 2 MEAC, with 1.2 MEAC as the typical value; the later opioids take their bands from their own sources. The default recovery threshold for *Time until threshold* is 1 MEAC (oxycodone's, 10 ng/mL, a chosen working value, and oliceridine's, 27.4 ng/mL, its respiratory potency, sit a little below theirs). The line is the time until the modelled effect-site concentration falls to that number, roughly when analgesia would be expected to become inadequate. Clinical experience puts the return of spontaneous ventilation in an apnoeic patient near the same concentration, but that is a rule of thumb: the program has no model of ventilation, and for some opioids, methadone among them, respiratory depression outlasts analgesia.

## What to look for

[The opioid MEAC scenario](scenario:opioid-meac) gives morphine 10 mg, hydromorphone 1.5 mg and fentanyl 100 mcg at time zero. On the MEAC panel the fentanyl is briefly far above MEAC and gone within an hour; the morphine takes over an hour to reach its peak and is still near MEAC at four hours.

## References

Austin KL, Stapleton JV, Mather LE. Relationship between blood meperidine concentrations and analgesic response: a preliminary report. *Anesthesiology* 1980;53:460-466.

Gourlay GK, Kowalski SR, Plummer JL, Cousins MJ, Armstrong PJ. Fentanyl blood concentration-analgesic response relationship in the treatment of postoperative pain. *Anesth Analg* 1988;67:329-337.

Bae J, Kwon M, Lee YH, Lee EK, Choi BM, Noh GJ. An allometric pharmacokinetic model and minimum effective analgesic concentration of fentanyl in patients undergoing major abdominal surgery. *Br J Anaesth* 2020;125:976-985.
