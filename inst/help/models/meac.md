Opioids differ in potency by four orders of magnitude: a typical analgesic concentration is about 8 ng/mL for morphine and 0.056 ng/mL for sufentanil. On a concentration axis they cannot be compared. The **MEAC panel** compares them on an axis of effect instead.

## MEAC

The **minimum effective analgesic concentration** is the plasma (here, effect-site) concentration at which a typical patient reports adequate analgesia for moderate postoperative pain. It was characterised in the 1980s by Austin, Stapleton and Mather for pethidine and by Gourlay and others for fentanyl, morphine and other opioids, using patient-controlled analgesia to find the concentration each patient titrated to. It is a population-typical value; individual MEACs vary several-fold, and MEAC for a given patient depends on the stimulus.

stanpumpR holds one MEAC per opioid in the drug library:

| Opioid | MEAC | Units |
|---|---|---|
| remifentanil | 1 | ng/mL |
| fentanyl | 0.6 | ng/mL |
| alfentanil | 39 | ng/mL |
| sufentanil | 0.056 | ng/mL |
| morphine | 0.008 | mcg/mL (8 ng/mL) |
| pethidine | 0.25 | mcg/mL |
| hydromorphone | 1.5 | ng/mL |
| methadone | 0.06 | mcg/mL |
| oxycodone | 12 | ng/mL |
| oliceridine | 27.9 | ng/mL |
| hydrocodone | 12 | ng/mL |
| oxymorphone | 0.8 | ng/mL |
| desmetramadol | 84 | ng/mL |

The drug pages carry the current values and their provenance. Several (oxycodone, oliceridine) are compromises between discordant sources, and several (hydrocodone, oxymorphone, desmetramadol) are provisional or await verification; the drug pages say so. The prodrugs codeine and tramadol have no MEAC of their own: their analgesia is counted on the morphine and desmetramadol rows. Buprenorphine, a high-affinity partial agonist that does not add to full agonists, has none and is not on the panel.

## The panel

With *Additional Plots → MEAC* ticked, each opioid's effect-site concentration is plotted as a percentage of its MEAC, and a **total opioid** line adds them: a patient at 60 per cent of a fentanyl MEAC and 60 per cent of a morphine MEAC is taken to be at 120 per cent, as if the two were a single opioid. This assumes the opioids act additively at the same receptor, which is a reasonable first approximation for μ agonists.

The same sum, as a fraction rather than a percentage, is the opioid "U" that drives the [propofol-opioid interaction](help:models/interaction) (through remifentanil equivalents) and the [opioid reduction of MAC](help:models/opioid-mac).

## % MEAC on each opioid's own panel

*Graph Options → % MEAC on opioid panels* draws the same percentage as a thin black line on each opioid's own panel, read against labels at the panel's right-hand edge. All the opioid panels share **one** scale: the highest % MEAC any opioid reaches sets its top, rounded up, and each panel maps that scale onto its own height. A line twice as high is twice as many MEACs, whichever panel it is on. This is what shows which moiety carries the analgesia when an opioid forms an active metabolite of very different potency, as oxycodone forms oxymorphone and hydrocodone forms hydromorphone: on their concentration axes the metabolite's curve and the parent's cannot be compared, but their % MEAC lines can. Hovering over an opioid panel adds its % MEAC at that moment. The right-hand edge is also where *Time until threshold* puts its labels, so ticking either turns the other off. See [the hydrocodone scenario](scenario:hydrocodone-hydromorphone-meac).

## The shaded band and the threshold

For the opioids from remifentanil to methadone in the table, the drug library's typical range is expressed in MEACs: 0.8 to 2 MEAC, with 1.2 MEAC as the typical value; the later opioids take their bands from their own sources. The default recovery threshold for *Time until threshold* is 1 MEAC (oxycodone's, 10 ng/mL, and oliceridine's, 27.4 ng/mL, sit a little below theirs): the moment analgesia is expected to become inadequate (or, for a ventilated patient, the moment spontaneous ventilation is expected to resume, which is near the same concentration).

## What to look for

[The opioid MEAC scenario](scenario:opioid-meac) gives morphine 10 mg, hydromorphone 1.5 mg and fentanyl 100 mcg at time zero. On the MEAC panel the fentanyl is briefly far above MEAC and gone within an hour; the morphine takes over an hour to reach its peak and is still near MEAC at four hours.

## References

Austin KL, Stapleton JV, Mather LE. Relationship between blood meperidine concentrations and analgesic response: a preliminary report. *Anesthesiology* 1980;53:460-466.

Gourlay GK, Kowalski SR, Plummer JL, Cousins MJ, Armstrong PJ. Fentanyl blood concentration-analgesic response relationship in the treatment of postoperative pain. *Anesth Analg* 1988;67:329-337.
