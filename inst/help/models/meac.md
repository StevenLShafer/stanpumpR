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

The drug pages carry the current values and their provenance. Several (oxycodone, oliceridine) are compromises between discordant sources, and the drug pages say so.

## The panel

With *Additional Plots → MEAC* ticked, each opioid's effect-site concentration is plotted as a percentage of its MEAC, and a **total opioid** line adds them: a patient at 60 per cent of a fentanyl MEAC and 60 per cent of a morphine MEAC is taken to be at 120 per cent, as if the two were a single opioid. This assumes the opioids act additively at the same receptor, which is a reasonable first approximation for μ agonists.

The same sum, as a fraction rather than a percentage, is the opioid "U" that drives the [propofol-opioid interaction](help:models/interaction) (through remifentanil equivalents) and the [opioid reduction of MAC](help:models/opioid-mac).

## The shaded band and the threshold

For the opioids, the drug library's typical range is expressed in MEACs: 0.8 to 2 MEAC, with 1.2 MEAC as the typical value. The default recovery threshold for *Time until threshold* is 1 MEAC: the moment analgesia is expected to become inadequate (or, for a ventilated patient, the moment spontaneous ventilation is expected to resume, which is near the same concentration).

## What to look for

[The opioid MEAC scenario](scenario:opioid-meac) gives morphine 10 mg, hydromorphone 1.5 mg and fentanyl 100 mcg at time zero. On the MEAC panel the fentanyl is briefly far above MEAC and gone within an hour; the morphine takes over an hour to reach its peak and is still near MEAC at four hours.

## References

Austin KL, Stapleton JV, Mather LE. Relationship between blood meperidine concentrations and analgesic response: a preliminary report. *Anesthesiology* 1980;53:460-466.

Gourlay GK, Kowalski SR, Plummer JL, Cousins MJ, Armstrong PJ. Fentanyl blood concentration-analgesic response relationship in the treatment of postoperative pain. *Anesth Analg* 1988;67:329-337.
