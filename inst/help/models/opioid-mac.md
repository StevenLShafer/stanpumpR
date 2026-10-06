Opioids lower MAC: with an opioid on board, less inhaled agent is needed to prevent movement at incision. With **Graph Options → Include opioid - MAC interaction** ticked, the MAC-equivalents panel reports the alveolar concentration relative to the opioid-reduced MAC, so the same end-tidal concentration counts for more. The gas concentrations themselves do not change.

## The model

The opioids present are combined on one scale by dividing each effect-site concentration by that opioid's MEAC and adding, exactly as the [MEAC panel](help:models/meac) does:

```
U = Σ Ce_i / MEAC_i
```

The fractional reduction in MAC is a sigmoid in U,

```
R(U) = Emax × U^γ / (U50^γ + U^γ)
```

so the MAC in force is MAC₀ × (1 − R), and an alveolar concentration worth M multiples of MAC without opioid is worth M / (1 − R) with it. Nitrous oxide needs no separate term, being already summed into the MAC equivalents as a fraction of its own MAC.

| Parameter | Value | Meaning |
|---|---|---|
| Emax | 0.9 | Ceiling: opioids cannot replace the anesthetic entirely |
| U50 | 1.76 MEAC | Opioid level at half the ceiling |
| γ | 1 | Slope |

These give a 50 per cent reduction in MAC at about 2.2 MEAC.

## This is an approximate model

The published studies of opioid MAC reduction do not agree well with one another, and the parameters above are a rough fit to nine published points rather than a formal analysis. They were adopted in October 2026 as a placeholder, and are expected to be replaced. The comparison with the published data, using stanpumpR's own MEAC values, is:

| Study | Opioid, plasma concentration | U (MEAC) | Observed reduction (%) | This model (%) |
|---|---|---|---|---|
| Brunner 1994 | fentanyl 1.67 ng/mL | 2.8 | 50 | 55 |
| Katoh 1999 | fentanyl 3 ng/mL | 5.0 | 61 | 67 |
| Katoh 1999 | fentanyl 6 ng/mL | 10 | 74 | 77 |
| Brunner 1994 | sufentanil 0.145 ng/mL | 2.6 | 50 | 54 |
| Lang 1996 | remifentanil 1.37 ng/mL | 1.4 | 50 | 39 |
| Lang 1996 | remifentanil 32 ng/mL | 32 | 91 | 85 |
| Westmoreland 1994 | alfentanil 28.8 ng/mL | 0.74 | 50 | 27 |
| Sebel 1992 | fentanyl 0.78 ng/mL | 1.3 | 59 | 38 |
| Sebel 1992 | fentanyl 1.72 ng/mL | 2.9 | 67 | 56 |

The model falls short at low opioid levels and for alfentanil and remifentanil, part of which is the MEAC each is scaled by rather than the curve. The parameters are isolated in `R/opioidMacInteraction.R` so that they can be replaced without touching anything else.

## Interaction with *Time until threshold*

When the MAC time until threshold is computed with this interaction on, the opioid's effect is held at its value at the moment the agents are turned off, although in truth the opioid would wear off too. The MAC time is therefore a little long.

## What to look for

[The opioid-MAC scenario](scenario:opioid-mac-interaction) gives 1.5 per cent sevoflurane with a remifentanil bolus and infusion. Untick the interaction and the MAC-equivalents line drops to the raw alveolar fraction of MAC.

## References

Brunner MD, Braithwaite P, Jhaveri R, et al. MAC reduction of isoflurane by sufentanil. *Br J Anaesth* 1994;72:42-46.

Katoh T, Kobayashi S, Suzuki A, Iwamoto T, Bito H, Ikeda K. The effect of fentanyl on sevoflurane requirements for somatic and sympathetic responses to surgical incision. *Anesthesiology* 1999;90:398-405.

Lang E, Kapila A, Shlugman D, Hoke JF, Sebel PS, Glass PSA. Reduction of isoflurane minimal alveolar concentration by remifentanil. *Anesthesiology* 1996;85:721-728.

Westmoreland CL, Sebel PS, Gropper A. Fentanyl or alfentanil decreases the minimum alveolar anesthetic concentration of isoflurane in surgical patients. *Anesth Analg* 1994;78:23-28.

Sebel PS, Glass PSA, Fletcher JE, Murphy MR, Gallagher C, Quill T. Reduction of the MAC of desflurane with fentanyl. *Anesthesiology* 1992;76:52-59.
