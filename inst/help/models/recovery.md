*Time until threshold* answers the question a clinician asks at the end of a case: if I stop the drug now, how long until the patient recovers? It is drawn as a thin black line on each panel, read against the minute labels on the right-hand axis, when **Graph Options → Time until threshold** is ticked. At each moment t, the line's height is the time it would take, starting at t with delivery stopped, for the effect-site concentration to fall to the drug's **recovery threshold**. A drug with no effect site, such as an antibiotic, is timed on its plasma concentration instead. For the antibiotics the question becomes: if I give no more now, how long until the **free** drug falls below the MIC? The section on the antibiotics below explains this.

## The thresholds

Each drug's threshold (`endCe` in the drug library) is the effect-site concentration at which recovery is expected. The defaults are each opioid's MEAC (the concentration at which analgesia is expected to become inadequate, and, near enough, at which spontaneous ventilation returns in a patient who has been apneic), 1 mcg/mL for propofol, 1 mcg/mL for rocuronium, and the lower end of the typical range for most other drugs. For the inhaled agents it is 0.1 of the age-adjusted MAC. For the antibiotics it is the plasma concentration at which the free drug equals the MIC (see below). The corticosteroids, sugammadex, glycopyrrolate, mannitol and the prodrugs codeine and tramadol have no default threshold, so their line stays at zero. A threshold of zero always means "no threshold": a concentration that decays never reaches zero, so there is nothing to time. All of them can be edited under **Settings → Drug Thresholds**; the edited values travel with the URL.

## Which concentration is timed

- **A drug with an effect site** is timed on its effect-site concentration, because that is where the effect is.
- **A drug with no effect site** is timed on its plasma concentration: the antibiotics, the corticosteroids, sugammadex, glycopyrrolate, mannitol, and the prodrugs codeine and tramadol. Of these, only the antibiotics have a threshold by default.

## The antibiotics: free drug at the MIC

For an antibiotic the threshold is the **MIC** (minimum inhibitory concentration) for the organism the drug is mainly given against in the perioperative setting. Only **free** (unbound) drug acts on the organism, and susceptibility breakpoints and the time-above-MIC target (fT>MIC) are both stated in terms of free drug. So the line answers: **if no more is given, how long until the free concentration falls below the MIC?** That is the time left above the MIC, and so when the next dose is due.

stanpumpR does not simulate protein binding. Each antibiotic's curve shows one fixed thing, and the threshold is placed on that curve accordingly:

- **Cefazolin's** curve is **unbound** drug, because its source model is written on free concentration. Its threshold is the MIC itself.
- **The other antibiotics'** curves are **total** drug, bound plus free, which is what a laboratory reports. Their threshold is the total concentration at which the free concentration equals the MIC, using the unbound fraction measured at that low level. For a highly bound drug this is far above the MIC. Ceftriaxone, for example, is about 93 per cent bound at these levels, so free drug reaches a 1 mg/L MIC only when the total is about 15 mg/L.

| Drug | Target organism | MIC, free drug (mg/L) | Curve shows | Free fraction at that level | Threshold on the curve (mcg/mL) |
|---|---|---|---|---|---|
| Cefazolin | *S. aureus*, methicillin-susceptible | 2 | Unbound drug | (not needed) | 2 |
| Cefalexin | *S. aureus*, methicillin-susceptible | 4 | Total drug | 0.85 | 4.7 |
| Ceftriaxone | Enterobacterales (*E. coli*, *Klebsiella*, *Proteus*) | 1 | Total drug | 0.065 (saturable) | 15 |
| Clindamycin | Staphylococci | 0.5 | Total drug | about 0.10 (saturable) | 5.2 |
| Gentamicin | Enterobacterales (*E. coli*, *Klebsiella*) | 2 | Total drug | 1 (unbound) | 2 |
| Metronidazole | *Bacteroides fragilis* group | 4 | Total drug | 0.96 | 4.2 |
| Vancomycin | *S. aureus*, including MRSA | 1 | Total drug | 0.70 | 1.4 |

Each antibiotic's own page gives the sources. The free fraction is for a typical adult, and binding changes with illness. When albumin is low (critical illness, malnutrition, late pregnancy) the free fraction of the albumin-bound drugs is higher, so at a given total more drug is free; but the extra free drug is also cleared faster, so the total itself falls faster than these models, fitted in healthier people, predict. Clindamycin binds alpha-1 acid glycoprotein instead, which rises after surgery and with inflammation: its free fraction is then lower, and free drug falls below the MIC sooner than the line shows. The MIC is for a susceptible isolate. Against a known isolate with a higher MIC, raise the threshold under **Settings → Drug Thresholds**, multiplying the isolate's MIC by the table's ratio of threshold to MIC.

## Oral, intramuscular and intranasal doses

"Delivery stopped" means no further doses. It does not mean the gut stops absorbing: drug already swallowed or injected has been given and will still arrive. So in the minutes after an oral dose, while the concentration is still rising towards the threshold, the line already shows the time until it will come back down, not zero. Between doses, with nothing more given, the line falls one minute per minute. It reaches zero where the concentration crosses the threshold for the last time. With regular dosing (every 8 or 12 hours, say) the line jumps up at each dose and falls between them. This applies equally when a clinical event changes the kinetics part way through.

## How it is computed for the intravenous drugs

After delivery stops, the effect-site concentration is a sum of exponentials whose amplitudes are the current compartment states:

```
Ce(t) = Σ state_i × e^(-λ_i t)
```

The time until threshold is the time at which this comes down through the threshold for the **last** time, searched over the next 24 hours (the next 7 days for a drug timed on its plasma, such as an antibiotic, whose time above the MIC often runs past a day). The last crossing is the right one because the effect site lags the plasma: straight after a bolus the effect site may still be below the threshold while on its way above it, and the time that matters is when it comes back down. The search brackets the crossing on a grid that starts at three seconds and grows by about 12 per cent a step, then refines it with a root finder. Zero means the concentration is already at or below the threshold and will stay there; 1440 minutes means it is still above the threshold after a day (10080 minutes, after a week, for a drug timed on its plasma).

The calculation is exact and is repeated at every time point, which is why the line can be slow to draw for long simulations with many drugs.

## How it is computed for the inhaled agents

For a gas, "stopping delivery" is defined (by Dr Shafer, for this program) as: turn off this agent's vaporizer or flowmeter, turn the fresh gas flow up so that there is no rebreathing (as is done to wake a patient), and leave ventilation as it is. Each agent is its own decision, so each panel times that agent alone; the MAC-equivalents panel times the sum with every agent turned off. The washout is simulated forward with the full engine, so the coupling between gases is kept: sevoflurane leaves faster if nitrous oxide is turned off at the same moment, because the nitrous oxide on its way out carries it along, but the sevoflurane panel shows the time for sevoflurane alone. See [Inhaled anesthetics](help:inhaled-agents).

One approximation remains for the gases: with the opioid-MAC interaction on, the opioid's effect on MAC is held at its value at the moment the agents are turned off, so the MAC time is a little long.

## Context sensitivity

The line makes the context-sensitive half-time visible. Hughes, Glass and Jacobs showed in 1992 that the time for the concentration to halve after an infusion stops depends on how long the infusion ran: for fentanyl it grows from minutes after a brief infusion to hours after a long one, because the peripheral compartments fill and then return drug to the plasma; for remifentanil it stays at a few minutes however long the infusion. *Time until threshold* generalises this from "halve" to "fall to the threshold", and shows it continuously through the case. [The context-sensitive decrement scenario](scenario:context-sensitive-opioids) runs four opioids for three hours; [the propofol maintenance scenario](scenario:propofol-induction-maintenance) shows the line climbing through a propofol infusion.

## Reading it

- The line rises during an infusion and falls after a bolus has redistributed.
- A horizontal line means a steady state: stopping now or in an hour makes no difference to the recovery time.
- A line that climbs steadily through a case is the signature of a drug accumulating in slow compartments. Stop it earlier than you think.
- The line is unavailable while normalization is on, because the threshold is a concentration and the normalized axis is not.

## References

Hughes MA, Glass PSA, Jacobs JR. Context-sensitive half-time in multicompartment pharmacokinetic models for intravenous anesthetic drugs. *Anesthesiology* 1992;76:334-341.

Shafer SL, Varvel JR. Pharmacokinetics, pharmacodynamics, and rational opioid selection. *Anesthesiology* 1991;74:53-63.
