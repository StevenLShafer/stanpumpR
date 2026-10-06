*Time until threshold* answers the question a clinician asks at the end of a case: if I stop the drug now, how long until the patient recovers? It is drawn as a thin black line on each panel, read against the minute labels on the right-hand axis, when **Graph Options → Time until threshold** is ticked. At each moment t, the line's height is the time it would take, starting at t with delivery stopped, for the effect-site concentration to fall to the drug's **recovery threshold**.

## The thresholds

Each drug's threshold (`endCe` in the drug library) is the effect-site concentration at which recovery is expected. The defaults are each opioid's MEAC (the concentration at which analgesia is expected to become inadequate, and, near enough, at which spontaneous ventilation returns in a patient who has been apneic), 1 mcg/mL for propofol, 1 mcg/mL for rocuronium, and the lower end of the typical range for most other drugs. For the inhaled agents it is 0.1 of the age-adjusted MAC. All of them can be edited under **Settings → Drug Thresholds**; the edited values travel with the URL.

## How it is computed for the intravenous drugs

After delivery stops, the effect-site concentration is a sum of exponentials whose amplitudes are the current compartment states:

```
Ce(t) = Σ state_i × e^(-λ_i t)
```

The time until threshold is the time at which this comes down through the threshold for the **last** time, searched over the next 24 hours. The last crossing is the right one because the effect site lags the plasma: straight after a bolus the effect site may still be below the threshold while on its way above it, and the time that matters is when it comes back down. The search brackets the crossing on a grid that starts at three seconds and grows by about 12 per cent a step, then refines it with a root finder. Zero means the concentration is already at or below the threshold and will stay there; 1440 minutes means it is still above the threshold after a day.

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
