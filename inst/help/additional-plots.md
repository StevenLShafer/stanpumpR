The third sidebar panel adds panels beneath the concentration plot. Each is a checkbox.

## MEAC

Plots each opioid's effect-site concentration as a percentage of its **minimum effective analgesic concentration**, and the sum over all opioids present. Several opioids can then be compared on one axis of clinical effect rather than of mass: 100 per cent is the concentration at which a typical patient has adequate analgesia, whichever opioid supplies it.

The MEAC values are in the drug library and on each drug's page. Drugs with a MEAC of zero (the non-opioids) do not appear on this panel. See [MEAC: comparing opioids](help:models/meac).

## Interaction

Models the synergy between propofol and an opioid using the Bouillon response surface for laryngoscopy, and plots the **probability of response** over time. Opioids are converted to remifentanil equivalents through their MEAC first, so any opioid in the library can be used. Three curves are drawn: the probability with both drugs, with propofol alone, and (always one) with opioid alone. The panel needs propofol and at least one opioid in the dose table. See [Propofol-opioid interaction](help:models/interaction).

## Events

Draws clinical events on the time axis and lets you enter them. Click the panel to add an event at that time; double-click to edit the list. The events are:

| Event | Colour |
|---|---|
| Start, Timeout, End | blue |
| Induction, Intubation, Extubation, Emergence | green |
| CPB Start, CPB36 … CPB31, CPB End | magenta shading to blue by temperature, then brown |
| Clamp On / Clamp Off, Tourniquet On / Tourniquet Off | red / brown |
| Other | orange |

Most events are annotations. The cardiopulmonary-bypass events (CPB Start, the temperature steps, CPB End) also **change the pharmacokinetics** of any drug whose model has parameters for them; at present that is the infant dexmedetomidine model. See [Events that change the kinetics](help:models/pk-events).

Removing the Events panel while events are entered asks for confirmation, because the events would be lost.

## Panels that appear on their own

Two panels are not checkboxes. The **MAC equivalents** panel appears whenever a potent inhaled agent (sevoflurane, isoflurane, desflurane or nitrous oxide) is running, and the **oxygen** panel whenever any gas flow is. See [Inhaled anesthetics](help:inhaled-agents).
