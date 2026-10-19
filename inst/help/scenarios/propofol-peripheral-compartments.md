## What to look for

Propofol 2 mg/kg (140 mg) at time zero and 100 mcg/kg/min for an hour, in a 70 kg, 40-year-old man. *Graph Options → Peripheral compartments* is on, so under the propofol panel, which is the plasma (V1), come two more: **V2**, the rapidly equilibrating compartment, and **V3**, the slowly equilibrating one. Both are in mcg/mL, so they read on the same scale as the plasma. See [Peripheral compartments and the depot](help:models/compartments).

**V2 follows the plasma.** Its return rate constant k21 is about 0.073 per minute, a half-time of under ten minutes, so by ten minutes it holds 2.6 mcg/mL against the plasma's 2.9, and at the end of the infusion 3.1 against 3.2.

**V3 is still filling when the infusion stops.** Its k31 is about 0.0047 per minute, a half-time of about two and a half hours, and its volume, 157 L, is twenty-five times V1's. After an hour it has reached 0.84 mcg/mL, about a quarter of the plasma.

**Then the gradient reverses.** The moment the infusion stops the plasma falls below V2, which starts to give drug back. V3 goes on rising for another twenty minutes, until the falling plasma meets it: it peaks at 0.89 mcg/mL at about 80 minutes, the moment it crosses the plasma (a compartment rises only while the plasma is above it), and passes V2 at about 100. From then on the slow compartment is the reservoir that holds the plasma up: at four hours the plasma is 0.19 mcg/mL and V3 still 0.58.

Hover over either compartment panel to read its concentration at that moment.

## Try next

- Lengthen the infusion to four hours (Max time 8 hours). V3 gets much closer to the plasma before the infusion stops, so there is more drug to give back afterwards, and the plasma falls more slowly: the [context-sensitive half-time](help:scenarios/context-sensitive-opioids) seen from the inside.
- Replace propofol with remifentanil 0.2 mcg/kg/min. Its peripheral compartments fill quickly, but its clearance is so high that the plasma falls in minutes whatever they hold.
- Tick *Normalize to → Peak plasma*: the compartment panels go, because a compartment has no peak of its own to normalize to.

## Background

[The three-compartment model](help:models/three-compartment) gives the equations behind V2 and V3. [Propofol](help:drugs/propofol) gives its parameters.
