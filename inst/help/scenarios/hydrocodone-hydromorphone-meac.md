## What to look for

Hydrocodone 10 mg by mouth at time zero. Two panels appear: hydrocodone, and the **hydromorphone** formed from it, which was not given; see [Active metabolites](help:models/metabolites).

The two concentration axes cannot be compared. Hydromorphone's MEAC is 1.5 ng/mL and hydrocodone's is 12, so a hydromorphone concentration that looks small on its own axis may still matter. *Graph Options → % MEAC on opioid panels* is on, so each panel also carries a thin black line: that opioid's effect-site concentration as a percentage of its MEAC, read against the labels at the right-hand edge. The scale is the **same on both panels**, set by the highest % MEAC either reaches, so the higher of the two black lines is the opioid doing more of the work.

Here that is hydrocodone throughout. It peaks at about 103% of its MEAC at 85 minutes and is still at about 41% at twelve hours. The hydromorphone line barely leaves the floor: it peaks at about 5% of its MEAC at about five hours, which is 6 to 8 per cent of the patient's total opioid effect at six and twelve hours. In this model hydrocodone's analgesia is its own.

Hover over either panel to read its % MEAC at that moment.

## Try next

- Replace the hydrocodone with oxycodone 10 mg PO. The metabolite is now oxymorphone, and its black line rises to about 90% of MEAC at two hours against oxycodone's 155% at 70 minutes. By six hours oxymorphone is about a third of the total opioid effect.
- Tick *Graph Options → Additional plots → MEAC* as well: the % MEAC panel draws the same lines together, with their sum, on an axis of its own.
- Tick *Time until threshold*. It shares the right-hand edge with the % MEAC line, so the % MEAC line goes off.

## Background

[MEAC: comparing opioids](help:models/meac) explains the axis. [Hydrocodone](help:drugs/hydrocodone) records how its hydromorphone formation was calibrated, and why hydrocodone is offered by mouth only.
