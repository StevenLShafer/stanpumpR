## What to look for

Sevoflurane 1.5 per cent in 4 L/min of oxygen with a remifentanil bolus and infusion. On the **MAC equivalents** panel the sevoflurane counts for well over its raw fraction of MAC, because *Include opioid - MAC interaction* is ticked: the remifentanil (about 1.6 ng/mL at steady state, 1.6 MEAC) reduces the MAC in force by about 40 per cent, and 1.5 per cent sevoflurane is a larger multiple of a smaller MAC.

The **% MEAC** panel shows the opioid level, U, that drives the reduction. Hover on it and on the MAC-equivalents panel at 30 minutes; the relation between them is the sigmoid under [Opioid reduction of MAC](help:models/opioid-mac).

The sevoflurane concentrations themselves do not change. Only the MAC they are compared with does.

## Try next

- Untick *Include opioid - MAC interaction* in Graph Options. The MAC-equivalents line drops to the raw alveolar fraction of the age-adjusted MAC.
- Double the remifentanil infusion to 0.2 mcg/kg/min. The reduction grows, but less than proportionately: the model has a ceiling of 90 per cent.
- Stop the remifentanil at 30 minutes (rate 0). The MAC equivalents fall over the next few minutes as the opioid leaves, with the sevoflurane unchanged.

## Background

This is an **approximate model**, a rough fit to nine published points that disagree with one another, and it is expected to be replaced; [Opioid reduction of MAC](help:models/opioid-mac) shows the comparison with the data. Models: [sevoflurane](help:drugs/sevoflurane), [remifentanil](help:drugs/remifentanil).
