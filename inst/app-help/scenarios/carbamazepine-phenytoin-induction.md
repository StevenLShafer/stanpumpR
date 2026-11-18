## What to look for

Carbamazepine 400 mg twice daily from the start, with phenytoin 300 mg a day added on day 21. **Include antiseizure drug interactions** is ticked under Graph Options (the scenario ticks it).

Before phenytoin is added, carbamazepine sits near 7.5 mg/L (already autoinduced; see [the autoinduction scenario](scenario:carbamazepine-autoinduction)). When phenytoin starts, carbamazepine falls over about two weeks to around 5.7 mg/L — phenytoin induces the CYP3A4 that clears carbamazepine, on top of carbamazepine's own autoinduction, so its clearance rises a further 42% (Graves 1998). Phenytoin itself climbs to its own steady state over the same period.

The fall is gradual because, again, it is enzyme turnover: the new phenytoin reaches its own steady state slowly (phenytoin's half-life is days), and the induced enzyme builds over a 3.6-day half-life on top of that.

## Try next

- Add phenobarbital as a third drug. Carbamazepine falls further; the two inducers' effects add in their excess, not their product (Graves estimated phenytoin-plus-phenobarbital × 1.62, which the additive rule reproduces as × 1.59).
- Stop the phenytoin after it has taken effect and watch carbamazepine climb back over the following two weeks as the induction fades.

## Background

[Antiseizure drug interactions](help:models/antiseizure-interactions) describes how a second inducer combines with autoinduction. The phenytoin-on-carbamazepine factor is from Graves 1998, the library's own carbamazepine source.
