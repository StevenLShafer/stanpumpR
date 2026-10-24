## What to look for

Carbamazepine 400 mg twice daily from the first dose, nothing else. Tick **Include antiseizure drug interactions**; the lighter *carbamazepine (no interactions)* line is the library's ordinary model, which is the *induced*, maintenance state.

With the interaction on, carbamazepine climbs fast over the first few days, overshooting to about 9.5 mg/L around day 4, then settles back to about 7.5 mg/L over the following two weeks. The overshoot and fall are autoinduction: carbamazepine induces the CYP3A4 that clears it, roughly doubling its own clearance, so the level that a naive liver would reach is about twice the eventual steady state. The library's plain model starts already induced and climbs straight to 7.5 without the hump.

This is why a single dose of carbamazepine, or the first few days of therapy, behaves so differently from maintenance: the half-life falls from 35–65 hours when naive to about 15 hours once induced.

## Try next

- Halve the dose to 200 mg twice daily. The autoinduction is smaller (it depends on the carbamazepine concentration, as Kudriakova 1992 found), so the overshoot is less pronounced.
- Add phenytoin on day 21. Carbamazepine, already fully autoinduced, is induced further and falls again over the next two weeks.

## Background

[Antiseizure drug interactions](help:models/antiseizure-interactions) describes how the library's induced Graves 1998 model is run backwards to the uninduced state and re-induced through the turnover equation. The doubling of clearance is from Bertilsson 1986.
