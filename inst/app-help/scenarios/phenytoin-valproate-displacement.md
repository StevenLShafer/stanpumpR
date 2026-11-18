## What to look for

Phenytoin 300 mg a day in a 70 kg adult, with valproate 500 mg twice daily added on day 21. **Include antiseizure drug interactions** is ticked under Graph Options (the scenario ticks it); both drugs get a second, uninteracted line.

Two things happen at once when valproate starts. **Total phenytoin falls**, from about 10.2 to about 7.2 mg/L, below its 10–20 mg/L band. **Valproate runs low**, around 45 mg/L against the 65 it would reach alone, below its 50–100 range.

The phenytoin fall is the subtle one, and the plot is deliberately showing only half of it. Valproate displaces phenytoin from albumin, so more of the phenytoin in blood is unbound and is cleared and distributed as free drug — the total, which is what the assay and this plot report, falls. But the *unbound*, pharmacologically active phenytoin does not fall; valproate also inhibits phenytoin's metabolism, which pushes the free level back up. So a total phenytoin level that drops into the "subtherapeutic" range after valproate is added can hide an unchanged or even rising free level. With valproate on board, a free phenytoin level, not the total, is the guide to the dose. The valproate fall is simpler: phenytoin induces valproate's clearance.

## Try next

- Remember the library plots total phenytoin only. The model applies the albumin displacement (total down) but not the metabolic inhibition (free up), because only the displacement is quantified in the source; see the help page.
- Give the phenytoin as a loading dose plus maintenance and watch how the valproate interaction, which builds over two weeks (induction), lags behind the displacement, which is immediate.

## Background

[Antiseizure drug interactions](help:models/antiseizure-interactions) explains the total-versus-unbound problem and why only the displacement is drawn. The displacement is from Perucca 1980 (unbound fraction 9.6% to 15.6%); the induction of valproate by phenytoin is from Teixeira-da-Silva 2022.
