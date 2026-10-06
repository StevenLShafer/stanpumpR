This page lists what is being worked on and what is visible but not yet active, so that nobody mistakes a placeholder for a feature. It describes work in progress at the time this help was written (October 2026); check the repository for the current state.

## Visible but not active

**Pregnant, CYP 2D6 and Renal Function** in the Patient Profile are disabled. No model in the current library uses them. The CYP2D6 field is waiting for the active-metabolite models below.

## Target-controlled infusion

A target-controlled infusion (TCI) pump holds a concentration rather than a rate: you set the plasma or effect-site concentration you want and the pump's pharmacokinetic model computes the infusion needed to reach it quickly and hold it. The algorithm is Shafer and Gregg's (1992), as implemented in the original STANPUMP, and it is being returned to stanpumpR as two new units in the dose table, **Plasma target** and **Effect site target**, for propofol, remifentanil, alfentanil, sufentanil, fentanyl, lidocaine, hydromorphone, etomidate and ketamine.

As drafted: the dose is the target concentration; the controller recomputes the rate every ten seconds; with the effect site targeted it gives the largest bolus that reaches the target without overshoot, then holds the plasma at the target once the effect site is within 5 per cent; a target of 0 stops it; manual boluses are allowed during TCI and manual infusions are not; a TCI rate panel appears below the concentration panels. When it merges, [Suggest Dosing](help:suggest-dosing) will remain as the older way to work backwards from a concentration.

## Active metabolites

Several opioids act partly or wholly through a metabolite. Work in progress adds a **metabolite link** to the engine, so that a parent drug's dose produces a second curve for the metabolite formed from it, and adds the drugs that need it:

| Parent | Active metabolite | Notes as drafted |
|---|---|---|
| codeine | morphine | A prodrug: codeine has no effect site of its own; the analgesia is the morphine's. Formation is CYP2D6-dependent |
| tramadol | O-desmethyltramadol (desmetramadol) | Modelled as a prodrug for its opioid effect; the monoaminergic analgesia of tramadol itself is not represented. Formation is CYP2D6-dependent |
| hydrocodone | hydromorphone | Oral only; CYP2D6-dependent formation |
| oxycodone | oxymorphone | Oxymorphone also as a drug in its own right |

This is the work that will activate the **CYP 2D6** field: poor, normal and ultrarapid metabolisers form the metabolite at different rates. Several times to peak effect in this work are provisional and are marked as such in the code. Hydromorphone's intramuscular and intranasal absorption are revised in the same work.

## Other work in progress

Two further pieces of work were under way on Dr Shafer's own machine when this help was written, concerning **weight adjustment** of dosing and **recovery**. They were not available to read, so nothing is said about them here beyond their existence; this page should be updated when they merge.

## Longer-term intentions

From the project README:

1. Oral opioids with good pharmacokinetics for each; only first-order absorption is currently supported.
2. Improved models of pediatric pharmacokinetics.
3. Improved models of drug interaction.
4. Pharmacokinetic changes with pregnancy, CYP2D6 and renal function.

## Known issues in the code

The developer documentation (`docs/architecture.md`) records that the per-drug simulation cache does not persist between reactive updates, so every drug is re-simulated on every edit rather than only the one that changed. It is a performance issue, not a correctness one, and is noted there so that it is fixed rather than mistaken for intended behaviour.
