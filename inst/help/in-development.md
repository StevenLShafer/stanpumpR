This page lists what is being worked on and what is visible but not yet active, so that nobody mistakes a placeholder for a feature. It describes the state of the repository when this help was last revised (October 2026); check the repository for the current state.

## Recently landed

Five pieces of work that this page previously listed as "in development" have now merged, and each has its own help page:

- **Target-controlled infusion.** Two new dose-table units, *Plasma target* and *Effect site target*, run a simulated TCI pump for propofol, remifentanil, alfentanil, sufentanil, fentanyl, lidocaine, hydromorphone, etomidate and ketamine. See [Target-controlled infusion](help:tci).
- **Active metabolites.** Codeine, tramadol, hydrocodone and oxycodone now form active metabolites, and the drugs needed for them (codeine, tramadol, desmetramadol, hydrocodone, oxymorphone) have been added. See [Active metabolites](help:models/metabolites).
- **Fat-free-mass dosing.** Most models are now scaled to the patient's fat-free mass by default, with a switch to turn it off. See [Scaling to fat-free mass](help:models/fat-free-mass).
- **Antibiotics, corticosteroids and reversal agents.** Sixteen drugs from three literature reviews: cefazolin, cefalexin, ceftriaxone, clindamycin, gentamicin, metronidazole and vancomycin; dexamethasone, hydrocortisone, methylprednisolone, prednisolone and prednisone; and sugammadex, neostigmine and glycopyrrolate, with naloxone moved to the Dowling model and given a nasal route. Most have no effect site and are plotted as plasma concentrations. Each drug's page says what its row plots (unbound cefazolin, free prednisolone and the hydrocortisone increment above baseline are not the usual laboratory measure) and which values are provisional.
- **Time until threshold across a metabolite, and during an absorption lag.** Recovery is now solved from the combined effect-site state when a drug receives an active metabolite, and reads "not yet absorbed" rather than zero during an extravascular lag.

## Now active

**CYP 2D6** in the Patient Profile is now active: it scales the formation of the active metabolites of codeine, tramadol, hydrocodone and oxycodone across the poor, intermediate, normal and ultrarapid phenotypes.

## Visible but not active

**Pregnant** in the Patient Profile is still disabled. The **Serum creatinine** field is live: mannitol, vancomycin, gentamicin, cefazolin and sugammadex estimate renal function from it, or from an assumed normal creatinine when it is blank. Renal function also governs the glucuronide metabolites of morphine and hydromorphone, which are not yet modelled.

## Provisional values flagged in the code

Several parameters in the newly added drugs are explicitly provisional and carry no citation yet, as their pages and the source files say: the oral time to peak effect of hydrocodone, the time to peak effect and potency of oxymorphone, the effect-site rate constant of desmetramadol, and the minimum effective concentrations set equal to or scaled from morphine's for hydrocodone and oxymorphone. These are marked in the code so they are replaced rather than trusted.

## Longer-term intentions

From the project README:

1. Oral opioids with good pharmacokinetics for each; only first-order absorption is currently supported.
2. Improved models of pediatric pharmacokinetics.
3. Improved models of drug interaction.
4. Pharmacokinetic changes with pregnancy and renal function.

A second-generation metabolite link (morphine-6-glucuronide from codeine, for example) would need a two-stage cascade, which the engine does not yet do.

## Known issues in the code

The developer documentation (`docs/architecture.md`) records that the per-drug simulation cache does not persist between reactive updates, so every drug is re-simulated on every edit rather than only the one that changed. It is a performance issue, not a correctness one, and is noted there so that it is fixed rather than mistaken for intended behaviour.
