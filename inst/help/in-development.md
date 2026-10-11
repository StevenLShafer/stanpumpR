This page lists what is being worked on and what is visible but not yet active, so that nobody mistakes a placeholder for a feature. It describes the state of the repository when this help was last revised (October 2026); check the repository for the current state.

## Recently landed

Five pieces of work that this page previously listed as "in development" have now merged, and each has its own help page:

- **Target-controlled infusion.** Two new dose-table units, *Plasma target* and *Effect site target*, run a simulated TCI pump for propofol, remifentanil, alfentanil, sufentanil, fentanyl, lidocaine, hydromorphone, etomidate and ketamine. See [Target-controlled infusion](help:tci).
- **Active metabolites.** Codeine, tramadol, hydrocodone and oxycodone now form active metabolites, and the drugs needed for them (codeine, tramadol, desmetramadol, hydrocodone, oxymorphone) have been added. See [Active metabolites](help:models/metabolites).
- **Fat-free-mass dosing.** Most models are now scaled to the patient's fat-free mass by default, with a switch to turn it off. See [Scaling to fat-free mass](help:models/fat-free-mass).
- **Antibiotics, corticosteroids and reversal agents.** Sixteen drugs from three literature reviews: cefazolin, cefalexin, ceftriaxone, clindamycin, gentamicin, metronidazole and vancomycin; dexamethasone, hydrocortisone, methylprednisolone, prednisolone and prednisone; and sugammadex, neostigmine and glycopyrrolate, with naloxone moved to the Dowling model and given a nasal route. Most have no effect site and are plotted as plasma concentrations. Each drug's page says what its row plots (unbound cefazolin, free prednisolone and the hydrocortisone increment above baseline are not the usual laboratory measure) and which values are provisional.
- **Time until threshold across a metabolite, and during an absorption lag.** Recovery is now solved from the combined effect-site state when a drug receives an active metabolite, and reads "not yet absorbed" rather than zero during an extravascular lag.

Two later changes to *Time until threshold* and the solvers:

- **Time until threshold for the antibiotics.** A drug with no effect site is now timed on its plasma concentration. Each antibiotic's threshold is the plotted concentration at which **free** drug equals the MIC for its main target organism (unbound cefazolin is compared with the MIC directly; total-drug curves use the MIC divided by the free fraction), so the line shows the time left above the MIC. See [Time until threshold](help:models/recovery).
- **Oral doses across clinical events.** The solver used when a clinical event changes the kinetics now absorbs oral, intramuscular and intranasal doses, rather than treating them as infusions. No drug with an extravascular route currently has event-dependent kinetics, so this guards future models.

And for plots longer than a day:

- **Time units.** A *Time units* selector in the Time card (minutes, hours, days, weeks) sets the unit that times are typed in and the time axis is labelled in, and the Max time choices that go with it (up to a year). Changing it converts the dose table, so the doses stay where they were. *Actual time* is offered for minutes and hours. Target-controlled infusions and inhaled agents are simulated on plots of 7 days or less. See [Time display](help:time-display).
- **Amiodarone.** Long-term oral amiodarone and its active metabolite desethylamiodarone (Pollak, Bouillon and Shafer 2000), given as a constant daily oral rate (`mg/day PO`), with three long-term scenarios; see [Amiodarone](help:drugs/amiodarone).
  Acute intravenous amiodarone is a separate entry, [Amiodarone IV](help:drugs/amiodaroneIV) (Korth-Bradley and colleagues 1996), for the first one to three days only, with [the label's 24-hour loading regimen](scenario:amiodarone-iv-loading) as a scenario.

- **Oxycodone.** A new model pooled from five studies replaces the Lamminsalo model. Clearance falls with age and renal function, oral absorption and an 11-minute effect-site half-time come from Lalovic 2006, and oxycodone can now be given intravenously (`mg`) as well as by mouth. See [Oxycodone](help:drugs/oxycodone).

## Now active

**CYP 2D6** in the Patient Profile is now active: it scales the formation of the active metabolites of codeine, tramadol, hydrocodone and oxycodone across the poor, intermediate, normal and ultrarapid phenotypes.

## Visible but not active

**Pregnant** in the Patient Profile is still disabled. The **Serum creatinine** field is live: mannitol, vancomycin, gentamicin, cefazolin, sugammadex, gabapentin, pregabalin and oxycodone estimate renal function from it, or from an assumed normal creatinine when it is blank. Renal function also governs the glucuronide metabolites of morphine and hydromorphone, which are not yet modelled.

## Provisional values flagged in the code

Several parameters in the newly added drugs are explicitly provisional and carry no citation yet, as their pages and the source files say: the oral time to peak effect of hydrocodone, the time to peak effect and potency of oxymorphone, the effect-site rate constant of desmetramadol, and the minimum effective concentrations set equal to or scaled from morphine's for hydrocodone and oxymorphone. These are marked in the code so they are replaced rather than trusted.

A separate audit of the minimum effective concentration (MEAC) and time to peak effect (tPeak) of every opioid in the library is warranted, to find values that are out of line with the others. The opioids' MEACs come from mixed sources (analgesia studies, compromises between discordant reports, values scaled from morphine), and their tPeaks from end points as different as CSF concentration and pupil constriction. Oxycodone's MEAC of 12 ng/mL is held pending that audit.

## Longer-term intentions

From the project README:

1. Oral opioids with good pharmacokinetics for each; only first-order absorption is currently supported.
2. Improved models of pediatric pharmacokinetics.
3. Improved models of drug interaction.
4. Pharmacokinetic changes with pregnancy and renal function.

A second-generation metabolite link (morphine-6-glucuronide from codeine, for example) would need a two-stage cascade, which the engine does not yet do.

## Known issues in the code

The developer documentation (`docs/architecture.md`) records that the per-drug simulation cache does not persist between reactive updates, so every drug is re-simulated on every edit rather than only the one that changed. It is a performance issue, not a correctness one, and is noted there so that it is fixed rather than mistaken for intended behaviour.
