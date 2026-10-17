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

- **Time units.** A *Time units* selector in the Time panel (minutes, hours, days, weeks) sets the unit that times are typed in and the time axis is labelled in, and the Max time choices that go with it (up to a year). Changing it converts the dose table, so the doses stay where they were. *Actual time* is offered for minutes and hours. Target-controlled infusions and inhaled agents are simulated on plots of 7 days or less. See [Time display](help:time-display).
- **Amiodarone.** Long-term oral amiodarone and its active metabolite desethylamiodarone (Pollak, Bouillon and Shafer 2000), given as a constant daily oral rate (`mg/day PO`), with three long-term scenarios; see [Amiodarone](help:drugs/amiodarone).
  Acute intravenous amiodarone is a separate entry, [Amiodarone IV](help:drugs/amiodaroneIV) (Korth-Bradley and colleagues 1996), for the first one to three days only, with [the label's 24-hour loading regimen](scenario:amiodarone-iv-loading) as a scenario.

- **Oxycodone.** A new model pooled from five studies replaces the Lamminsalo model. Clearance falls with age and renal function, oral absorption and an 11-minute effect-site half-time come from Lalovic 2006, and oxycodone can now be given intravenously (`mg`) as well as by mouth. See [Oxycodone](help:drugs/oxycodone).
- **The TCI API.** A stateless web service, `/tci/v1`, hands a real TCI pump its next rate from the pump's own record of what it has delivered, using the same controller as the *Plasma target* and *Effect site target* rows, with a correction for the delay before the pump acknowledges a new rate, a pump ceiling, and a fallback rate with a hard stop for a command that arrives late. It is not part of the shinyapps.io deployment and is served separately. A simulated pump built to it, [FauxTCI](https://stevenlshafer.github.io/FauxTCI/), runs in the browser. See [The TCI API](help:tci-api).
- **Research models of drugs of abuse.** The opt-in *Show illicit drugs* group now holds cocaine, methamphetamine, LSD, psilocybin, MDMA and GHB beside diamorphine. Each is plasma only, and each offers only the routes whose input model the human evidence actually identifies: a route with a bioavailability but no absorption model, or one the engine cannot represent at all, is recorded and refused rather than guessed at. MDMA's published zero-order depot filling is discretised, and its single-dose calibration stated, because it inhibits its own metabolism. GHB is the first drug here to need a new engine rather than new parameters: it is two compartments with saturable elimination, so a second gram adds more than the first did, and its dose is the salt while its curve is the acid, 21% apart, which is why its unit says `oxybate`. Δ9-THC is still deliberately left out, for a stated reason. See [Research models of drugs of abuse](help:models/illicit-drugs).
- **More since then.** A startup menu that asks which drugs to display, grouped by category ([Quick start](help:quick-start)); doses repeated `qd`, `bid`, `tid` or `qid` to the end of the plot, and new routes and forms: regional-anesthesia (`RA`) depots for the local anesthetics, sublingual buprenorphine, oral liquids and extended-release (`XR`) forms given as timed pulses, and oral bioavailability that changes with the dose ([Units](help:units), [Absorption](help:models/absorption)); a **Minto / Schnider** switch that runs the pump models beside the library's ([The Minto and Schnider models](help:models/minto-schnider)); an opt-in *Show illicit drugs* group of research models; many new drugs ([All drugs](help:drugs/index)); and a simulation web service, `run_api()`, that takes a dose table and covariates and returns the concentrations (described at [stanpumpr.io/api](https://stanpumpr.io/api) and under [The Simulation API](help:simulation-api)).

## Now active

**CYP 2D6** in the Patient Profile is now active: it scales the formation of the active metabolites of codeine, tramadol, hydrocodone and oxycodone across the poor, intermediate, normal and ultrarapid phenotypes, and the clearance of aripiprazole, haloperidol and risperidone. Beside it, **CYP 2C19** is read by citalopram and escitalopram, and **CYP 2C9** by meloxicam and phenytoin.

**Pregnant** in the Patient Profile is now active (October 2026): it applies the changes of late pregnancy from the review of Ansari, Carvalho, Shafer and Flood (*Anesth Analg* 2016;122:786-804) to eight drugs: the five for which the review gives a figure (morphine, remifentanil, oxycodone, methadone and dexamethasone), and three renally cleared antibiotics (cefazolin, gentamicin and vancomycin), which take its 50% rise in glomerular filtration. It changes nothing else. See [Pregnancy](help:models/pregnancy), which also lists the drugs the review describes without a figure, so that nobody mistakes an unchanged curve for a studied one.

## No longer a placeholder

Nothing in the Patient Profile is now visible but inactive. The **Serum creatinine** field is live: cefazolin, empagliflozin, ertugliflozin, gabapentin, gentamicin, lacosamide, levetiracetam, mannitol, intravenous meloxicam, metformin, oxycodone, pregabalin, sugammadex and vancomycin estimate renal function from it (see [Cautions](help:cautions)), or from an assumed normal creatinine when it is blank. Renal function also governs the glucuronide metabolites of morphine and hydromorphone, which are not yet modelled.

## Provisional values flagged in the code

Several parameters in the newly added drugs are explicitly provisional and carry no citation yet, as their pages and the source files say: the oral time to peak effect of hydrocodone, the time to peak effect and potency of oxymorphone, the effect-site rate constant of desmetramadol, and the minimum effective concentrations of hydrocodone (set equal to oxycodone's) and oxymorphone (scaled from morphine's). These are marked in the code so they are replaced rather than trusted.

An audit of the minimum effective concentration (MEAC) and time to peak effect (tPeak) of every opioid in the library was done in October 2026 against digests of the cited papers. It compared each MEAC with the effect-site peak after equianalgesic doses and with accepted potency ratios. Hydrocodone's MEAC moved from morphine's 8 ng/mL to oxycodone's 12, its clearest outlier. Oxycodone's 12 ng/mL was kept, for consistency with morphine's. Four effect sites were corrected: methadone (10 minutes, Kharasch's peak pupil constriction, within Inturrisi's range for analgesia; the 11.3 minutes had been attributed to Inturrisi but is not in his paper), oliceridine (4.4 minutes; its 15 minutes had been a respiratory equilibration half-time), and buprenorphine and naloxone, whose published ke0 values are now carried as times to peak. Fentanyl's and sufentanil's tPeaks now match Shafer and Varvel's figure (3.6 and 5.6 minutes). Still open: morphine's MEAC (8 ng/mL) and tPeak (93.8 minutes), and hydromorphone's and pethidine's, have no primary source among the archived papers. Morphine's MEAC is at the low end of its literature (16 ng/mL, range 6 to 33, in Dahlström's postoperative study), and the oxycodone, hydrocodone and oxymorphone values are tied to it.

## Longer-term intentions

From the project README:

1. Oral opioids with good pharmacokinetics for each; only first-order absorption is currently supported.
2. Improved models of pediatric pharmacokinetics.
3. Improved models of drug interaction.
4. Pharmacokinetic changes with renal function beyond the drugs that read the creatinine, and with pregnancy beyond the eight drugs the Pregnant field adjusts.
5. Expanding the in-app help: more teaching scenarios, and screenshots.

A second-generation metabolite link (morphine-6-glucuronide from codeine, for example) would need a two-stage cascade, which the engine does not yet do.

## Known issues in the code

The per-drug simulation cache, once recorded here as not persisting between reactive updates, has been fixed: an edit now re-simulates only the drugs whose inputs changed, and the others reuse their stored simulation (`docs/architecture.md`).
