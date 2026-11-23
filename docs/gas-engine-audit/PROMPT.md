# Audit of stanpumpR's inhaled-anesthetic engine

You are one of two independent auditors (the other is a different AI system; you will not see its report, and it will not see yours). Steven L. Shafer, who maintains stanpumpR, has asked for a detailed audit of the engine that simulates the inhaled anesthetics. Your report will be uploaded to Claude, which wrote most of stanpumpR. Claude will check every claim you make against the code and against the original papers before anything is changed. A claim it cannot check costs time and is set aside, so the rules on evidence below matter more than the number of findings.

Work through Parts A to F in order. Part F, what new data should change the model, is the reason for the audit. Parts A to E establish what the model does now.

---

## 1. What stanpumpR is, and what the gas engine does

stanpumpR is an R package and Shiny app that simulates drug concentrations for clinical care, teaching and research. It is advisory: it does not control drug delivery. Its intravenous drugs are solved in closed form. The inhaled agents (nitrous oxide, sevoflurane, isoflurane and desflurane, with oxygen, air, nitrogen and ventilation) have an engine of their own.

That engine follows **GasMan®**, the model James H. Philip built at Brigham and Women's Hospital about 30 years ago. It has five states per gas: the breathing circuit, the alveolar gas, and the vessel-rich, muscle and fat groups. Dr Shafer began from GasMan so that agreement with GasMan's output would validate the whole chain: the parameters, the structure and the arithmetic. stanpumpR therefore carries two implementations:

- `R/advanceGasManBaseline.R` restates GasMan's own integration scheme in R. It is the reference the tests compare against. It is not what the app runs.
- `R/advanceClosedFormGas.R` is the production engine. Within each short step it solves every gas exactly by matrix exponential. The total uptake that couples the gases (the concentration and second gas effects) is held at its value at the start of the step. The production engine has ten deliberate differences from GasMan, listed in `inst/app-help/models/gas-differences.md`.

Since then, stanpumpR has added the following on top of GasMan's model:

- **MAC.** MAC is adjusted for age with Mapleson's 1996 relation. There is one summed MAC-equivalents series.
- **Physiology sliders.** Cardiac output, body temperature, altitude, and haemoglobin (the haemoglobin acts only through the arterial oxygen).
- **Endobronchial intubation.** A checkbox models one-lung ventilation with a 50% shunt.
- **Arterial oxygen.**
- **Physiology events.** Settings can step at any time during the run.
- **An approximate opioid-MAC interaction.**
- **A time-until-threshold calculation.** It computes how long each agent, and the summed MAC, takes to fall to a threshold after delivery stops.

Dr Shafer's question is whether the model built 30 years ago, and validated against GasMan, should now take in what has been learned since.

---

## 2. Materials

### Pinned commits

Every line number you cite must refer to these commits:

- **stanpumpR** at `c90c33e50346380960d5d86c5a991ca1bf35a1ff`:
  https://github.com/StevenLShafer/stanpumpR/tree/c90c33e50346380960d5d86c5a991ca1bf35a1ff
- **GasMan** (GPL-3.0, public) at `d3a2dd36f6be4582cdb7c26ccb860164d9f9983a`:
  https://github.com/rasman/gasmanonline/tree/d3a2dd36f6be4582cdb7c26ccb860164d9f9983a/gasman_api/gasmanAPI
  This is the same commit that `R/advanceGasManBaseline.R` says it transcribed and that `inst/validation/VALIDATION.md` ran against.

### The attachment

The attachment is `stanpumpR-gas-audit-c90c33e.zip`. It contains:

- `stanpumpR/...`: the gas engine and the files around it, with their repository paths.
- `gasman/...`: GasMan's API source. The computation is in `gasman/gasman_api/gasmanAPI/GasDoc.cpp` (`ComputeTerms`, `Calc`, `CalcUptake`, `SetWeight`), and the parameters are in `gasman/gasman_api/gasmanAPI/gasman.ini`.
- `INDEX.md`: every file, with its line count and permalink, and every function and constant, with its line.
- `CORE-NUMBERED.txt`: every file you must read, concatenated, with each line numbered.
- `SUPPLEMENT-NUMBERED.txt`: the remaining stanpumpR files, numbered the same way. These are `AGENTS.md` (the project's rules and recorded decisions), `R/app_server.R`, `R/viewerRun.R`, `R/constants.R`, the drug table, the pregnancy registry, the intravenous physiology page and the larger tests.
- `Eger-Chapter-8-Evolution-of-Pharmacokinetics-2017-07-29.pdf`: see the next subsection.

How to read the files:

- If you can run code, unzip the attachment and use `grep -n`; that is the most reliable way to get line numbers right.
- If you cannot, read `CORE-NUMBERED.txt`. Its line numbers are the files' own: the number before the colon on each line is the line in the file named by the nearest `===== FILE:` or `----- ... continues -----` header above it.

### Ted Eger's chapter

The PDF is the draft of chapter 8 of Edmond I. "Ted" Eger II's scientific autobiography, "The Evolution of Pharmacokinetics" (2017), on which Dr Shafer was co-author and editor. Dr Shafer considers it Eger's final, and best, account of inhaled anesthetic uptake and distribution. Use it as the map of the phenomena a model of uptake and distribution must capture or consciously leave out. Its reference list (89 references) is a starting bibliography.

The text in angle brackets, `<Ted: ...>` or `<...>`, is Dr Shafer's editorial comment to Eger. It is not Eger's text. Do not attribute it to Eger. Some of those comments ask questions that bear on the model, for example the one at the end of "The Concentration and Second Gas Effects" about whether tissue:gas coefficients must also be scaled when λ′ = λ(1 − FI).

### The papers

The papers are in Dr Shafer's Google Drive folder **stanpumpRPapers**, folder ID `1Hi6HmnULQsIlETZw4lo5rhxkfog33vRR`.

- **File names.** PDFs are named `PMID_<pmid>.pdf`. Supplements are `PMID_<pmid>_mmc1.pdf`, `_sup1`, `_ESM1` and so on. Papers without a PMID are named `DOI_<doi>.pdf` or by a short name.
- **Finding a paper.** Find a paper by searching for its file name. Do not try to list the folder: it holds more than 1,100 files, and listing it stops returning new files after about 300.
- **Text extractions.** Text extractions are in its `_build/text` subfolder, under the same name with `.txt`. Two-column layouts are scrambled there, so confirm any number you take from them on the PDF.
- **Summaries.** Many papers also have a summary in the repository at `papers/digests/PMID_<pmid>.md`. Those summaries are Claude's; treat them as leads, not sources.

All the papers should be in that folder. Section 9 lists the ones already found there and the ones that were not. If a paper you need is not in the folder, do not substitute your memory of it: list it under **Papers to add** in your report (section 8), and Dr Shafer will put it there. If you cannot reach the Drive at all, say so at the top of your report and mark every literature claim *not read*.

---

## 3. Rules of evidence

These rules exist because earlier external reviews of this code produced confident claims that were false. They included PMIDs called fabricated that were correct, factors called "exact match" that did not match the code, and functions that do not exist.

1. **Anchor every code finding.** Give the path and line number(s) at the pinned commit, for example `stanpumpR/R/advanceClosedFormGas.R:572-590` or `gasman/gasman_api/gasmanAPI/GasDoc.cpp:840-852`, and quote the exact line or lines you are critiquing. If you cannot find a line, say so; never describe code you have not seen.
2. **Anchor every literature claim.** Give the first author, year, journal, PMID, the page, table or figure, and the number or a quotation of at most 25 words. Say how far you read it: *read in full (PDF)*, *abstract only*, *secondary source* (name the source) or *not read*. A number from memory is marked *UNVERIFIED*, and will be treated as a lead to look for, not as a finding.
3. **Never write "exact match", "verified" or "identical"** unless you compared the two values digit by digit, and show both values.
4. **Quantify.** Where you say something changes what a user sees, estimate by how much: for example, "end-tidal sevoflurane 8% low at 5 min in a 4 kg infant at 3 L/min", or "MAC-equivalents 12% high at 32 °C". Show the arithmetic, or give R code that Claude can run after `devtools::load_all()`. The exported entry point is `simulateGases(doseTable, weight, age, maximum, physiology, temperature, altitude, endobronchial, ...)`; the tests in `tests/testthat/test-gas-*.R` show how to call it. If you cannot run R, say so. You may re-implement a small calculation in Python and give the code.
5. **Do not paste GasMan's code into a proposed change.** GasMan is GPL-3.0 and stanpumpR is MIT. Describe what GasMan does, quoting the lines you compared, and propose stanpumpR code of your own.
6. **Classify every finding** as one of:
   - **DEFECT**: the code does not do what its own comments, help page or tests say. In the GasMan baseline, it does not do what GasMan does.
   - **UNDOCUMENTED DEVIATION**: the production engine differs from GasMan in a way not listed in `gas-differences.md`.
   - **EVIDENCE GAP**: an assumption or parameter that the literature contradicts or does not support.
   - **ENHANCEMENT**: something new the evidence supports adding.
   - **REOPENS DECISION**: a challenge to one of Dr Shafer's recorded decisions (section 4). These are welcome when new evidence warrants them; say which decision and what evidence.
7. **Severity, with the patient and scenario named:**
   - **S1**: changes a number a clinician could act on by 10% or more for a plausible patient and scenario, or breaks conservation of mass or units.
   - **S2**: 1-10%, or only at legal but unusual inputs.
   - **S3**: under 1%, documentation, or clarity.
8. **Say what you did not check.**

---

## 4. Already settled: report only with new evidence

Read `stanpumpR/AGENTS.md` (in the supplement) for the project's rules. Decisions that bear on this audit:

- **GasMan's parameters are kept on purpose.** They are used for now so that the engine can be validated against GasMan (Shafer, 2026-10-05: "for now I want to be identical with GasMan; later on we will update with more current data"). This audit is the "later on". A proposal to change a parameter is welcome, but it must be framed as a documented departure from GasMan: say what it does to the GasMan comparison tests, and how the validation chain is kept. One way is to keep the baseline as GasMan and make the change only in the production engine.
- **The ten differences in `inst/app-help/models/gas-differences.md` are deliberate.** They include the ideal circuit, minute ventilation with 30% dead space, oxygen consumption, MAC by age, summed MAC, always carrying nitrogen, and matrix-exponential steps. Report a difference only if it is not on that list or is not implemented as described.
- **Thresholds for the time until threshold are Dr Shafer's clinical choice.** MAC 0.1; each volatile agent 0.1 × its age-adjusted MAC; nitrous oxide 10% (`R/gasRecovery.R`, header).
- **Cardiac-output slider.** The brain and heart keep 16% of resting output, scaled by cardiac output^0.35. The exponent 0.35 is his decision of 2026-11-14 (`R/physiology.R`, `gasBodyPhysiology()`).
- **Temperature.** Volatile-agent MAC follows Liu 2001. Nitrous-oxide MAC follows Antognini 1994 in rats, applied to people on his decision of 2026-11-16. Temperature is deliberately not coupled to cardiac output or ventilation, which have their own controls (2026-11-16).
- **Endobronchial intubation.** Shunt fraction 0.5 (the chapter's half), with no hypoxic pulmonary vasoconstriction, decided 2026-11-19. The ventilated lung's share of the alveolar volume, 0.5, is still an open decision (`R/endobronchial.R`, header).
- **Altitude.** Flowmeter calibration error at altitude is not modelled; this is an open decision (`R/altitude.R`, header). MAC, as a partial pressure, does not move with altitude.
- **The opioid-MAC interaction is a placeholder.** It was adopted in October 2026 and is expected to be replaced (`inst/app-help/models/opioid-mac.md`). Proposals to replace it are wanted.
- **The program chooses the model, never the physician.** Where the literature offers several models (of MAC by age, or of an interaction), recommend one and say why. stanpumpR will not ask a clinician to choose.
- **No paper by Gideon Koren is cited or used**, in any author position. Check the full author list of anything from the Hospital for Sick Children/Motherisk group.
- **Out of scope:** `cube()`, the intravenous engines, Shiny UI robustness (audited separately), and the greenhouse-gas calculation.
- **Fixed in earlier audits; do not re-report unless the fix is wrong.** See `AGENTS.md`, "Rules from Carbon's third/fourth UI audit" and "From CodeRabbit's review of PR #730". They cover:
  - the closed circuit when fresh gas cannot cover uptake plus CO₂ (`gasCircuitBlend()` supply limit)
  - negative uptake on emergence
  - the washout step for small patients (exponential time differencing)
  - the rebound check in the gas time until threshold
  - a default ventilation that is never zero
  - repeated settings as change points
- **Already noted; one line is enough.** `inst/app-help/models/gas-engine.md` (first paragraph) and the header of `R/gasProperties.R` still say GasMan is closed source and that nothing was derived from its code. The GPL source has since been released, and `R/advanceGasManBaseline.R` restates its algorithm from it.

---

## 5. The audit

### Part A: Does stanpumpR's GasMan match GasMan?

**A1. The baseline against GasMan's code.** Map `R/advanceGasManBaseline.R` (and `inst/validation/gasman_baseline_standalone.R`) to `GasDoc.cpp` line by line, and report every difference. Cover:

- `ComputeTerms`: the effective flows, the precomputed exponentials, the weight factor `fWtFactor` on the alveolar and tissue terms but not on the circuit.
- `Calc`, for each circuit type: Open, Semi-closed, Closed and Ideal.
  - the circuit target, including the emergence term
  - the alveolar target with the "Correct for constant lung capacity" uptake term: `CKT` on induction, `ALV` on emergence
  - the tissue targets
  - the venous lag and the `m_bRtnEnb` switch
  - the vaporisation option `m_bVapEnb` and injections
- `CalcUptake`: which compartments it sums, the `(1 - fExp)` form, and the `!m_bRtnEnb` branch.
- The vernier sub-stepping: `MAX_VERNIER`, `VERNIER_TICKS`, the rejection of a negative tension, and the 10% rule on `fExpCKT`/`fExpALV`.
- `InitDocument`: ventilation and cardiac output scaled by weight^0.75.
- `SetWeight`: the allometric time step, 10·W^0.25/28.92508.
- `applyManualDt`.

`GasDoc.cpp` says its exponential integration ("differential extrapolation") was introduced by Xin Bao for a veterinary adaptation. Establish whether anything in the scheme the baseline restates depends on that adaptation, and whether GasMan's own outputs at this commit differ from the desktop GasMan the literature describes.

**A2. Parameters.** Compare every value in `gasman.ini` with `getGasProperties()` and `getGasBody()` in `R/gasProperties.R`. The values to compare:

- `Lambda`, `VRG`, `MUS`, `FAT` and `MAC` for each agent
- `[Volumes]`
- `[Ratio]`
- `[Defaults]` (VA, CO, FGF, circuit, weight)

Also cover:

- **Tissue:gas or tissue:blood.** Confirm that the tissue coefficients are tissue:gas, as the code asserts; `gasPartitionTissueGas()` and `VALIDATION.md` say why.
- **Agents left out.** Note the agents in `gasman.ini` that stanpumpR omits (halothane, enflurane, ether, xenon), and whether xenon would be worth adding.

**A3. The production engine against GasMan.** Read `advanceClosedFormGas()`, `gasSystemSoluble()`, `gasSystemOxygen()`, `gasCircuitBlend()`, `gasTotalUptake()`, `gasMixedVenous()` and `gasSettingsAt()`.

- Confirm that with the semi-closed circuit, no dead space and no oxygen uptake, the production engine is GasMan's model except for the integration scheme.
- Confirm that the equations printed in `inst/app-help/models/gas-engine.md` (circuit blend `k`, `D`, `f`, `g`; the closed circuit with `R`, `P`, `s`; the alveolar equation; uptake; mixed venous) are the equations the code solves.
- Check conservation of each gas, including nitrogen and oxygen.
- Report any difference from GasMan that is not among the ten.

**A4. Integration error.** The production engine holds the total uptake constant within a step of about `maximum/600`. `gas-engine.md` ("Integration") states the error budget: about 1.5% of value early in a nitrous-oxide wash-in on a one-hour plot, and 13% on a 24-hour plot. Check those claims. Recommend, with evidence, whether to change the scheme. Options include predictor-corrector on the uptake, Strang splitting, or an adaptive embedded Runge-Kutta like the Dormand-Prince pair stanpumpR already uses for saturable drugs (`mmDormandPrince()`).

**A5. The validation record.** Read `inst/validation/VALIDATION.md`, `tests/testthat/test-gasman-baseline.R`, `test-gas-convergence.R`, `test-gas-engine.R` and `test-gas-scenarios.R`.

- Do the tests test what they claim?
- Are GasMan's own outputs stored and compared, or only stanpumpR's restatement of GasMan?
- What remains untested? The baseline's header lists open and closed circuits, injection, flush and the other agents.

### Part B: Patient covariates

**B1. Trace the covariates.** Trace every path by which the Patient Profile reaches the gas engine (`app_server.R`, around the `gases()` reactive that calls `simulateGases()`; `R/simulateDrugsWithCovariates.R`). As far as we know:

- **Weight** scales every volume, the functional residual capacity included, linearly. It scales cardiac output and default ventilation as weight^0.75, and oxygen consumption linearly.
- **Age** changes only MAC (`macForAge()`).
- **Height, sex, pregnancy, haemoglobin** (outside the arterial-oxygen calculation), **serum albumin** and the rest do not reach the engine.

Confirm or correct this list with line numbers.

**B2. Body size and composition.** stanpumpR's intravenous models follow `docs/weight-adjustment.md`: a 70 kg, 170 cm, 35-year-old reference man, fat-free mass scaling, and `ffmAlSallami()` in `R/pkSizeFactors.R`. The gas engine scales the brain-heart-viscera group, muscle and fat all in proportion to total weight. Evaluate, with sources:

- **Obesity.** Fat mass rising faster than weight, and the vessel-rich group not scaling with fat. What did Weber 2021 (GasMan in obese patients) change, and how did it do it?
- **Children and infants.** The larger vessel-rich share, cardiac output and ventilation per kg, and FRC per kg (Eger, Bahlman & Munson 1971).
- **The elderly** (Strum 1991).
- **Height and sex.** Through fat-free mass, FRC and cardiac output.
- **Reference values for organ volumes and blood flows**, for example Williams and Leggett 1989 and ICRP Publication 89. The engine's 76/18/6% split of cardiac output carries a comment that no primary source for it has been established (`R/gasProperties.R`, `getGasBody()`).

For each covariate you recommend, give:

- the equation and its source (PMID, table)
- the population range it was fitted in
- how it should be bounded outside that range (stanpumpR's rules: every model must stay finite and positive at any legal patient; age terms floor at 18 years unless the source itself studied younger patients)
- its effect on a representative simulation

**B3. Solubility covariates.** Blood:gas coefficients vary with:

- age (neonates: Lerman 1984; Malviya and Lerman 1990)
- haemoglobin, albumin and lipids (Esper 2015)
- eating (Munson 1978)

Which of these change predictions enough to matter, and which are supported well enough to apply?

**B4. MAC covariates.**

- **MAC40 values.** stanpumpR pairs GasMan's MAC values (sevoflurane 2.1, isoflurane 1.1, desflurane 6.0, nitrous oxide 110%) with Mapleson's age slope (−0.00269 per year, about 6% per decade). Mapleson 1996 and Nickalls and Mapleson 2003 publish their own MAC40 values. Is the pairing consistent, and which values should be used?
- **The first year of life.** MAC peaks in infancy and is lower in neonates (Lerman; LeDez and Lerman 1987; Taylor and Lerman 1991; Lerman 1994). Mapleson's relation was fitted from 1 year and is extrapolated below (`macForAge()`).
- **Pregnancy.** Palahniuk 1974, Gin and Chan 1994, Chan 1995 and 1996, and Ueyama 2010. The Patient Profile has a Pregnant field the gas engine does not read. The pregnancy registry marks the inhaled agents `not_applicable`: see the rows for the four agents in `inst/extdata/pregnancyRegistry.csv` (supplement). Check the row's text for internal consistency.
- **Other covariates.** Red hair (MC1R; Liem 2004), chronic alcohol use, sex, and any others with human data.
- For each, recommend apply or not, and give the equation.

### Part C: The physiology sliders and endobronchial intubation

The physiology controls are documented in `inst/app-help/models/physiology.md` and implemented in `R/physiology.R`, `R/altitude.R`, `R/endobronchial.R`, `R/arterialOxygen.R` and `R/physiologyEvents.R`. The sliders that reach the gases are cardiac output, temperature and altitude; haemoglobin acts only through the arterial oxygen. The liver, renal and gastrointestinal sliders do not reach the gases. Say whether they should, for example through volatile-agent metabolism.

**C1. Cardiac output** (`gasBodyPhysiology()`, `gasTissueShares()`).

- Check the redistribution and the split of the brain and heart into their own state.
- Compare with Eger, Bahlman, Halsey and Sawyer 1973 (the distribution of increased cardiac output), Munson, Eger and Bowers 1973, and the chapter's "heart-brain circuit" in shock.
- Is the vessel-rich group's tension (state 3, "Brain") an adequate effect site? Or should the brain have its own equilibration with end-tidal gas, as EEG studies of the volatile agents measure (give the studies)?

**C2. Temperature and MAC.** This is the area Dr Shafer is most concerned about. Make certain that wherever MAC is used, it is MAC at the body's temperature. Enumerate every use of MAC and check that each takes the body's temperature. At least these:

- `gasPropertiesAt()` and `gasMacTemperatureFactor()` (`R/physiology.R`)
- the MAC-equivalents series in `advanceClosedFormGas()`, including per-row temperatures under physiology events
- the band in `gasDrugEntries()`
- `macRecoveryTime()` and `macRecoveryTimeExact()` (`R/gasRecovery.R`)
- `gasCoupledRecovery()` and `gasWashout()`
- `applyOpioidMacInteraction()`
- the Visualizer (`R/viewerRun.R`, the one-MAC line)
- the help tables generated in `R/physiology.R` and `R/help-drugs.R`

Then answer these questions:

- **Per-agent thresholds.** These are stored as 0.1 × MAC at age 40 and scaled by age in `gasThresholdForAge()` (`R/gasRecovery.R`), but not by temperature. The header of the temperature section in `R/physiology.R` says this is deliberate ("typed tensions ... do not move"). Is that consistent with a threshold defined as a fraction of MAC? What would a user see at 32 °C?
- **Evidence for the MAC slope.** Liu 2001 is isoflurane in children; stanpumpR applies its slope to every volatile agent at every age. Find agent-specific data for sevoflurane and desflurane, in people or animals: Eger, Saidman and Brandstater 1965; Regan and Eger 1967; Vitez, White and Eger 1974; Antognini 1993; and later work.
- **Nitrous oxide.** Check the rat-derived nitrous-oxide relation (Antognini, Lewis and Reitan 1994).
- **Hyperthermia.** The slider stops at 37 °C. Should it reach fever? MAC rises with temperature in dogs up to about 42 °C.
- **Solubility.** Check the solubility slopes against Zhou and Liu 2001 and 2002, Allott 1973, and Eger and Eger 1985.
- **MAC-awake.** Does MAC-awake change with temperature as MAC does?

**C3. Endobronchial intubation** (`gasBodyEndobronchial()`, `gasArterial()`, `gasTotalUptake()`, `arterialOxygen()`).

- Check the implementation against the chapter's section "Ventilation/Perfusion Inequalities", Eger and Severinghaus 1964, and Stoelting and Longnecker 1972.
- Does the model reproduce the qualitative predictions: arterial tension about half normal at first for a poorly soluble agent, nearly unaffected for a highly soluble one? Show it with a simulation.
- Evaluate the ventilated lung's share of alveolar volume, which is still open, and the arterial-oxygen calculation (Severinghaus 1979).
- If the evidence on hypoxic pulmonary vasoconstriction during one-lung ventilation justifies reopening the 50% shunt, say so as REOPENS DECISION, with the measured shunt fractions.

**C4. Altitude** (`R/altitude.R`). Check:

- the barometric-pressure model (West 1996)
- the treatment of variable-bypass vaporisers (constant partial pressure) and of the desflurane vaporiser (constant volume per cent)
- humidification
- the claim that MAC as a partial pressure does not change with altitude

**C5. Physiology events.** Check that `gasStateAcrossBodies()` conserves each agent across a step in body or temperature, and that every reader of MAC uses the temperature in force at each time (the `segments` in `gasWashout()`; `gasRecoveryBySegment()`).

### Part D: Other drugs and MAC

At present only opioids modify MAC, through a sigmoid in summed opioid effect-site concentration expressed in multiples of MEAC: Emax 0.9, U50 1.76 MEAC, γ 1 (`R/opioidMacInteraction.R`; `inst/app-help/models/opioid-mac.md`, which tabulates the nine published points it was fitted to). Nitrous oxide adds to the volatile agents in the MAC sum. A separate panel draws Bouillon 2004's propofol-remifentanil response surface for laryngoscopy (`R/modelInteraction.R`). No hypnotic, ketamine, benzodiazepine, α2-agonist or lidocaine effect on MAC is modelled.

**D1. The opioid-MAC interaction as implemented.**

- Check the fit to the nine points.
- Check the use of effect-site concentrations scaled by each opioid's MEAC. Is MEAC the right common scale, or would a published opioid-potency or remifentanil-equivalent scale fit better?
- Compare the direction of the error in the time until threshold as stated in the code comment and on the help page.

**D2. The evidence, drug class by drug class.** Start from Hendrickx, Eger, Sonner and Shafer 2008 ("Is synergy the rule?") and Shafer, Hendrickx, Flood, Sonner and Eger 2008 ("Additivity versus synergy"), then the primary studies. Cover:

- opioids, including the response-surface studies (Manyam 2006; Heyse 2012; Vereecke 2013; Hannivoort 2016)
- propofol (Harris 2006; Schumacher 2009)
- etomidate
- ketamine and S-ketamine (Hamp 2018)
- benzodiazepines: midazolam (Melvin 1982; Inagaki 1993), diazepam (Perisho 1971), remimazolam if any data exist
- dexmedetomidine and clonidine (Aantaa 1997; Fragen and Fitzgerald 1999)
- intravenous lidocaine (Himes 1977)
- nitrous oxide with the volatile agents

For each study give:

- the drug and the volatile agent
- the endpoint: MAC for immobility, MAC-awake, MAC-BAR, tolerance of laryngoscopy, or another
- the concentration measure: measured plasma at steady state, predicted effect-site and by which model, or dose only
- the reduction, n and population
- PMID, page and table

Studies that report only doses cannot be put on stanpumpR's concentration scale without a pharmacokinetic model. Say which model that study's regimen would need, and whether stanpumpR's library model for the drug fits that population. stanpumpR's library includes propofol (Eleveld), etomidate, ketamine (racemic), midazolam, diazepam, lorazepam, remimazolam, dexmedetomidine, lidocaine and the opioids. It has no model of S-ketamine or clonidine.

**D3. A model to recommend.** Propose how stanpumpR should combine several drugs with the volatile agents. Consider:

- fractional MAC reduction per class
- a response surface (Greco; Minto 2000's hierarchical model; Bouillon's sequential model)
- additivity within a class and synergy across classes, as Hendrickx 2008 found

Address three or more drugs at once (volatile + nitrous oxide + opioid + propofol + midazolam), the difference between the immobility and hypnosis endpoints, and which endpoint the MAC-equivalents panel and its threshold should report. Give parameter values with sources, and say how the time until threshold should treat drugs whose concentrations change after the agents are turned off.

### Part E: Eger's chapter, phenomenon by phenomenon

For every phenomenon below, say:

- whether stanpumpR models it, partly models it, or does not (with file and line)
- whether it should, and the evidence
- the expected size of the effect on what a user sees

The phenomena, in the chapter's order:

1. The concentration effect.
2. Constant-alveolar versus constant-inspired delivery, that is, end-tidal targeting.
3. The effect site: brain versus vessel-rich group.
4. Ventilation and cardiac output, and the role of solubility.
5. Anesthetic depression of ventilation and circulation, and the feedback loops (spontaneous versus controlled ventilation; Munson 1973; Gibbons 1977).
6. Surgical stimulation raising ventilation, cardiac output and metabolism.
7. Concurrent changes in ventilation, flow and its distribution: age, size (Wahrenbrock 1974), shock, exercise, and fat blood flow.
8. Temperature: pharmacodynamics (MAC) and pharmacokinetics (solubility, metabolism).
9. Ventilation/perfusion inequality, the alveolar-to-arterial difference (Eger 1966), and endobronchial intubation.
10. Intertissue diffusion, and the five-compartment fits (Carpenter 1986 and 1987; Yasuda 1990 and 1991; Strum 1991; Perl 1965; Rackow 1965; Allott 1976).
11. Henry's law (Lockhart 1990).
12. Duration and MAC-hours as determinants of recovery (Eger and Johnson 1987; Eger 1998; Nordmann 2006; McKay 2010), and context-sensitive decrement times (Eger and Shafer 2005; Bailey 1997; Lockwood 2010).
13. Metabolism (halothane's saturable metabolism; sevoflurane's few per cent).
14. Degradation by CO₂ absorbents.
15. Expansion of gas spaces by nitrous oxide, 1/(1 − F).
16. Circuit structure: valve and fresh-gas placement (Eger and Ethans 1968; Harper and Eger 1976; Mapleson 1954).
17. Percutaneous and visceral losses.
18. The second gas and concentrating effects (Epstein 1964; Stoelting and Eger 1969; Taheri and Eger 1999), and Eger 1978's λ′ = λ(1 − FI). Answer the editorial question in the chapter: must the tissue:gas coefficients also be scaled when λ′ = λ(1 − FI)?

### Part F: New data, and whether to incorporate it

This is the purpose of the audit. Philip's model, and GasMan's parameters, date from decades ago. For each item, give the evidence, the change you would make, its expected effect (quantified), whether it breaks the GasMan validation chain and how to keep that chain, and a priority (now, later, never).

**F1. Structure.** Should the engine add any of the following?

- a fifth, intertissue-diffusion compartment
- ventilation/perfusion mismatch and alveolar dead space for anesthetic gases. Peyton and colleagues showed that this mismatch augments the second gas effect, and that end-tidal and arterial tensions differ (Hendrickx and Peyton)
- a separate brain effect site
- metabolism
- a real circle system with volume and absorbent
- a spontaneous-ventilation mode with anesthetic-depressed ventilation

**F2. Parameters.** Recommend current best values, with sources, for:

- tissue volumes and blood flows (Williams and Leggett 1989; ICRP 89; Mapleson 1973)
- partition coefficients (Yasuda 1989; Esper 2015; Zhou)
- MAC (Mapleson 1996; Nickalls and Mapleson 2003; Eger 2001)
- FRC (by height, sex and age)
- dead space (30% of minute ventilation)
- respiratory quotient (0.8; marked in the code as not yet confirmed by Dr Shafer)
- oxygen consumption (3.5 mL/kg/min, linear in weight, while cardiac output scales as weight^0.75)

**F3. Other models.** Is there a published or open model better suited than GasMan's? Candidates include Lerou and Booij's system model (2001-2002), Levitt's PKQuest (2002), Mapleson's circulation-time models, and the physiologically based models of later groups. What should stanpumpR borrow from it?

**F4. Validation data.** List human data sets, with PMID and table or figure, that could serve as independent validation tests of the model:

- FA/FI curves
- washout curves to days
- the second gas effect in patients
- age-specific uptake
- emergence times by duration

Data not used to build GasMan are the most valuable. For each, write the test as stanpumpR would: the scenario (agents, flows, ventilation, patient), the measured values to compare, and a tolerance that respects the measurement's own scatter.

**F5. Priorities.** End Part F with a ranked list of at most ten changes. For each give the expected gain, the evidence strength (strong, moderate, weak) and the effort.

---

## 6. What to deliver

**One Markdown file**, named `gas-engine-audit-<auditor>-<YYYY-MM-DD>.md` (for example `gas-engine-audit-gemini-2026-11-24.md` or `gas-engine-audit-chatgpt-2026-11-24.md`), with exactly these sections. If your environment can create files, attach the file; if not, put the whole report in one fenced `markdown` block so it can be saved as is.

```markdown
# Gas engine audit: <auditor and model version>, <date>

## Scope and access
- Commits audited: stanpumpR c90c33e, GasMan d3a2dd3
- Code files read in full / skimmed / not read
- Drive access: yes/no; papers opened (PMIDs)
- What I could not do (run R, open the Drive, read a paper)

## Executive summary
(at most 15 bullets, most important first)

## Findings at a glance
| ID | Part | Type | Severity | Title |
|---|---|---|---|---|

## Findings
(one block per finding, IDs A-01..., B-01..., C-01..., D-01..., E-01..., F-01...)

### A-01: <title>
- **Type:** DEFECT | UNDOCUMENTED DEVIATION | EVIDENCE GAP | ENHANCEMENT | REOPENS DECISION (which)
- **Severity:** S1/S2/S3, with the patient and scenario
- **Location:** `stanpumpR/R/....R:<lines>` at c90c33e
- **Code quoted:**
  ```r
  <the exact lines>
  ```
- **Compared with:** `gasman/...:<lines>` quoted, or the paper (PMID, page, table, quotation, how far read)
- **Claim:** what is wrong or missing, in one or two sentences
- **Evidence:** arithmetic, R code to run, or the source
- **Proposed change:** minimal; no GasMan code
- **Test that would show it fixed:**
- **Confidence:** high/medium/low, and what would change your mind

## Part E table
| # | Phenomenon | Modelled? (file:line) | Should it be? | Evidence (PMID) | Effect size |
|---|---|---|---|---|---|

## Part F recommendations
(F1-F4 as asked, then the ranked list F5)

## Papers
| PMID | Citation | Used for | In stanpumpRPapers (file name, or NOT FOUND) | How far read |
|---|---|---|---|---|

## Papers to add
(the papers you needed and could not find in stanpumpRPapers, each with
PMID or DOI and what it would settle; Dr Shafer will add them)

## What I did not check
```

---

## 7. Two reminders

- **Look past each finding for the larger defect beside it.** In earlier audits of this code, the more serious problem often lay next to the one first noticed.
- **A finding Claude cannot reproduce from your report is set aside.** Twenty anchored findings are worth more than a hundred general ones.

---

## 8. Papers to add

In your report, the section **Papers to add** lists every paper you needed that is not in stanpumpRPapers. Dr Shafer will add them and may ask you to finish the parts that depended on them.

---

## 9. Papers already checked against stanpumpRPapers

On 2026-11-23, before this prompt was sent, the papers named above were looked for in stanpumpRPapers by PMID.

<!-- ARCHIVE-CHECK -->
