# Second audit of stanpumpR's inhaled-anesthetic engine: before James Philip reads it

You are one of two independent auditors. The other is a different AI system; you will not see its report, and it will not see yours. Steven L. Shafer, who maintains stanpumpR, will next ask **James H. Philip**, who built GasMan®, to review stanpumpR's gas engine. Before he does, Dr Shafer wants every part of it that Dr Philip will read to be right: the code, the help pages, the validation record, and every statement stanpumpR makes about GasMan.

Your report will be uploaded to Claude, which wrote most of stanpumpR. Claude will check every claim you make against the code and the sources before anything is changed. A claim it cannot check costs time and is set aside, so the rules on evidence in section 3 matter more than the number of findings.

This is a **verification** audit, not a literature review. The first round, on 2026-11-23, asked what new science should change the model; that question has been answered and decided (section 4). This round asks whether what was built since is correct, and whether the whole engine is ready to be read by GasMan's author.

---

## 1. What stanpumpR is, and what changed since the first round

stanpumpR is an R package and Shiny app that simulates drug concentrations for clinical care, teaching and research. It is advisory: it does not control drug delivery. The inhaled agents (nitrous oxide, sevoflurane, isoflurane and desflurane, with oxygen, air, nitrogen and ventilation) have an engine of their own, which follows GasMan's model: per gas, a breathing circuit, the alveolar gas, and the vessel-rich, muscle and fat groups.

- `R/advanceGasManBaseline.R` restates GasMan's own integration scheme in R. It is the reference the tests compare against, not what the app runs.
- `R/advanceClosedFormGas.R` is the production engine. Within each step it solves every gas exactly by matrix exponential, with the total uptake that couples the gases held at its value at the start of the step. Its deliberate differences from GasMan are listed in `inst/app-help/models/gas-differences.md`.

The first round's two audits were validated and fixed. Their assessments are in the bundle (`docs/gas-engine-audit/chatgpt-2026-11-23-assessment.md` and `gemini-2026-11-23-assessment.md`); they say what was confirmed, what was not, and what changed. Since then, these pull requests changed the engine (`CHANGES-SINCE-ROUND-1.diff` in the bundle has the diff):

| PR | What it changed |
|---|---|
| #858 | ChatGPT's round-1 findings. In both R restatements of GasMan: the fast-decay test no longer looks at a fixed circuit's decay; the vernier clock re-arms after every settings row; an open circuit at no flow delivers nothing; the last vernier level always completes. Graded sub-steps in the coupled engine after each change of setting (`gasSubSteps()`: 0.01 min, growing by 1.1 to the plot's step). GasMan's C++ output frozen as `inst/validation/gasman_cpp_fixture.csv` and asserted. MAC at 40 changed to Mapleson 1996's (sevoflurane 1.80, isoflurane 1.17, desflurane 6.6, nitrous oxide 104%; GasMan's kept as `GASMAN_MAC40`). Gas thresholds defined at 37 °C and moved with temperature (`gasThresholdForTemperature()`). The MAC band at the patient's age (`gasTypicalBand()`). A physiology event at the plot's last minute now taken. |
| #864 | Gemini's round-1 findings. The opioid-adjusted MAC reads each opioid from its own series (`derivedEffectSite()`, `totalOpioidMEAC()`), with the opioid's times added to the MAC series and the time until threshold computed at them (`gasWashoutWithTimes()`, `gasRecoveryRefine()`). A body in force for no time is dropped (`gasBodySteps()`). A step in altitude keeps the circuit's and lungs' volume (`gasStateAcrossPressure()`). Alveolar tension converted to mmHg at 713 Torr per 100% (`GAS_ALVEOLAR_TORR`). GasMan's every-ten-ticks re-arm of its fast-decay test restated, and a 5 kg GasMan case frozen. Help-page corrections. |
| #907 | Dr Shafer's ten decisions on the round-1 findings recorded (section 4). |
| #931 | The body temperature slider's top raised from 37 to 38 °C, the same relations extrapolated one degree. |
| #938 | A **Shunt fraction** slider, 0-50%, starting at 50%, shown while the endobronchial box is ticked (`shuntFromInput()`, `checkShunt()`, `simulateGases(shunt =)`, the physiology event's `shunt` field). |
| #939 | The MAC panel's dashed line is now the summed brain (vessel-rich group) MAC (`gasBrainMac()`); the end-tidal line and its time until threshold are unchanged. |
| #940 | When the physiology is off normal, the drug's shaded band is replaced by a grey ribbon from its curve at normal physiology to its curve as simulated (`R/physiologyRibbon.R`, `nominalDrugs()` in `R/app_server.R`), and a gas's threshold line steps with the temperature in force (`thresholdSteps()`). |
| #941 | The gas compartments scaled by body composition (`getGasBody(height =, age =, sex =, adjustToFFM =)`, `gasBodyComposition()`): the vessel-rich group, muscle and lungs follow Al-Sallami fat-free mass, the fat group fat mass, the lean groups' flows fat-free mass to the 0.75 and the fat's its volume to the 0.75 (Weber 2021), cardiac output their sum. The 70 kg, 170 cm, 35-year-old man is GasMan's standard exactly. Without height, age or sex, or with the Patient Profile's **Adjust weight to fat-free mass** unticked, GasMan's linear scaling. Oxygen consumption and default ventilation stay in total weight. Under the cardiac-output slider the brain and heart keep 16/76 of the vessel-rich group's flow. |

---

## 2. Materials

### Pinned commits

Every line number you cite must refer to these commits:

- **stanpumpR** at `8c1ea1bd3610181226f087c2598ae48f5a17b0aa`:
  https://github.com/StevenLShafer/stanpumpR/tree/8c1ea1bd3610181226f087c2598ae48f5a17b0aa
- **GasMan** (GPL-3.0, public) at `d3a2dd36f6be4582cdb7c26ccb860164d9f9983a`, the same commit as round 1:
  https://github.com/rasman/gasmanonline/tree/d3a2dd36f6be4582cdb7c26ccb860164d9f9983a/gasman_api/gasmanAPI
- **Round 1** audited stanpumpR at `c90c33e50346380960d5d86c5a991ca1bf35a1ff`.

### The attachment

The attachment is `stanpumpR-gas-audit-r2-8c1ea1b.zip`. It contains:

- `stanpumpR/...`: the gas engine and the files around it, with their repository paths.
- `gasman/...`: GasMan's API source. The computation is in `gasman/gasman_api/gasmanAPI/GasDoc.cpp` and the parameters in `gasman/gasman_api/gasmanAPI/gasman.ini`.
- `INDEX.md`: every file, with its line count and permalink, and every function and constant, with its line.
- `CORE-NUMBERED.txt`: every file you must read, concatenated, each line numbered.
- `SUPPLEMENT-NUMBERED.txt`: the other stanpumpR files, numbered the same way: `AGENTS.md` (the project's rules and recorded decisions), `R/app_server.R`, `R/simulationPlot.R`, `R/viewerRun.R`, `R/constants.R`, the scenario definitions, the drug table, the round-1 assessments and the larger tests.
- `CHANGES-SINCE-ROUND-1.diff`: what changed in the engine's files from `c90c33e` to `8c1ea1b`. Files added since round 1 (for example `R/physiologyRibbon.R` and several tests) are in the bundle whole and not in the diff.

How to read the files:

- If you can run code, unzip the attachment and use `grep -n`; that is the most reliable way to get line numbers right.
- If you cannot, read `CORE-NUMBERED.txt`. The number before the colon on each line is the line in the file named by the nearest `===== FILE:` or `----- ... continues -----` header above it.

### Running stanpumpR

If you can run R, clone the repository at the pinned commit and run `devtools::load_all()` in it. The exported entry point is `simulateGases()`; its arguments, at the pinned commit (`R/advanceClosedFormGas.R:1719`):

```r
simulateGases(doseTable, weight = 70, age = 50, maximum = 60, cardiacOutput = NULL,
              uptakeEffect = TRUE, circuit = c("ideal", "semi-closed"),
              deadSpace = GAS_DEAD_SPACE_FRACTION, oxygenUptake = TRUE,
              physiology = NULL, temperature = NULL, altitude = NULL,
              endobronchial = FALSE, physiologyEvents = NULL, haemoglobin = NULL,
              shunt = NULL, height = NULL, sex = NULL, adjustToFFM = TRUE)
```

An example that runs at the pinned commit: 4 L/min nitrous oxide and 2 L/min oxygen with 2% sevoflurane, 6 L/min minute ventilation, all turned off at 60 minutes, in a 120 kg, 165 cm, 60-year-old woman, at 0.6 of normal cardiac output, 35 °C, 1500 m, with an endobronchial tube and a 30% shunt:

```r
dt <- data.frame(Drug = c("oxygen", "nitrousOxide", "sevoflurane", "ventilation",
                          "sevoflurane", "nitrousOxide", "oxygen"),
                 Time = c(0, 0, 0, 0, 60, 60, 60),
                 Dose = c(2, 4, 2, 6, 0, 0, 6))
s <- simulateGases(dt, weight = 120, age = 60, maximum = 120, height = 165, sex = "female",
                   physiology = c(cardiac = 0.6), temperature = 35, altitude = 1500,
                   endobronchial = TRUE, shunt = 0.3)
head(s$results)          # Drug, Time, Site (Alveolar, Brain, MAC), Y
dim(s$state$sevoflurane) # one row per time; columns are the gas's states
```

The tests in `tests/testthat/test-gas-*.R`, `test-brain-mac.R`, `test-physiology-ribbon.R`, `test-endobronchial.R`, `test-altitude.R` and `test-arterial-oxygen.R` show how the rest is called. If you cannot run R, say so, and give R code that Claude can run, or re-implement a small calculation in Python and give the code.

### Papers

This round needs few papers. Where one is needed (Weber 2021 for body composition, the Al-Sallami fat-free mass, Mapleson 1996), it is in Dr Shafer's Google Drive folder **stanpumpRPapers** (folder ID `1Hi6HmnULQsIlETZw4lo5rhxkfog33vRR`), named `PMID_<pmid>.pdf`; find a file by searching for its name, not by listing the folder. If you cannot reach it, mark the claim *not read*. Do not retain, store or train on anything you read there.

---

## 3. Rules of evidence

Earlier external reviews of this code produced confident claims that were false: PMIDs called fabricated that were correct, factors called "exact match" that did not match the code, and functions that do not exist. Hence these rules.

1. **Anchor every code finding.** Give the path and line number(s) at the pinned commit, for example `stanpumpR/R/advanceClosedFormGas.R:1513-1530` or `gasman/gasman_api/gasmanAPI/GasDoc.cpp:840-852`, and **quote the exact line or lines** you are critiquing. If you cannot find a line, say so; never describe code you have not seen.
2. **Anchor every help-page finding** the same way: the page's path and line, the sentence quoted, and the code line that contradicts it, quoted.
3. **Anchor every literature claim.** First author, year, journal, PMID, page, table or figure, and the number or a quotation of at most 25 words. Say how far you read it: *read in full (PDF)*, *abstract only*, *secondary source* (name it) or *not read*. A number from memory is *UNVERIFIED*.
4. **Never write "exact match", "verified" or "identical"** unless you compared the two values digit by digit, and show both.
5. **Quantify.** Where something changes what a user sees, say by how much, for which patient and settings, and show the arithmetic or the code.
6. **Say whether a finding is in code changed since round 1** (it appears in `CHANGES-SINCE-ROUND-1.diff`, or in a file added since) **or in older code.**
7. **Do not paste GasMan's code into a proposed change.** GasMan is GPL-3.0 and stanpumpR is MIT. Describe what GasMan does, quoting the lines you compared, and propose stanpumpR code of your own.
8. **Classify every finding** as one of:
   - **DEFECT**: the code does not do what its own comments, help page or tests say; in a GasMan restatement, it does not do what GasMan does.
   - **WRONG STATEMENT**: a help page, comment or validation record says something the code (or GasMan's code, or the cited source) does not bear out.
   - **UNDOCUMENTED DEVIATION**: the production engine differs from GasMan in a way `gas-differences.md` does not list.
   - **TEST GAP**: a test does not test what it says, or something claimed is not tested.
   - **REOPENS DECISION**: a challenge to a recorded decision (section 4), with the new evidence. Use it sparingly.
9. **Severity, with the patient and scenario named:**
   - **S1**: changes a number a clinician could act on by 10% or more for a plausible patient, breaks conservation of mass or units, or says something untrue about GasMan.
   - **S2**: 1-10%, or only at legal but unusual inputs.
   - **S3**: under 1%, wording, or clarity.
10. **Mark each finding Blocking or Not blocking** for Dr Philip's review. Blocking means he would be right to object to it, or it is S1.
11. **Say what you did not check.**

---

## 4. Already settled: report only if the implementation is wrong

Read `stanpumpR/AGENTS.md` (in the supplement) for the project's rules. These decisions bear on this audit. Report against them only if the code does not do what was decided, or with new evidence marked REOPENS DECISION.

**From round 1 and before:**

- GasMan's parameters are kept, except where a row of `gas-differences.md` says otherwise. The GasMan restatement stays GasMan's; departures live in the production engine.
- MAC at 40 is Mapleson 1996's (2026-11-23). MAC by age is Mapleson's relation. MAC as a partial pressure does not move with altitude.
- Thresholds for the time until threshold: MAC 0.1; each volatile agent 0.1 × its age-adjusted MAC; nitrous oxide 10%. Every gas threshold is defined at 37 °C and moves with temperature as MAC does (2026-11-24).
- Cardiac-output slider: the brain and heart keep 16% of resting output, scaled by cardiac output^0.35 (2026-11-14). With body composition, that share is held at 16/76 of the vessel-rich group's flow.
- Temperature: volatile MAC follows Liu 2001; nitrous oxide MAC follows Antognini 1994 in rats; temperature is not coupled to cardiac output or ventilation; the range is 28-38 °C, the degree above 37 °C extrapolated (2026-11-25).
- Endobronchial intubation: a shunt slider of 0-50% starting at 50%, no hypoxic pulmonary vasoconstriction (2026-11-19, 2026-11-25). The ventilated lung's share of alveolar volume, 0.5, is still open.
- Altitude: flowmeter calibration is not modelled (open).
- The opioid-MAC interaction is a placeholder, to be reviewed when Dr Shafer's anaesthetic drug interaction model arrives. Only opioids lower MAC, on one MEAC scale (decision of 2026-11-25). The time until threshold holds the opioid's effect fixed rather than letting it wash out (about 9% long in the case measured; the help page says so). Do not propose a replacement in this round.

**Decisions of 2026-11-25** (the assessment of Gemini's round-1 report lists all ten):

- The ribbon (#940): band removed and a grey ribbon drawn from the normal-physiology curve to the curve, on the line the panel draws; threshold line kept, stepping with temperature.
- The MAC panel's dashed line (#939): the summed brain (vessel-rich group) MAC.
- Body composition (#941), with oxygen consumption and default ventilation in total weight: "That is how clinicians think about it."
- Infant MAC: deferred until four papers arrive; Mapleson's relation stays below 1 year. Pregnancy: no factor on MAC.
- The lung stays a single well-mixed lung; no ventilation/perfusion scatter until Peyton's mathematics has been checked.
- GasMan's own defects (`inst/validation/VALIDATION.md`) are to be reported to its maintainers.

**Out of scope:** `cube()`, the intravenous engines (except where the temperature extension to 38 °C reaches them), Shiny UI robustness, the greenhouse-gas calculation, and the JavaScript of the Visualizer (`tools/viewer/`; its R side, `viewerGasRun()`, is in scope).

**Fixed earlier; do not re-report unless the fix is wrong:** everything marked fixed in the two round-1 assessments, and in `AGENTS.md` under "Rules from Carbon's third/fourth UI audit" and "From CodeRabbit's review of PR #730" (the closed circuit's supply limit, negative uptake on emergence, the washout step for small patients, the rebound check, a default ventilation never zero, repeated settings as change points).

**Already known, fixed after the pinned commit:** the first line of `inst/app-help/models/gas-differences.md` says the table lists eleven differences; since #941 added the body-size row it lists twelve. One line in your report is enough.

---

## 5. The audit

Work through Parts A to F in order.

### Part A: What GasMan's author will check first

**A1. The restatement against GasMan.** Map `R/advanceGasManBaseline.R` and `inst/validation/gasman_baseline_standalone.R` to `GasDoc.cpp` (`ComputeTerms`, `Calc`, `CalcUptake`, the vernier loop, `SetWeight`, `InitDocument`) and report every remaining difference. Check in particular the five changes made since round 1: the fast-decay test and a fixed circuit, the vernier clock after a settings row, an open circuit at no flow, the last vernier level, and the re-arm every ten ticks. Each restatement's header lists what it does not restate (liquid injection, the flush, the vapour-volume option and others): is that list complete and accurate?

**A2. Every statement stanpumpR makes about GasMan.** Enumerate the statements about GasMan in the help pages, the code comments, `inst/validation/VALIDATION.md` and the round-1 assessments: its authorship and history, its licence, what its API and its desktop program each do, its defaults, its parameters, and its defects. For each, say whether it is true at `d3a2dd3`, with the lines. In particular:

- `VALIDATION.md` (the section headed "The fast-decay test re-arms every ten ticks" and the one after it) describes two defects in GasMan itself: the forward calculation applies its fast-decay test for three ticks and the history replay for two, and the CSV writer reports the forward values for the last ten ticks, so a printed value depends on the end time asked for; and `loadJsonCircuit` misreads the circuit names "Semi-closed" and "Closed". Dr Shafer intends to send these to GasMan's maintainers. Check each claim against GasMan's source (`GasDoc.cpp`: `Calc`, `ResultsAt`, `loadJsonCircuit`; `GasApplication.cpp`: `createCSV`; the command-line tool `gasman_api/run/main.cpp`, in the zip but not numbered), quoting the lines, and say whether the scenario and numbers given are enough for someone else to reproduce it.
- Are GasMan and its author credited correctly and consistently (the name, the ® mark, Brigham and Women's Hospital, the GPL-3.0 repository)? Is anything said of GasMan unfair, overstated or out of date?

**A3. `gas-differences.md`.** For each row, show the code that implements it, quoted. Then list anything in the production engine at its defaults that differs from GasMan and is not a row. Consider what was added since round 1: the graded sub-steps, Mapleson's MAC at 40, body composition, and the controls that are off at normal (temperature, altitude, cardiac output, the tube, physiology events, the brain MAC line). Should any of those be a row? Recompute the numbers in "Consequences when comparing side by side" (the semi-closed and ideal circuit values at 1 and 30 minutes; the vapour-volume case, 4.89 and 5.05%).

**A4. The validation record.** Read `inst/validation/VALIDATION.md`, `gasman_cpp_fixture.csv`, `tests/testthat/test-gasman-baseline.R`, `test-gas-scenarios.R`, `test-gas-convergence.R` and `test-gas-engine.R`.

- Does every claim in `VALIDATION.md` match a test that asserts it, at the tolerance stated?
- Since #941, `getGasBody()` scales by body composition when it is given height, age and sex. Confirm that **every** GasMan comparison still runs with GasMan's linear scaling, so that no comparison passes or fails for the wrong reason. Name any test or script that passes height, age and sex.
- Is the 70 kg, 170 cm, 35-year-old man GasMan's standard to the last digit in every field of the body (volumes, flows, fractions, VO2, circuit)?

### Part B: Each change since round 1

For each change, check four things: does it do what its help page, comments and tests say; does it conserve each gas; does it stay finite and sensible at the legal extremes (age 0 to 90+, weight and height across the Patient Profile's ranges, both sexes, both positions of **Adjust weight to fat-free mass**); and does it work together with physiology events and the other controls?

**B1. Graded sub-steps** (`gasSubSteps()`). Check the error figures in `inst/app-help/models/gas-engine.md` ("Integration") and in the ChatGPT assessment's table against your own reference. What happens to the ramp at a physiology event, at a row that repeats the settings, and at a change of setting inside a ramp?

**B2. The opioid-adjusted MAC** (`totalOpioidMEAC()`, `derivedEffectSite()`, `applyOpioidMacInteraction()`, `gasWashoutWithTimes()`, `gasRecoveryRefine()`). Is it causal: no effect before a bolus is given? Are the opioid's times joined to the MAC series without changing the gas values at the gas times? Does the refinement's 1% rule do what its comment says?

**B3. Physiology events, and altitude steps** (`gasBodySteps()`, `gasStateAcrossBodies()`, `gasStateAcrossPressure()`, `gasCircuitBlend()`). For a step up and a step down in altitude, with the ideal and the semi-closed circuit, at high flow and at no flow, state the invariant that should hold across the step (what is conserved in the lungs, the circuit and each tissue) and test it. Check the same for a step in cardiac output, temperature, the tube and the shunt, and for several steps at one time.

**B4. Partial pressures in mmHg** (`GAS_ALVEOLAR_TORR`). List every conversion between a tension in per cent and mmHg or Torr in the engine, the arterial oxygen, the help pages and the Visualizer's R side, and say whether each uses 713 (dry alveolar gas at sea level), 760 or the barometric pressure where it should.

**B5. Temperature to 38 °C.** List every reader of temperature (round 1 listed `gasPropertiesAt()`, `gasMacTemperatureFactor()`, the MAC series, the band, `macRecoveryTime()`, `gasCoupledRecovery()`, `gasWashout()`, `applyOpioidMacInteraction()`, the Visualizer, the generated help tables, and now `gasThresholdForTemperature()` and `thresholdSteps()`), and check that each is right between 37 and 38 °C: nothing clamped at 37, nothing refused, MAC and solubility extrapolated by the same relations. For the intravenous drugs, `physiologyTemperatureFactor()` now exceeds 1 above 37 °C: is that applied as the help says?

**B6. The shunt slider** (`shuntFromInput()`, `checkShunt()`, `gasBodyEndobronchial()`, `gasArterial()`, `gasArterialOxygen()`). What does the tube do at a shunt of 0%? (The ventilated lung still holds half the alveolar volume.) Is that what the help page says? Is the shunt carried by physiology events and bookmarks only while the tube is in? Does the arterial oxygen follow the slider?

**B7. The brain MAC line** (`gasBrainMac()`, `gasMacSum()`). Which state is "brain" when the cardiac-output slider splits the vessel-rich group, and is that what the help says? Is the brain MAC at the temperature in force, adjusted for age, and adjusted for opioids the same way as the end-tidal line? Does anything (the time until threshold, the band, the threshold line, the Interaction panel) read the brain line where it should read the end-tidal line, or the reverse?

**B8. The ribbon and the stepping threshold** (`physiologyRibbons()`, `nominalStepMatched()`, `thresholdSteps()`, `nominalDrugs()` in `R/app_server.R`, `R/simulationPlot.R`). Is the "normal" simulation normal in every control the gases read (cardiac output, temperature, altitude, tube, shunt, haemoglobin, physiology events) and in nothing else (the patient, the doses and the body composition must be the same)? Is the ribbon on the line the panel draws? Does the threshold line step at the same times and to the same values as the threshold the time until threshold uses?

**B9. Body composition** (`getGasBody()`, `gasBodyComposition()`, `gasBodyPhysiology()`, `GAS_REFERENCE_VRG_SHARE`, `ffmAlSallami()` in `R/pkSizeFactors.R`).

- Check the equations against Weber 2021 (PMID 32067148) and the Al-Sallami fat-free mass, and the anchoring at the reference man.
- The lungs' volume follows fat-free mass and the alveolar ventilation follows total weight: what does that do to the alveolar time constant in a 150 kg patient, and is it what the help says?
- Oxygen consumption is in total weight while cardiac output follows composition: check the mixed venous oxygen and the arterial oxygen in an obese patient with the tube.
- Children: is the Al-Sallami relation used within its range, and what happens below it (a 3.5 kg newborn, 50 cm)?
- The extremes: age 0, the smallest and largest weight and height the Patient Profile accepts, both sexes, the cardiac-output slider at its ends.
- `viewerGasRun()` (`R/viewerRun.R`): does the Visualizer receive the composed body?

### Part C: Conservation across combined settings

Write a mass balance for each gas, nitrogen and oxygen included: what the fresh gas delivered, what is held in the circuit, the lungs and each tissue, what was taken up, what was vented, and the oxygen consumed. Run it over combinations of the changes, for example:

- a 150 kg, 160 cm, 70-year-old woman and a 4 kg, 3-month-old infant, composition on and off
- cardiac output at 0.4 and 1.5 of normal
- 32 °C and 38 °C, and a step from 37 to 32 °C at 30 minutes
- 3000 m, and a step from 3000 m to sea level at 30 minutes
- the tube with a 30% shunt, put in at 10 minutes and taken out at 40
- fresh gas at 0.5 L/min and at 8 L/min
- 2% sevoflurane with 60% nitrous oxide, turned off at 60 minutes

Report any non-finite value, any negative tension, any alveolar total above 100% of the dry gas, and any gas created or lost, with the size of the imbalance. Give the code.

### Part D: The help pages against the code

Dr Philip will read these pages. For every number, equation and claim about the gas engine in:

- `inst/app-help/models/gas-engine.md`, `gas-differences.md`, `physiology.md` (the gas parts), `recovery.md` (the gas parts), `opioid-mac.md`, `fat-free-mass.md`
- `inst/app-help/inhaled-agents.md`, `patient-profile.md` (the parts that reach the gases), `validation.md`
- `inst/app-help/drugs/sevoflurane.md`, `isoflurane.md`, `desflurane.md`, `nitrousOxide.md`, `oxygen.md`, `air.md`, `ventilation.md`
- the ten gas scenario pages under `inst/app-help/scenarios/`
- `inst/validation/VALIDATION.md`

say whether the code bears it out. Recompute the numbers you can. Report statements made stale by the changes since round 1 (Mapleson's MAC, body composition, 38 °C, the shunt slider, the brain MAC line, the ribbon). Give a table of what you checked, including what was right, so that the coverage is visible.

### Part E: The Visualizer's gas run (R side)

`viewerGasRun()` reads the engine's own states. Check that what it sends (capacities, flows, the shunt and arterial values, the cardiac output per time, the lungs' capacity across a tube event, the physiology line) agrees with the engine under body composition, the shunt slider, and temperature and altitude events.

### Part F: Through GasMan's author's eyes

If you were James Philip reading these files, what would you object to, find unclear, or find unfair? stanpumpR says its engine "follows GasMan" and was "validated against GasMan". Is that claim supported, and is it stated with the right limits, given that the app runs the ideal circuit, Mapleson's MAC, body composition and the matrix-exponential integration by default? At most ten items, most important first.

---

## 6. What to deliver

**One Markdown file**, named `gas-engine-audit-round2-<auditor>-<YYYY-MM-DD>.md` (for example `gas-engine-audit-round2-chatgpt-2026-11-27.md`). If your environment can create files, attach it; if not, put the whole report in one fenced `markdown` block so that it can be saved as it is. If you run out of room, stop at a finding's end and say so; you will be asked to continue.

```markdown
# Gas engine audit, round 2: <auditor and model version>, <date>

## Scope and access
- Commits audited: stanpumpR 8c1ea1b, GasMan d3a2dd3
- Files read in full / skimmed / not read
- Could I run R? Python? Reach stanpumpRPapers? Which papers did I open?

## Ready for James Philip?
One of: Yes / Yes, after the blocking items below / No. Then the blocking
items, by ID, one line each.

## Executive summary
(at most 12 bullets, most important first)

## Findings at a glance
| ID | Part | Type | Severity | Blocking | New since round 1? | Title |
|---|---|---|---|---|---|---|

## Findings
### A-01: <title>
- **Type:** DEFECT | WRONG STATEMENT | UNDOCUMENTED DEVIATION | TEST GAP | REOPENS DECISION (which)
- **Severity:** S1/S2/S3, with the patient and scenario
- **Blocking:** yes/no
- **New since round 1:** yes (PR #) / no
- **Location:** `stanpumpR/<path>:<lines>` at 8c1ea1b
- **Quoted:**
  ```r
  <the exact lines>
  ```
- **Compared with:** the GasMan lines, the help sentence, or the source, quoted
- **Claim:** one or two sentences
- **Evidence:** arithmetic, or code Claude can run
- **Proposed change:** minimal; no GasMan code
- **Test that would show it fixed:**
- **Confidence:** high/medium/low, and what would change your mind

## Part C: mass balance
| Scenario | Gas | Largest imbalance (mL, and % of delivered) | When | Notes |
|---|---|---|---|---|

## Part D: help pages checked
| Page:line | Statement (short) | Code (file:line) | Verdict |
|---|---|---|---|

## Part F: through GasMan's author's eyes
(at most ten items)

## What I did not check
```

---

## 7. Reminders

- **Look beside each finding for a larger defect.** In earlier audits of this code, the more serious problem often lay next to the one first noticed.
- **New code written to close a finding is where the next defect is.** The fixes of #858 and #864 and the features of #931 to #941 were written in the last three days. Read them as you would read code you suspect.
- **A finding Claude cannot reproduce from your report is set aside.** Twenty anchored findings are worth more than a hundred general ones, and a short report that says "checked, correct" with its evidence is a useful report.
