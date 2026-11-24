# Gemini's audit of the gas engine, 2026-11-23: assessment

Gemini audited stanpumpR at `c90c33e` and GasMan at `d3a2dd3`, with the prompt in
`PROMPT.md` beside this file. ChatGPT audited the same commits with the same
prompt (`chatgpt-2026-11-23-assessment.md`); the two were kept apart, and
ChatGPT's findings were fixed in PR #858 before this one began. Gemini's report
came back as `gas-engine-audit-gemini-2026-11-23.md`; it quotes papers, so it is
kept out of the repository.

Under the AGENTS.md rule on adversarial review, every finding is a lead. Each was
checked by Claude Code on 2026-11-24, part by part: every quoted line at
`c90c33e` and again on the master the fixes were made on (`ec042bd6`), every
number re-run with the package on R 4.3.3, GasMan's source read at `d3a2dd3` and,
for Part A, a `gasman_run` built from it (it reproduces all 520 values of the
frozen fixture), and the literature claims checked against the repository's
cards, the archive's PDFs where present and PubMed abstracts where not.

**The pattern.** Gemini's code facts were nearly all right and most of its
numbers reproduced, several to the digit (the 24-hour opioid aliasing, the
endobronchial MAC panel, the 5000 m physiology event, the 5 kg GasMan case). Its
errors were in the literature and in its own comparison models, as in its
earlier passes: the "Weber 2021" emergence times it used as a benchmark are not
in Weber 2021 but its own simulation of Weber's body; Westmoreland 1994's
fentanyl value is 0.5 ng/mL, not 1.67 (McEwan 1993's); two Katoh papers are
conflated; Schumacher 2009 estimated no ke0; the 2.1% sevoflurane MAC is the
label's value at age 40, not age 20; GasMan's API does not scale its default
flows with weight; xenon's VRG is in `gasman.ini`; and a function it named,
`evalRecoveryAt`, does not exist. Several of its proposed tests were wrong (a
`maxVernier` of 2 where GasMan uses 1; one that would build a GasMan parser
bug into the restatement). And, as with Carbon, the larger defect sometimes lay
beside the claim: D-01 is acausal on a plot of any length, not only a long one,
and A-01 led to an inconsistency inside GasMan itself.

**Agreement with ChatGPT.** The two found independently: the vernier clock
(A-01), the integration step on long plots (Gemini A-05, ChatGPT A-03), MAC at
40 (B-03, B-02), the default thresholds and temperature (C-02, C-01), the band
at the patient's age (C-01, C-02), the opioid-MAC comment (D-03, D-01), the
"closed source" text, the inhaled agents' pregnancy rows, and the V/Q and body
size proposals. All of those were fixed or decided in #858. Gemini alone found
D-01's aliasing, C-07, C-06, the ten-tick re-arm and the stale help statements.

## Summary

| ID | Report's type | Verdict | Done |
|---|---|---|---|
| A-01 | defect | Fixed by #858 in part; a further divergence confirmed | Fixed: the test re-arms every ten ticks; a 5 kg GasMan case frozen |
| A-02 | defect | Flow scaling not reproduced; the tick already documented | Our own docs' attribution corrected |
| A-03 | defect | Confirmed in part; its test and Epstein explanation wrong | Standalone constants corrected; GasMan's parser bugs documented |
| A-04 | undocumented deviation | Default FGF not reproduced; altitude in the semi-closed circuit real but unreachable; MAC line not a deviation | Documented |
| A-05 | evidence gap | Already fixed by #858 | Nothing more |
| A-06 | defect | Not reproduced as stated; the real gap fixed by #858 | Help wording corrected |
| B-01 | evidence gap | Code facts confirmed; benchmarks misattributed | A proposal; a recorded decision of 2026-10-05 |
| B-02 | evidence gap | Direction holds; magnitudes overstated | A proposal |
| B-03 | evidence gap | MAC40 decided in #858; infant and pregnancy parts proposals | For Dr Shafer |
| B-04 | defect | Two of six fixed by #858; one kept on purpose; three small items remain | Recorded |
| C-01 | defect | Age half fixed by #858; temperature half confirmed, an open decision | Documented; for Dr Shafer |
| C-02 | evidence gap | Decided 2026-11-24 and implemented in #858 | Nothing more |
| C-03 | evidence gap | Reopens the 28-37 C range | For Dr Shafer |
| C-04 | defect | Numbers confirmed; the panel is alveolar by design; a stale header | Header corrected; the dashed line for Dr Shafer |
| C-05 | reopens decision | PaO2 50.6 reproduced; its 0.3-shunt arithmetic not | For Dr Shafer |
| C-06 | defect | Confirmed | Fixed: 713 Torr per 100% |
| C-07 | defect | Confirmed, all three parts | Fixed: an event at 0; altitude steps move gas |
| D-01 | defect | Confirmed, and larger | Fixed: each opioid read from its own series |
| D-02 | evidence gap | Its central literature claim wrong | A proposal |
| D-03 | defect | Comment fixed by #858; the states exist, the function does not | Comment corrected; coupling a proposal |
| D-04 | evidence gap | Behaviour by design; arithmetic and citations partly wrong | A proposal |
| D-05 | enhancement | Effect-site line confirmed; the -31% jump not reproduced | For Dr Shafer, with C-04 |
| E-01 | defect | Three statements stale; two fixed by #858 | Fixed |
| E-02 | evidence gap | Structure statements accurate | A proposal (ChatGPT's F-01) |
| F-01 to F-04 | enhancement | Proposals, with internal errors | For Dr Shafer |
| Part E #18 | question | The algebra confirmed; one column misexplained | Recorded |

And two from CodeRabbit's review of #858, carried here: no sub-step after the
graded ramp is longer than the plot's step (`ceiling()`), and the threshold
line's temperature is documented (C-01).

## The defects fixed

**D-01, the opioid-adjusted MAC.** `totalOpioidMEAC()` read each opioid's effect
site from its 100 equispaced points, 14.5 minutes apart on a 24-hour plot. A 140
mcg remifentanil bolus at 5 minutes with 2% sevoflurane read 0.86 MAC at 6.2
minutes where the adjusted MAC is 2.45 (Gemini: 0.731 against 2.083 at GasMan's
MAC, reproduced to the digit). The defect is larger than reported: the effect
begins about one grid step before the bolus on a plot of any length (+49% on a
one-hour plot at 4.98 minutes). The MEAC and Interaction panels had the same
defect and were moved off `equiSpace` on 2026-10-09 (`R/derivedSeries.R`); this
reader was missed. Now each opioid is read from its own series
(`derivedEffectSite()`) at the MAC series' times, the opioids' own times join the
MAC series (a late bolus peaks at 3.883, the gas points alone giving 3.742), and
the coupled washout adds to its sample every row whose threshold the sampled rows
interpolate to more than 1% off (`gasRecoveryRefine()`): sampled, the time until
threshold read 4.3 minutes where every row gives 8.7. Cost on a 24-hour plot 0.34
to 0.60 s, within 0.06 minute of computing every row.

**C-07, physiology events.** An event at time 0 left the run starting in the
normal body for no time: at 5000 m the lungs started at sea-level room air (752
Torr where the pressure is 420; PaO2 159 against 83 mmHg), the first time was
recorded twice, and the Visualizer refused a run whose temperature was 30 C
throughout. `gasBodySteps()` now drops a body in force for no time, and the run
equals the slider's exactly. A step in altitude later in the run kept every
gas-phase tension, so just after a step from sea level to 5000 m the alveolar
gases summed to 94% of a sea-level atmosphere where 52% fits.
`gasStateAcrossPressure()` keeps the circuit's and lungs' volume: a fall lets gas
out at its composition, a rise draws the circuit's gas into the alveoli (Gemini
proposed scaling both ways, which is right only for a fall).

**C-06, oxygen in mmHg.** The engine's alveolar tensions are fractions of the dry
gas, so 100% is 760 - 47 = 713 Torr; `arterialOxygen()` and the altitude help
table converted at 760, so the alveolar oxygen read 6.6% high everywhere, 722.7
mmHg on pure oxygen. Now `GAS_ALVEOLAR_TORR`. Behind the 50% shunt on oxygen,
PaO2 50.6 to 49.8 mmHg; on air at 5000 m, SaO2 47 to 43%. The help page's
figures are recomputed. (Its second part, the circuit tension's humidity basis at
altitude, is ChatGPT's C-04, recorded as display only.)

**A-01, the ten-tick re-arm.** GasMan starts a history sample every ten ticks and
re-arms its fast-decay test at each; the restatements re-armed it only at a
settings row, so a 5 kg patient read 1.2% low against GasMan. Fixed in both
restatements; the 5 kg case is frozen from `gasman_run` and passes at 0.25%.
Beside it, a defect in GasMan itself: its forward calculation tests three ticks
and its history replay two, and its CSV writer mixes the two, so a printed value
depends on the end time asked for (13.6% apart at one time in that patient).
Gemini's proposed fix (re-arm for three ticks every ten) would have been 13.6%
off. Recorded in `inst/validation/VALIDATION.md`.

**Smaller.** The file header said MAC was summed from the brain tension (C-04;
the code has used alveolar since an earlier change and says why). The help said
diffusion hypoxia and one-lung ventilation are not modelled, and that the
sliders' parameters never change during a run (E-01): the engine produces
diffusion hypoxia (alveolar oxygen 13.9% two minutes after 70% nitrous oxide is
switched to air, against 15.3% for the same change from nitrogen), the
endobronchial box is one-lung ventilation, and physiology events change the
parameters. The standalone GasMan script's enflurane and halothane constants were
not `gasman.ini`'s (A-03). Our own documents credited GasMan's API with a weight
scaling of the default flows that only the desktop program applies (A-02). The
comment on the frozen-R approximation said the intravenous engines do not expose
the opioids' washout; they keep `recoveryStatesOwn` (D-03).

Tests: `tests/testthat/test-gas-audit-gemini.R` (D-01, C-07, C-06, A-01), each
failing on `ec042bd6`; `test-coderabbit-858.R` for the graded tail.

## Not reproduced, or not defects

- **A-02.** GasMan's API does not scale default VA and CO with weight
  (`InitDocument` computes the factor at weight 70; a `gasman_run` at 10 kg with
  no flows reports 4 and 5 L/min). Gemini's "-100%" compared runs with different
  inputs. Do not adopt its allometric defaults in the restatement.
- **A-03's Epstein explanation.** The "Semi-closed" parser bug is real in GasMan,
  but every scenario file here says "Semi", so no comparison was affected, and
  whether it explains Epstein's early report is not established. A "Closed" name
  hits the same bug; Gemini's proposed test would have built it in.
- **A-04.** stanpumpR has no default fresh gas flow. The MAC panel's alveolar
  basis is GasMan's too (its MAC column is ALV/MAC).
- **A-06.** `VALIDATION.md` already recorded five `gasman_run` scenarios; what
  was missing (an assertion) #858 added.
- **D-05's jump.** State 3 at 2 minutes is 0.5618% at a cardiac multiple of
  1.00, 0.999, 0.99 and 1.01; the split group is 1.263 L, not 2.1.
- **Part E #18.** The algebra is right: replacing the blood:gas coefficient with
  lambda (1 - F_I) needs the tissue:gas coefficients scaled too, and every value
  in Gemini's table reproduces. Its "true coupled" column is
  `advanceGasManBaseline()` with nitrogen washing out from 80%, acting as a second
  gas, not a discrete-tick error.

## For Dr Shafer

Each is recorded where its answer will live.

1. **The band and the threshold line under a temperature event** (C-01): both
   are drawn at the temperature at time 0, while the MAC panel and the time
   until threshold follow the temperature in force. Stepping them is one piece
   of work. Recorded as a `[decision]` in `R/physiologyEvents.R`.
2. **The MAC panel's dashed "effect site" line** (C-04, D-05): it is the alveolar
   line again. Options: leave it; draw the summed brain (vessel-rich) MAC there,
   keeping the alveolar line and its time until threshold (with an endobronchial
   tube at 5 minutes, 0.955 alveolar against 0.497 brain); or a CNS effect site
   with its own ke0 (F-04), for which no source in the archive gives a
   sevoflurane ke0.
3. **The endobronchial shunt** (C-05): kept at 0.5 with no hypoxic pulmonary
   vasoconstriction on the decision of 2026-11-19. Gemini would reopen it; its
   clinical comparison is largely planned one-lung ventilation in the lateral
   position, not an accidental mainstem intubation.
4. **Fever** (C-03): the range is 28-37 C. Above 37 is refused throughout.
5. **Body size in the gas compartments** (B-01, F-01): linear in weight,
   GasMan-identical, on the decision of 2026-10-05. With matched ventilation a
   150 kg patient's sevoflurane emergence here is about three times the 70 kg
   value, where Weber 2021's GasMan model of an obese patient (lean volumes
   unchanged, fat volume and flow raised) found the 90% decrement after five
   hours barely changed (29 minutes at 70 kg, 26 at 150).
6. **Infant MAC and pregnancy** (B-03, F-03): with Mapleson's MAC at 40,
   sevoflurane MAC at age 0 is 2.31% against Lerman 1994's 3.3% in neonates. A
   pregnancy factor of 0.72 rests on isoflurane at 8-12 weeks, which the
   registry's rules do not let it apply to a term patient or to other agents.
7. **Drugs other than opioids on MAC, and drug-specific opioid potency** (D-02,
   D-04, F-02): propofol, benzodiazepines, dexmedetomidine and ketamine leave MAC
   unchanged today, by design. Gemini's model needs its evidence corrected first
   (above).
8. **The opioid's own washout in the time until threshold** (D-03): the states
   exist when the time until threshold is drawn, but not for every engine, and
   the coupled gas washout would need a threshold that changes as it runs.
9. **The lung's V/Q scatter** (E-02, F-04): ChatGPT's F-01 again.
10. **GasMan's own defects** (A-01, A-03): the forward/history split and the
    two circuit names its parser misreads. Whether to report them to GasMan's
    maintainers is yours to decide.

Small items left in the pregnancy registry (B-04): the isoflurane row's n = 32
does not say how it was counted; nitrous oxide's, desflurane's and
sevoflurane's rows copy fields from studies of other agents, as their cards say
is extrapolated context; and a note's "the age adjustment in R/gasRecovery.R" is
in `R/gasProperties.R` (`macForAge()`). Gin and Chan's `evidence_read` stays
`abstract` on purpose.

## Papers

Gemini read from elsewhere on the Drive, outside stanpumpRPapers: Liu 2001
(PMID 11374601), Peyton 2008 (18292675) and 2011 (21270630), Nordmann 2006
(16613927) and McKay 2005 (15728054). Seven papers the code cites as read are
not in the archive (Liu 2001, Zhou 2002, Allott 1973, Antognini 1994, West 1996,
Severinghaus 1979, Williams & Leggett 1989); `PROMPT.md` section 9 lists them,
and the session that wrote the prompt has put their recording in
`papers/pending-work.csv` to Dr Shafer. The three swapped interaction-paper
files were refiled in their cards by #859; the PDFs on Drive are renamed by hand.
New here: the archived `PMID_10839909.pdf` (Minto 2000) is only the Web
Supplement, and the article itself is missing.
