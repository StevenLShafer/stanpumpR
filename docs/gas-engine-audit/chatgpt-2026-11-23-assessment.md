# ChatGPT's audit of the gas engine, 2026-11-23: assessment

ChatGPT (Codex) audited stanpumpR at `c90c33e` and GasMan at `d3a2dd3`, with the
prompt in `PROMPT.md` beside this file. Its report came back as
`gas-engine-audit-chatgpt-2026-11-23.md`; it quotes papers, so it is kept out of
the repository, like the replies of `docs/independent-extraction/`. Gemini's
audit was still running when this was written, so nothing here is yet an
agreement between the two.

Under the AGENTS.md rule on adversarial review, every finding is a lead. Each was
checked by Claude Code on 2026-11-23: the quoted lines at the pinned commit (the
gas code did not change between `c90c33e` and `23b87a2`, on which the fixes were
made), every number re-run with the package on R 4.3.3, GasMan's own source read
at `d3a2dd3` where a finding was about it, and the papers read on their PDFs in
stanpumpRPapers where a registry row depended on them.

**The pattern.** Unlike several earlier external reviews, nothing was invented:
every quoted line existed and said what the report said, and every number that
was re-run reproduced, most to the printed digit (the 24-hour first point,
50.8191 against 58.6768; the cold threshold crossing, 10.33 against 11.82
minutes; the MAC rows 0.805217 and 1.046942). Its errors were of reach: one
finding called a documented behaviour a defect (A-02's switch), and one called a
registry value that the registry's own vocabulary defines a mistake (B-03's
`not_applicable`).

## Summary

| ID | Report's type | Verdict | Done |
|---|---|---|---|
| A-01 | defect | Confirmed on GasDoc.cpp, in part | Three divergences fixed in both R restatements; the rest documented |
| A-02 | defect | Switch: not a defect (documented). Starvation: confirmed, small | Documented in the code |
| A-03 | enhancement | Confirmed to the digit | Fixed: graded sub-steps |
| A-04 | evidence gap | Confirmed | Fixed: GasMan's C++ output frozen as a fixture and asserted |
| B-01 | evidence gap | Agreed | Nothing changed; a proposal |
| B-02 | reopens decision | Values confirmed | Decided by Dr Shafer, 2026-11-23: Mapleson's values adopted |
| B-03 | evidence gap | Registry text: confirmed in part, plus one new error | Registry corrected; the MAC proposals are for Dr Shafer |
| C-01 | reopens decision | Numbers confirmed | `[decision]` for Dr Shafer, in `R/gasRecovery.R` |
| C-02 | defect | Confirmed, both parts | Fixed: the end-of-plot event; the band at the patient's age |
| C-03 | evidence gap | Not re-run | Nothing changed |
| C-04 | defect | Confirmed in mechanism; display only | Recorded, not changed |
| D-01 | defect | Confirmed | Fixed: the comment |
| D-02 | evidence gap | Not re-run | A proposal for Dr Shafer |
| E-01 | enhancement | Agreed | A proposal |
| F-01 | enhancement | Agreed | A proposal |
| (A-04) | stale text | Confirmed | Fixed: "GasMan is closed source" |
| (Papers) | archive | Confirmed on PubMed | Task suggested: three swapped PMIDs |

## The findings

### A-01: the R restatement of GasMan against GasMan's API

Checked against `gasman_api/gasmanAPI/GasDoc.cpp` at `d3a2dd3` (`Calc` and its
caller):

- **A fixed circuit's decay.** GasMan initialises `fExpCKT = 99` and does not
  advance a fixed (open or ideal) circuit, so only the alveolar decay can trip
  its fast-decay test there. Both R restatements tested the circuit's decay too.
  *Fixed* in `R/advanceGasManBaseline.R` and
  `inst/validation/gasman_baseline_standalone.R`.
- **The vernier clock.** GasMan computes `i = VERNIER_TICKS -
  int((dwt - m_dwBaseTime) / dx)` with `dwt` the END of the tick and the base the
  time of the settings row in force, so the test applies in the first two ticks
  after every settings row. The package routine applied it in the first three
  ticks of the run and never after a change, against its own comment; the
  standalone, whose settings never change, in three ticks. *Fixed* in both.
- **No fresh gas.** GasMan's effective delivered tension is `100 * vapour /
  (FGF * dt)`, and 0 with no flow; both R routines set an open circuit at no flow
  to the dial. *Fixed* in both.
- **Not restated**, and now said in both headers: liquid injection, the flush,
  the vaporiser's expansion of the flow (the standalone's applies it to every
  agent where GasMan applies it only to a liquid, and has no branch at 99% or
  more), the "closed" label (which GasMan runs through the semi-closed
  equations), GasMan's InitDocument and SetWeight defaults (the routines take
  resolved flows), and GasMan's whole-millisecond tick.

The five validation scenarios do not reach any of the three fixes at 70 kg, and
the restatement still matches the frozen C++ output to the same figures (A-04).
The report's further proposal, native API fixtures for injection, flush, open
circuit at no flow and an early and a late change that cross the 0.1 criterion,
needs `gasman_run` built again; it was not done here.

### A-02: oxygen consumption

- **`oxygenUptake = FALSE` keeps consuming oxygen.** Reproduced (20.93 to 11.13
  in a minute of apnoea), and documented: the argument's help says FALSE
  "restores the earlier behaviour, in which oxygen was a sink with no effect on
  volume", and `test-gas-engine.R` checks exactly that ("the old formula when
  switched off"). The switch is the volume coupling, as its documentation says.
  Not a defect; renaming an exported argument was declined.
- **Starvation within a step.** Confirmed in mechanism: whether oxygen is being
  consumed is decided at the start of a step, while the oxygen equation keeps
  its sink and is floored at zero. So once a supply below VO2 has emptied the
  lungs, the oxygen that arrives is consumed (by the floor) but its volume and
  its carbon dioxide are left out of the coupling: with 0.1 L/min of oxygen,
  0.02 L/min of net volume and 0.08 L/min of carbon dioxide, against an
  alveolar ventilation of 4.2 L/min. It arises only in a patient with no oxygen.
  Written into the code at `consuming`, and left as it is.

### A-03: accuracy that depended on the plot's length

Reproduced to the digit: on a 24-hour plot of 4 L/min nitrous oxide in 6 L/min
with 2% sevoflurane, the first point read 50.8191 against 58.6768 (-13.4%), with
errors above 1% to 9.6 minutes. The engine and its help already said so and
advised a short plot; the report's point was that it need not be so.

The coupled advance holds the summed uptake over each sub-step, and the uptake
changes fastest just after a change of setting: a nitrous oxide wash-in starts
with none in the lungs, so a first step held at that uptake takes none up. The
sub-steps now start at 0.01 minute at each change of setting and grow by 1.1
until they reach the plot's own step (`gasSubSteps()`), so each is about a tenth
of the time since the change; a row that repeats the settings does not restart
them, and the uncoupled advance keeps its uniform steps. The report's proposal,
an adaptive exponential midpoint, was not needed for this.

Measured against a reference starting at 0.0005 minute and growing by 1.02 to a
twentieth of the plot's step, at every plotted point of the induction, its
emergence and the same induction in a 4 kg infant, on plots of an hour, a day
and a week:

| Case | Plot | Largest error before | After | Steps |
|---|---|---|---|---|
| 70 kg induction | 1 h | 1.6% | 0.28% | 602 to 617 |
| 70 kg induction | 24 h | 13.4% | 0.36% | 602 to 650 |
| 70 kg induction | 7 days | 6.3% | 0.36% | 602 to 670 |
| 70 kg, emergence at 60 min | 24 h | 13.4% | 0.36% | 602 to 697 |
| 4 kg induction | 24 h | 5.8% | 0.59% | 602 to 650 |

The run time did not change measurably (about 0.7 s a run here). Growth 1.2 was
also measured (0.85% and 0.73% at most) and not chosen.

### A-04: the C++ reference was never asserted

Confirmed: `test-gas-scenarios.R` held the app engine to the R restatement, and
the restatement to nothing, so a number mistranscribed into both would have
passed; and `runGasEngineScenarios()`, run without GasMan's binary, wrote NA
over the only record of GasMan's output. The 520 values of the run of
2026-10-05 are now `inst/validation/gasman_cpp_fixture.csv`, which the script
reads when the binary is not given, and the test holds the restatement to it
within 0.1% of each compartment's peak (it is within 0.062%, fat, and under
0.004% elsewhere) and the engine within the native tolerance of 4%. The check
the report asked for: with sevoflurane's blood:gas coefficient raised 10% in
both R routines at once, the engine stays within 1.2% of the restatement in
scenario 1 (ideal circuit), which the old test passed, while the restatement
moves 1.3% to 8.4% of peak from GasMan's output, which the new check fails. The stale
statement that GasMan is closed source (the report's A-04) is corrected in
`R/gasProperties.R`, `R/advanceClosedFormGas.R` and the help page: its
repository is GPL-3.0 and holds the desktop and web application's source.

### B-01: body size

Agreed as an evidence gap; nothing changed. The engine scales every volume and
VO2 with weight and cardiac output and ventilation with weight to the 0.75, so
fat and lean tissue grow together. Weber 2021's obese virtual patients are a
sensitivity, not a calibration, and the report itself found Weber's printed
fat-flow equation inconsistent with its table. A body-composition model for the
gases would be its own decision for Dr Shafer.

### B-02: MAC at 40

The current values (sevoflurane 2.1, isoflurane 1.1, desflurane 6.0, nitrous
oxide 110) are GasMan's and the US labels' (Ultane, Forane, Suprane, read
2026-11-23). The age slope is Mapleson 1996's, whose own values at 40 are 1.80,
1.17, 6.6 and 104 (confirmed by `papers/digests/PMID_8777094.md`). Asked as a
`[decision]` at `MAC40`, and decided by Dr Shafer the same day: "Use Mapleson
1996 values. ... We validated against GasMan initially, but are now moving to
more recent research." Done: `MAC40` is 104, 1.80, 1.17 and 6.6; the default
thresholds a tenth of them (0.18, 0.117, 0.66); GasMan's values kept as
`GASMAN_MAC40`; the change is recorded on the help pages (*How stanpumpR
differs from GasMan*, the engine page, each agent's page) and in
VALIDATION.md, and every scenario page's MAC figure recomputed. At a given
tension sevoflurane reads 16.7% more MAC equivalents, isoflurane 6.0% fewer,
desflurane 9.1% fewer and nitrous oxide 5.8% more; the tensions themselves do
not change.

### B-03: pregnancy and infant MAC

- **The registry text** (`data-raw/pregnancyRegistry.R`, four inhaled-agent
  rows): each said Palahniuk 1974 was "read in full" and later "(Palahniuk 1974,
  title only)", and gave its stimulus as a tail clamp where the paper's is a
  clamp on the ear for a minute; both were already found by the paper's card
  (`papers/digests/PMID_4857789.md`) and never fixed. And the isoflurane row
  gave Chan & Gin 1995's early postpartum MAC as 0.775%, the abstract's figure:
  the Results, read on the PDF, give 0.725% (0.675-0.775, n = 5), the abstract
  printing the top of that range as the median. Zhou 1995 (0.75 against 1.04%)
  and Chan 1996 (27% and 30%) were read on their PDFs and are right. All
  corrected, and the CSV rebuilt.
- **`not_applicable`** for the inhaled agents is not a defect: the registry's
  vocabulary defines it to include "an inhaled anesthetic (whose pregnancy change
  is in MAC, a pharmacodynamic effect)". The rows' `evidence_read` stays
  `abstract`, since Strout 1981 and Mazze 1985 have not been read.
- **A pregnancy MAC factor** (isoflurane 0.721 at 8-12 weeks, Gin 1994) and an
  **infant MAC curve** (Mapleson's relation is invalid below 1 year, which
  `macForAge()` and its help already say) are proposals for Dr Shafer.

### C-01: the default threshold and body temperature

Reproduced: at 32 C, 0.21% sevoflurane is 0.130 of the cooled MAC (1.615%), and
the vessel-rich group falls to it in 10.33 minutes after 30 minutes at 2%,
against 11.82 to a tenth of the cooled MAC and 9.94 at 37 C (with GasMan's MAC; with Mapleson's, adopted the same day, 11.19 to the 0.18% default, 12.76 to 0.138%, and 10.74 at 37 C). The thresholds were
decided on 2026-11-16 to be typed tensions that do not move; the question is
whether the default should instead be a fraction of MAC resolved at the
temperature in force. Recorded as a `[decision]` in the thresholds section of
`R/gasRecovery.R`.

### C-02: the band and an event at the end of the plot

- **An event at the end of the plot.** Reproduced: a temperature event at the
  plot's last minute was never taken, because the loop over intervals stopped
  one change point short, so the last point and the time until threshold read
  before it. *Fixed*: the loop takes a physiology step at the last change point
  as at any other, the same time recorded before and after it.
- **The band.** Reproduced: a volatile agent's band was drawn at a 40-year-old's
  MAC whatever the patient's age (2.1% sevoflurane where an 80-year-old's MAC is
  1.64%), while the MAC series and the default thresholds follow age and the
  help says "the MAC band ... follow[s]" MAC. *Fixed*: `gasTypicalBand()` takes
  the age. A band is one range for the whole plot, so with a temperature event
  it stays at the temperature in force at time 0; said in the code.

### C-03: the endobronchial tube's ventilated volume

An evidence gap about the ventilated share of the alveolar volume and the 50%
shunt, both recorded decisions of 2026-11-19. Not re-run; nothing changed.

### C-04: the circuit's humidity at altitude

Confirmed in mechanism: the circuit tension the engine stores and the Visualizer
draws is fresh gas at its dry partial pressure plus rebreathed gas, while the
alveoli receive the fresh gas humidified; at 3000 m the two differ by 2.7% of
the fresh part. The engine's alveolar inflow is consistent, and nothing plotted
on the main panels reads the circuit; the help page states the humidification.
Display only, and at altitude only. Recorded, not changed.

### D-01: the opioid-adjusted time until threshold

Confirmed: the comment in `applyOpioidMacInteraction()` said holding the
opioid's effect fixed always errs long; the help page says, rightly, that the
error has no fixed direction (it reads short while the opioid's effect-site
concentration is still rising). *Fixed*: the comment.

### D-02, E-01, F-01: proposals

- **D-02.** The opioid-MAC model is a recorded placeholder. The report proposes
  a movement-to-incision model fitted by drug and agent, starting from
  Vereecke 2013's sevoflurane-fentanyl fit with and without 66% nitrous oxide,
  and not scaling the nitrous oxide share by the opioid. Its arithmetic was not
  re-run here.
- **E-01.** Expose the arterial tension and each agent's uptake flux as
  diagnostic outputs, and keep a mass balance (delivered, taken up, stored,
  vented) for future conservation tests.
- **F-01.** A two-region ventilation-perfusion lung, tested against Peyton 2008
  and 2011's paired arterial and end-tidal measurements, which a single
  well-mixed lung cannot produce; it must reduce to the present lung when the
  regions are alike.

## For Dr Shafer

1. **B-02**: decided 2026-11-23, Mapleson's values (above).
2. **C-01**: the default threshold of a volatile agent as a fraction of the MAC
   in force at the patient's temperature, or a tension, as decided on
   2026-11-16?
3. Whether any of the proposals (B-01, B-03, D-02, E-01, F-01) is wanted, and in
   what order. The report ranks them: the fixtures and integration accuracy
   first (done here), then oxygen balance, the threshold units, the MAC
   intercepts, the V/Q prototype, the opioid model, body size, infant and
   pregnancy MAC, and last an explicit circuit, an EEG effect site and a slow
   tissue state.

## Papers

The report read four papers from outside stanpumpRPapers, which would belong in
it if F-01, C-01 or B-02 goes ahead: Peyton 2008 (PMID 18292675) and Peyton
2011 (21270630), the paired arterial and end-tidal fixtures for F-01; Liu 2001
(11374601), the hypothermia MAC regression the engine already uses; and
Nickalls & Mapleson 2003 (12878613), the age-adjusted MAC sum. Its full list of
papers to add is in the report.

It also found that three files of the archive and their cards carry each other's
PMIDs: `PMID_22222473` holds Hendrickx 2008 (18633028), `PMID_19741484` holds
Heyse 2012 (22222473), and `PMID_18633028` holds Schumacher 2009 (19741484).
Confirmed on PubMed and against the cards' citations; nothing in the code cites
them. The archive belongs to the digest pipeline, so the fix was suggested as a
task of its own rather than made here.
