# ChatGPT's round-2 audit of the gas engine, 2026-11-26: assessment

ChatGPT (Codex) audited stanpumpR at `8c1ea1b` and GasMan at `d3a2dd3` with
`PROMPT-2.md`, the verification audit before James Philip is asked to review
the engine. Its report came back as `gas-engine-audit-round2-chatgpt-2026-11-26.md`
and is kept out of the repository, like round 1's. Gemini's round-2 report had
not come back when this was written.

Under the AGENTS.md rule on adversarial review, every finding is a lead. Each
was checked by Claude Code on 2026-11-26: every quoted line at `8c1ea1b`, every
number re-run with the package (R 4.3.3), and both GasMan defect claims and
the vapour-volume figure re-run on a `gasman_run` built from d3a2dd3 (g++ 13.3).

**The pattern.** The best external report on this code so far. Every quoted
line existed and said what the report said; every number re-run reproduced to
the printed digits (2.374424 L, 0.599937 L, 88.243%, 7.0863 against 8.8555
L/min, 6.30 L of which 4.68 in the tissues, 0.731791 and 0.64418, 5.04343). It
built GasMan's command-line tool and shared library itself, which no earlier
reviewer had done, and its "checked, correct" tables are as useful as its
findings. Its one error of reach: it called B-02 "patient-facing" because the
app supplies a default ventilation, but the app never runs the semi-closed
circuit (`simulateGases()` in `app_server()` takes the default, ideal); B-02
and B-03 are reached only from R. Its severity labels follow the prompt's rule
that anything untrue about GasMan is S1, so several S1 items are one sentence.

**Ready for James Philip?** Its verdict was "yes, after the blocking items".
With the fixes below in, and B-02 decided the same day, none is left.

## Summary

| ID | Report's type and severity | Verdict | Done |
|---|---|---|---|
| A-01 | wrong statement, S1 | Confirmed: the header said MAC equals GasMan's | Fixed |
| A-02 | wrong statement, S1 | Confirmed: GasMan's API keeps its 70 kg VA and CO | Fixed (roxygen and the scenario label) |
| A-03 | wrong statement, S1 | Confirmed: GasMan's 8 L circuit does not scale; mine, in #941 | Fixed |
| A-04 | defect, S2 | Confirmed, the standalone validation script only | Fixed: reports the expanded flow |
| A-05 | test gap, S3 | Confirmed; both claims reproduced natively here | Inputs and commands added |
| A-06 | wrong statement, S3 | Confirmed (a test comment) | Fixed |
| A-07 | wrong statement, S2 | Confirmed: the bullet omits its settings; its 17.6% reads 4 L/min as minute ventilation | Settings stated, with the app's own 1.11 and 1.73 |
| A-08 | wrong statement, S3 | Confirmed: 5.04, not 5.05 | Fixed |
| B-01 | wrong statement, S3 | Production value reproduced; the reference accepted | Reworded: under 0.7% |
| B-02 | defect, S1 | Confirmed to the digit; R only, not the app | Decided: the semi-closed circuit takes no physiology GasMan lacks |
| B-03 | wrong statement, S1 | Confirmed (88.243%); R only | Help scoped to the ideal circuit |
| D-01 | wrong statement, S1 | Confirmed; #941 missed this page | Fixed |
| D-02 | wrong statement, S3 | Confirmed; #940 missed this page and a comment | Fixed |
| D-03 | wrong statement, S2 | Confirmed: the tables are with 2% sevoflurane | Settings stated |
| D-04 | wrong statement, S3 | Confirmed, all four | Fixed |
| D-05 | wrong statement, S1 | Confirmed: 6.3 L is held, 4.7 L taken up | Fixed |
| E-01 | defect, S2 | Confirmed, and wider: also the arterial values and the steps sent | Fixed, with tests |

Its "checks that did not produce findings" also carried four small
corrections, all made: the standalone script's header still called ART = ALV
an unchecked assumption (GasMan's `GetART()` returns ALV), did not list ether
and xenon as not restated, and did not say it follows GasMan's two-tick
history clock; `VALIDATION.md` overread a `GasDoc.cpp` comment; the help's
"line-by-line transcription" overstated a restatement with named omissions;
and the help said GasMan's code is "published under the GPL-3.0" where its
repository's licence is GPL-3.0 and its API's README says MIT.

## The defects fixed

**E-01, the Visualizer and a physiology event outside its window.** A gas run
was refused if the body temperature changed anywhere in the simulation, so a
cooling event at 120 minutes refused a window of 0 to 60 drawn entirely at
37 °C, and a window of 30 to 60 wholly after a cooling event at 20 was refused
although its temperature never changed. Beside it, as the report found: a tube
put in after the window sent arterial values for a window with no shunt, and
the physiology steps sent ran past the window's end. `viewerGasRun()` now
takes the bodies in force inside the window (the one at its start, then each
later step before its end), draws the tissues' capacities at that window's
temperature, sends the arterial values only where a tube in the window
shunts, and `viewerPhysiologySteps(end =)` stops at the window's end (for the
intravenous drugs too). Tests `test-gas-audit-chatgpt-r2.R`, each failing at
`8c1ea1b`.

**A-04, the standalone script's FGF.** With `vaporizer_effect = TRUE` it
delivered at the expanded flow and reported the setting; it now reports what
GasMan's `GetFGF()` reports. The default, `FALSE`, and every comparison were
unaffected.

## The statements corrected

A-01, A-02, A-03, A-06, A-07, A-08, B-01, B-03, D-01 to D-05 and the four
above are sentences on the help pages, in the code's comments or in the
validation record. Three were written in this week's pull requests: A-03 (the
body-size row of `gas-differences.md`, #941), D-01 (the cardiac output on the
inhaled-agents page, #941) and D-02 (the threshold line on the same page,
#940). The body-size row now also says what the report's Part F asked: Weber
and colleagues scaled the fat group's flow and held the lean groups, so
scaling the lean groups by fat-free mass is stanpumpR's own choice.

**A-05.** `inst/validation/gasman_defects/` holds the two inputs and
`VALIDATION.md` the commands, with `--every 6`, without which `gasman_run`
prints every 10 seconds and never 1:18. Rerun here: 0.731791 and 0.64418 for
the end times 84 and 180 s; 0.789155 for `Semi`, 0.72834 for `Semi-closed` and
`Closed`, 1.2494 for `Ideal`.

## For Dr Shafer

1. **B-02, decided 2026-11-26.** At a step down in altitude,
   `gasStateAcrossPressure()` scaled the semi-closed circuit's own gas up and
   filled the lungs from it, so gas appeared with no source: 2.37 L of
   nitrogen at no flow and no ventilation (3000 m to sea level), 0.60 L at 0.5
   L/min of oxygen. The app never runs this circuit; it is GasMan's, kept for
   the comparison, and GasMan has no altitude. Dr Shafer decided more widely
   than the finding: "If the semi-closed system is only used for comparison
   with GasMan, then we should not permit it to reflect any physiology not
   available to GasMan." `advanceClosedFormGas()` now refuses the semi-closed
   circuit a temperature other than 37 °C, an altitude, an endobronchial tube,
   the cardiac-output slider, a haemoglobin other than the default, and body
   composition (`gasNotGasMansBody()`); GasMan's own cardiac output setting
   and linear scaling by weight stay. Tests in `test-gas-audit-chatgpt-r2.R`.
2. **GasMan's licence.** Its repository's root `LICENSE` is GPL-3.0; its API's
   README (line 572) says "Code released under the MIT License". Worth
   mentioning to Dr Philip with the two defect reports. stanpumpR copies no
   GasMan code either way.

Recorded, not changed: in a legal but impossible patient (150 kg, 10 cm, age
0, cardiac output 0.1, 7000 m, a 50% shunt, 0.5 L/min of fresh gas) the
fat-free mass is 0.38 kg, the lungs 9 mL and their time constant 0.002 minute,
so the first 0.01-minute step spans five of them and the oxygen reaches its
floor within it while the uptake is held, and the alveolar total exceeds the
humidified dry gas by 3 percentage points for that step. No plausible patient
was shown to reach it.

## Papers

The report read the relevant pages of Weber 2021 (PMID 32067148), Al-Sallami
2015 (25940825) and Mapleson 1996 (8777094) on their PDFs. Its readings agree
with the code: Mapleson's four MAC values at 40 and the exponent −0.00269, and
Al-Sallami's equations 6a and 6b, which the authors warn against using below
age 3 (the engine extrapolates them to newborns; recorded).
