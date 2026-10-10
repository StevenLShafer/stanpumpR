# Design note: MEAC exposure as a replacement for oral morphine equivalents

Status: **proposal, not implemented.** Drafted 2026-10-10 at the request of
Steven L. Shafer. Nothing in this note changes the app; it describes what a
future change would build, what it depends on, and what is still unresolved.

---

## 1. Summary

Oral morphine milligram equivalents (MME) normalise opioids by **dose**: a fixed
ratio per drug and route, multiplied by the milligrams prescribed. The ratio
ignores time course, route-specific kinetics, active metabolites, organ
function and body size, and its provenance is mostly single-dose
relative-potency studies and consensus.

stanpumpR can already normalise opioids by **exposure**. It computes
U(t) = Σ Ce_i(t) / MEAC_i, the effect-site opioid level in multiples of each
drug's minimum effective analgesic concentration. This note proposes four
additions on top of that:

1. **Exposure metrics** (§4.1): mean, peak and trough U over a dosing day, and
   time above thresholds. These are the exposure-based replacement for
   "MME/day".
2. **Derived conversion ratios** (§4.2): an MME-style ratio calculated from
   MEAC, clearance and bioavailability for a given patient and route, instead
   of looked up in a table.
3. **A rotation solver** (§4.3): find the dose of a new opioid, route and
   frequency that reproduces a stable patient's current exposure. Because
   kinetics are linear, this needs no iteration.
4. **A respiratory scale** (§4.4): a second, separate normalisation, R(t),
   against a respiratory-depression potency. Safety reporting uses R(t), not U.

The proposal is aimed at **within-patient** use (rotation, tapering, comparing
regimens) rather than population thresholds, for the reasons in §6.

---

## 2. The problem with MME

- **No time course.** 60 mg of oxycodone as a single dose and as 15 mg four
  times a day have the same MME and very different peak exposure.
- **One ratio per drug.** Renal function (morphine, hydromorphone metabolites),
  CYP2D6 genotype (codeine, tramadol), body size and age are invisible to MME.
- **Prodrugs and metabolites.** Codeine and tramadol are assigned a fixed
  potency even though their effect comes from a metabolite whose formation
  varies many-fold between patients.
- **Asymmetry and nonlinearity.** Published methadone ratios change with dose.
  This is largely pharmacodynamic (incomplete cross-tolerance), but MME treats
  it as a fixed potency.
- **Inconsistent sources.** Equianalgesic tables disagree by up to two-fold for
  hydromorphone, oxymorphone and transdermal fentanyl, and the CDC changed
  several factors between 2016 and 2022.

---

## 3. What already exists

| Piece | Where | Notes |
|---|---|---|
| Per-drug MEAC | `inst/extdata/drugDefaults_global.csv`, `MEAC` column; mirrored in each `R/drugs_<name>.R` | Ten opioids; values and provenance in `inst/help/models/meac.md` and the drug pages |
| % MEAC per drug | `simCpCe()` writes `equiSpace$MEAC` | Effect-site based |
| Total opioid U(t) | `totalOpioidMEAC()` in `R/opioidMacInteraction.R` (exported) | Additive sum on a common grid |
| Metabolite scoring | `R/mergeMetabolite.R`, `foldMetabolites()` | Codeine → morphine, tramadol → desmetramadol, oxycodone → oxymorphone; the parent's own MEAC is 0 where its effect is the metabolite's |
| Repeated dosing | `R/scheduled.R` (`qd`/`bid`/`tid`/`qid`) | Repeats to the end of the plot |
| Extravascular routes | `advanceClosedFormPO_IM_IN.R`; `ka_*`, `bioavailability_*`, `tlag_*` in drug functions | PO for oxycodone, hydromorphone, codeine, tramadol, oxymorphone; IM/IN for hydromorphone |
| Covariates | drug functions, `pkSizeFactors()`, `R/renalFunction.R` | Size scaling is mandatory; renal terms only where the source has them |
| Shiny-free driver | `simulateDrugsWithCovariates()` | Entry point for the API in §5 |
| Inverse solver precedent | `R/tci.R` | Closed-form; same "unit response" idea used in §4.3 |

---

## 4. Proposal

### 4.1 Exposure metrics

For a regimen simulated to steady state (or over any window the user picks), let
U(t) be the total opioid level from `totalOpioidMEAC()`. Over a window
[t0, t0 + T], usually the last full 24 h:

| Metric | Definition | Reads as |
|---|---|---|
| **Mean U** (`Ubar`) | (1/T) ∫ U dt | The exposure analogue of MME/day |
| **Peak U** | max U | Peak effect; relevant to sedation and respiratory risk |
| **Trough U** | min U | Breakthrough pain risk |
| **Peak:trough ratio** | Peak / trough | How "spiky" the regimen is |
| **Time above 1 MEAC** | fraction of T with U ≥ 1 | Time with adequate analgesia |
| **Time above threshold** | fraction of T with R ≥ R_crit (§4.4) | Respiratory exposure |

**Steady state.** For linear kinetics, the mean concentration at steady state
is F × (dose rate) / CL whatever the ke0 or dosing interval, so `Ubar` can be
checked against a closed form. The window is "steady state" once two
consecutive 24 h means agree within a tolerance (proposed 2%). Methadone
(terminal half-life above a day) may need a week or more of plot; the summary
must state whether steady state was reached rather than silently reporting a
transient.

**Name.** Avoid "morphine equivalents" so the two are not confused. Proposed
display label: **"MEAC-days"** for ∫U dt over 24 h (numerically equal to
`Ubar`), with "mean MEAC multiple" as the plain-language gloss.

### 4.2 Derived conversion ratios

At steady state, the dose rate that holds drug i at one MEAC is

    D_i = MEAC_i × CL_i / F_i,route

and the equianalgesic ratio between drugs i and j is D_j / D_i. Every term is
already in the drug function for the patient being simulated, so the ratio is
**specific to the patient and the route**. It changes with size, age and
creatinine wherever the model has those covariates.

Values from the current library for a 70 kg, 170 cm, 40-year-old male
(`adjustToFFM = TRUE`), computed 2026-10-10:

| Drug | CL (L/min) | MEAC | 1 MEAC, IV (mg/day) | F_PO in model | 1 MEAC, PO (mg/day) |
|---|---|---|---|---|---|
| morphine | 1.23 | 8 ng/mL | 14.2 | *none* | (≈ 43 with an assumed F of 0.33) |
| hydromorphone | 1.30 | 1.5 ng/mL | 2.8 | 0.6 | 4.7 |
| oxycodone | 0.62 | 12 ng/mL | 10.8 | 0.5 | 21.5 |
| oxymorphone | 2.00 | 0.8 ng/mL | 2.3 | 0.1 | 23.0 |
| methadone | 0.10 | 60 ng/mL | 8.8 | *none* | (≈ 11 with an assumed F of 0.8) |
| pethidine | 0.76 | 250 ng/mL | 275 | *none* | — |
| fentanyl | 0.63 | 0.6 ng/mL | 0.55 (23 mcg/h) | — | — |
| alfentanil | 0.20 | 39 ng/mL | 11.2 | — | — |
| sufentanil | 0.92 | 0.056 ng/mL | 0.074 | — | — |
| remifentanil | 2.53 | 1 ng/mL | 3.65 | — | — |
| oliceridine | 0.53 | 27.9 ng/mL | 21.2 | — | — |

The implied MME conversion factors (oral morphine mg per mg of drug, taking
1 MEAC ≈ 43 mg/day oral morphine) against the CDC 2022 table:

| Drug, route | MEAC-derived | CDC 2022 | Comment |
|---|---|---|---|
| oxycodone PO | 2.0 | 1.5 | Reasonable agreement |
| hydromorphone PO | 9.1 | 5 | **Disagrees ~2×**: hydromorphone MEAC (1.5 ng/mL) may be low, or F/CL off |
| oxymorphone PO | 1.9 | 3 | MEAC is provisional (a tenth of morphine's) |
| methadone PO | ≈ 3.9 | 4.7 | Within the published range |
| fentanyl TD (mcg/h) | ≈ 1.9 | 2.4 | Ignores the patch's absorption kinetics |

This comparison is useful in itself: **the disagreements point to MEAC values
or kinetic parameters worth re-examining**, hydromorphone first. It should
become a pinned test (§7) so a change to any MEAC or clearance shows up as a
change in its implied ratio.

Note that the reference drug, oral morphine, **has no oral route in
stanpumpR**, so the "≈ 43 mg/day" anchor uses an assumed bioavailability. Adding
oral morphine is a prerequisite (§5.3).

Calibration point: 50 MME/day (the CDC "reassess" threshold) is about 1.2 MEAC
of continuous exposure and 90 MME/day about 2.1 MEAC, for this reference
patient. These numbers belong in the help page only as an orientation aid, not
as thresholds.

### 4.3 Rotation solver

**Question.** A patient is stable and comfortable on regimen A, for example
oxycodone 20 mg PO tid. What dose of drug B, by route r at frequency f,
reproduces their exposure?

**Personal MEAC cancels.** Let the patient's true MEAC be m × the population
MEAC, with m the same for every μ agonist (an assumption; see §6). Their
personal exposure is U/m on both regimens, so matching population U matches
personal U. Rotation needs the *ratio* of MEACs to be right, not their
absolute values, which is the single strongest argument for the approach.

**Closed form.** Kinetics are linear in dose (no opioid declares an
`oralSaturation` block), so for a fixed schedule shape U_B(t) = d × u_B(t),
where u_B is the response to a unit dose on that schedule. Then:

- **Match mean:** d = Ubar_A / ubar_B
- **Match trough:** d = min U_A / min u_B
- **Match peak:** d = max U_A / max u_B

One simulation of A and one unit-dose simulation of B give the answer; no
iteration. This mirrors the "unit-rate response" trick in `R/tci.R`.

**Cross-tolerance discount.** Incomplete cross-tolerance is pharmacodynamic and
the model cannot know it. The solver takes an explicit discount δ (dose_B ×
(1 − δ)) that the user must see and set. The default is an open question
(§8). The output always shows the undiscounted dose alongside the discounted
one.

**Transition plans (later).** Because the engine simulates arbitrary dose
tables, a rotation can be shown as a cross-taper. A's doses stop at T and B's
start at T or are stepped in over days, with U(t) through the transition on
the MEAC panel. No new engine work is needed; only a helper that writes the
dose table.

**Rounding.** Solved doses are rounded to available tablet strengths, with the
resulting Ubar and trough shown after rounding. A tablet-strength list per
drug would be a new CSV or a new column.

### 4.4 A separate respiratory scale, R(t)

MEAC is an analgesic potency. Respiratory depression has its own potency, and
the ratio of the two is not the same across opioids (it is the premise of
oliceridine, whose library MEAC was itself chosen from a respiratory end point).
Using U for safety makes both mistakes at once.

Proposal:

- A new CSV column, provisionally `RC50`: the effect-site concentration giving
  a 50% fall in the ventilatory response to CO2 (or a 50% reduction in minute
  ventilation at fixed end-tidal CO2), whichever end point the sources share.
  It is 0 for non-opioids, like `MEAC`.
- R(t) = Σ Ce_i(t) / RC50_i, computed exactly as `totalOpioidMEAC()` computes U.
- Displayed as its own series, so the "therapeutic index" of a regimen is
  visible as the gap between U and R.
- Interaction with sedatives (benzodiazepines, gabapentinoids) is out of scope
  for the first version but is the obvious extension, through the same
  response-surface machinery as `R/opioidMacInteraction.R`.

Data for `RC50` are sparse; §8 lists what is needed. Until a drug has an `RC50`,
the safety metrics are reported as unavailable for it, not computed from MEAC.

---

## 5. Implementation sketch (for when this goes ahead)

### 5.1 Shiny-free API

Tentative names, all built on `simulateDrugsWithCovariates()`:

- `opioidExposure(drugs, window = c(start, end))`: returns `Ubar`, peak,
  trough, time above 1 MEAC, `steadyState` (logical) and, where available, the
  R-based metrics.
- `equianalgesicDose(drug, route, covariates, target = 1)`: the D_i of §4.2
  for one patient.
- `rotateOpioid(current, to = list(drug, route, frequency), covariates,
  match = c("mean", "trough", "peak"), discount)`: §4.3.

All return minutes and the drug's own units, consistent with the rest of the
engine (`docs/architecture.md#time-units`).

### 5.2 App

- Under the % MEAC panel, a small summary table of the §4.1 metrics for the
  last 24 h of the plot, with a steady-state flag.
- A **Rotate** dialog: choose the target drug, route and frequency, the match
  criterion and the discount; it writes the new regimen into the dose table
  through `setDoseTable()` so the user can see both on the plot.
- `exportDoseTable()` gains the summary metrics.
- A help page, `inst/help/models/meac-exposure.md`, linked from
  `models/meac`, with a scenario showing an oxycodone → hydromorphone rotation.

### 5.3 Library gaps to close first

| Gap | Why it matters |
|---|---|
| **Oral morphine** (`ka_PO`, `bioavailability_PO`) | It is the reference drug of the scale being replaced |
| **Oral methadone** | The rotation everyone gets wrong; long half-life also tests the steady-state logic |
| **Morphine-6-glucuronide** | The renal story for morphine; needs the two-stage metabolite cascade the engine lacks (`R/getDrugPK.R`, ~line 567) |
| **Transdermal fentanyl** | A zero-order input with a skin depot; common in chronic use |
| **Hydromorphone MEAC** | Implied ratio twice the CDC's (§4.2) |
| **Oxymorphone MEAC** | Marked provisional in the drug file |
| **Buprenorphine, tapentadol** | Not in the library and do not fit additivity (§6); exclude explicitly rather than guess |

---

## 6. Limitations

- **Tolerance makes absolute U uninterpretable in chronic patients.** MEACs
  come from opioid-naive postoperative PCA. A tolerant patient's m may be 5 or
  10. Absolute `Ubar` is therefore a measure of drug on board, not of
  analgesia or risk, across patients. Within one patient it is sound.
- **m is assumed to be the same for every μ agonist.** Incomplete
  cross-tolerance says it is not; the discount δ is a blunt correction.
- **Additivity.** Reasonable for full μ agonists. Wrong for partial agonists
  (buprenorphine: ceiling, slow dissociation, displacement of full agonists)
  and incomplete for mixed-mechanism drugs (tramadol's and tapentadol's
  monoaminergic effects). Such drugs are excluded from U, with a visible note
  saying so.
- **The effect site matters less in chronic dosing.** At steady state, mean Ce
  equals mean Cp. ke0 shapes the peaks and troughs, not `Ubar`.
- **MEAC uncertainty.** Several values are compromises between discordant
  sources. A later version should carry a range per MEAC and show U as a band.
- **Not a prescribing tool.** Results are simulations for a typical patient
  with the covariates entered. The UI and help must say so wherever a dose is
  proposed.

---

## 7. Validation and tests

1. **Closed-form check.** For each opioid with a PO route, the simulated
   steady-state `Ubar` matches F × dose rate / (CL × MEAC) within 1% (new test
   file `test-meac-exposure.R`).
2. **Pinned ratios.** The §4.2 table, pinned with `expect_equal_rounded()`, so
   a change to any MEAC, clearance or F shows up as a change in its implied
   ratio and has to be acknowledged.
3. **Linearity of rotation.** `rotateOpioid()` applied to A → B → A returns the
   original dose (with δ = 0).
4. **Literature comparison.** Implied ratios against published equianalgesic
   tables, with each discrepancy over 1.5× either explained in the drug page or
   triggering a review of the MEAC.
5. **Outcome validation (research, not software).** Using prescription data
   simulated with typical covariates, does `Ubar`, peak U or time above an R
   threshold predict overdose or respiratory events better than MME? This is
   what would justify using the scale outside stanpumpR.

---

## 8. Open questions

1. **Default cross-tolerance discount.** Zero (fully explicit), or the
   conventional 25–50% with a warning? Should it differ for methadone?
2. **Which match criterion is the default for rotation?** Mean is the natural
   MME replacement; trough is closer to "keep them comfortable"; peak is the
   safety view.
3. **Source and end point for `RC50`.** Ventilatory response to CO2, minute
   ventilation at fixed end-tidal CO2, or the Dahan-group "ventilation at
   isohypercapnia" models? Which opioids have usable data?
4. **Hydromorphone.** Revisit MEAC, clearance or F, or accept the discrepancy
   and document it?
5. **Should the MME-equivalent be shown at all?** Showing "≈ N MME/day" beside
   `Ubar` eases adoption but keeps the old concept alive. One option is to show
   it only in export, labelled as derived.
6. **Tablet strengths.** A new data file, or out of scope for a teaching and
   simulation tool?
