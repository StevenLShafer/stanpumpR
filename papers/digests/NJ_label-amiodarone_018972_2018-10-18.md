---
file: NJ_label-amiodarone_018972_2018-10-18.pdf
pmid: none
doi: none
citation: CORDARONE (amiodarone HCl) tablets, US prescribing information, NDA 018972 (Wyeth/Pfizer; made by Sanofi Winthrop Industrie), revised 10/2018, Drugs@FDA posting 2018-10-18 (Reference ID 4336449). https://www.accessdata.fda.gov/drugsatfda_docs/label/2018/018972s054lbl.pdf
kind: label
drugs: [amiodarone]
source_read: text layer, 15 pages; read Highlights, 2 Dosage and Administration, 8.5 Geriatric Use, 12.2 Pharmacodynamics, 12.3 Pharmacokinetics; skimmed warnings and Medication Guide
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
---

# CORDARONE tablets label 2018: oral amiodarone

## Study
US product label for 200 mg amiodarone HCl tablets (doses are mg of the hydrochloride). Elimination data from a
single-dose study in 12 healthy subjects and from patients after stopping chronic therapy (12.3); food study in 30
healthy subjects (600 mg).

## Model and parameters
Dosing (section 2, verbatim numbers): loading **800 to 1600 mg/day until initial therapeutic response (usually 1 to
3 weeks)**; then **600 to 800 mg/day for one month**; then maintenance **usually 400 mg/day**. Divided doses with
meals for >= 1000 mg/day; give consistently with respect to meals. No other maintenance range (e.g. 200 or 600
mg/day) is stated in this edition.

```yaml
parameters:
  - {name: F, value: 0.50, unit: fraction, source: "12.3 Absorption", note: "approximately 50%; slow, variable"}
  - {name: tmax, value: "3-7", unit: h, source: "12.3", note: "single dose"}
  - {name: Css_per_dose, value: 0.5, unit: mg/L per 100 mg/day, source: "12.3", note: "mean, chronic 100-600 mg/day, approximately dose proportional"}
  - {name: food_effect, value: "AUC x2.3 (1.7-3.6), Cmax x3.8 (2.7-4.4), Tmax -37%", source: "12.3", note: "600 mg after high-fat meal, n=30"}
  - {name: protein_binding, value: 96, unit: "%", source: "12.3 Distribution"}
  - {name: Vd, value: 60, unit: L/kg, source: "12.3 Distribution", note: "average, very variable"}
  - {name: t_half_single_dose, value: 58, unit: days, source: "12.3", note: "range 15-142, n=12 healthy; DEA 36 d (14-75)"}
  - {name: t_half_initial_patients, value: "2.5-10", unit: days, source: "12.3", note: "initial 50% fall after stopping chronic therapy"}
  - {name: t_half_terminal_patients, value: 53, unit: days, source: "12.3", note: "range 26-107, most 40-55; DEA about 61 d"}
  - {name: time_to_ss_no_load, value: 265, unit: days, source: "12.3", note: "range 130-535"}
  - {name: CL_young, value: 150, unit: mL/h/kg, source: "12.3 Effect of Age", note: "about; over 65 y about 100 mL/h/kg, t1/2 20 -> 47 d"}
```
Derived: 0.5 mg/L per 100 mg/day implies an apparent oral clearance of about 200 L/day; 150 mL/h/kg x 70 kg = 252
L/day. Metabolism CYP3A and CYP2C8 to desethylamiodarone (DEA); negligible renal excretion; not dialysable; renal
impairment no effect; grapefruit juice AUC +50%.

## Pharmacodynamics and end points
12.2: no well-established concentration-effect relationship; concentrations much below **1 mg/L** are often
ineffective and above **2.5 mg/L** generally not needed (plasma amiodarone; assay not stated). Antiarrhythmic effect
not before 2-3 days, usually 1-3 weeks even with loading; ventricular class III effect correlates more with DEA
accumulation. No ke0. Not an opioid; no MEAC.

## Covariates
Age (> 65 y lower clearance); severe LV dysfunction prolongs DEA t1/2; cirrhosis lowers DEA after IV.

## Caveats
Summary values from small studies; dose is the hydrochloride salt; the window is stated loosely ("much below",
"generally not needed"). The edition differs from the older monograph regimens Pollak 2000 simulated.

## Used by stanpumpR
- R/drugs_amiodarone.R:111 (header) and :211-214 (code) - band 1.0-2.5 mg/L "of the product monograph", typical 1.5
  (Pollak's target).
- R/help-scenarios.R:516-517 (now :681-691) - 'amiodarone-label-loading' scenario: 1600 mg/day PO at day 0, 800 at
  day 21 (30240 min), 600 at day 51 (73440 min).
- inst/help/drugs/amiodarone.md:32 and :46 - window; label regimens max 1600 (3 wk) / 800 (1 month) / 600 and min 800
  (1 wk) / 600 (1 month) / 400 mg/day.
- inst/help/scenarios/amiodarone-label-loading.md:3, :12 - the same max and min regimens.

## Verification against stanpumpR
- Band 1.0-2.5 mg/L (drugs_amiodarone.R:111, :213-214; help :32): label 12.2 gives 1 and 2.5 mg/L. **matches**
  (units mg/L, amiodarone not DEA, as the code says). The 1.5 mg/L typical is Pollak's, correctly attributed.
- Minimum regimen 800 mg/day x 1 week, 600 x 1 month, then 400 (help :46, scenario page :12): the label's lower
  bounds (800 load, shortest 1 week; 600 for one month; usual maintenance 400). **matches.**
- Maximum regimen 1600 x 3 weeks, 800 x 1 month, then **600 mg/day** (help-scenarios.R:687-689, help :46, scenario
  page :3): the 1600 x 3 weeks and 800 x 1 month are the label's upper bounds, but this 2018 label gives no 600 mg/day
  maintenance (only "usually 400 mg/day"). The 600 mg/day step must come from Pollak 2000's reading of an older
  monograph. **cannot verify from this edition** (not an error in the code, which attributes the regimen to Pollak's
  simulation; but "the label's high-dose loading regimen" in the scenario title is only partly supported by this
  label).
- Timing in the scenario: 30240 min = 21 d, 73440 min = 51 d = 21 d + 30 d; consistent with "3 weeks" then "one
  month". **matches** (unit conversion correct).
- Not a cited location, for context: the page's clearance 229 L/day is close to the label's dose-proportionality
  (about 200 L/day, derived) and its 252 L/day from 150 mL/h/kg (derived).
