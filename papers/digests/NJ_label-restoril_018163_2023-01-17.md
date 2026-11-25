---
file: NJ_label-restoril_018163_2023-01-17.pdf
pmid: none
doi: none
citation: "RESTORIL (temazepam) capsules USP, US prescribing information. NDA 018163; label dated 2023-01-17 (Medication Guide revised January 2023; Reference ID 5109842). https://www.accessdata.fda.gov/drugsatfda_docs/label/2023/018163s071lbl.pdf"
kind: label
drugs: [temazepam]
source_read: partial - text layer; boxed warning, Description, Clinical Pharmacology (Pharmacokinetics; Bioavailability, Induction, and Plasma Levels; Controlled Trials Supporting Efficacy), drug-interaction PK note (cimetidine), Dosage and Administration, How Supplied. Warnings, adverse reactions, abuse/dependence and medication guide not digested.
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
revised: 2026-10-27 (added the sleep-laboratory dose-response outcomes from Controlled Trials Supporting Efficacy, PDF p.4)
---

# Restoril (temazepam) label, January 2023

## Study
FDA label for temazepam capsules 7.5, 15, 22.5 and 30 mg. PK statements derive from an unnamed single- and
multiple-dose ADME study with 3H-labelled drug and from unnamed 30 mg single-dose and 7-day studies; subject
numbers, demographics and assay are not given.

Dosing (Dosage and Administration): usual adult 15 mg at bedtime; 7.5 mg may suffice (and in transient
insomnia); some need 30 mg; elderly or debilitated start at 7.5 mg. Taper on discontinuation.

## Model and parameters
No compartmental model. Clinical Pharmacology values:
```yaml
parameters:
  - {name: first-pass metabolism, value: 8, unit: "%", source: "Pharmacokinetics, p.2", note: "'minimal (8%)'; drug 'well absorbed'"}
  - {name: protein binding, value: 96, unit: "%", source: "p.2-3", note: unchanged drug}
  - {name: distribution (short) t1/2, value: "0.4-0.6", unit: h, source: "p.3", note: "biphasic decline of parent drug"}
  - {name: terminal t1/2, value: "3.5-18.4", unit: h, source: "p.3", note: "mean 8.8 h, depending on population and method"}
  - {name: metabolite formation t1/2, value: 10, unit: h, source: "p.3", note: "excretion t1/2 about 2 h; formation rate-limiting"}
  - {name: urinary recovery, value: "80-90", unit: "% of dose", source: "p.3", note: "O-conjugate 90% of metabolites, N-desmethyl O-conjugate 7%"}
  - {name: dose proportionality (AUC), value: "15-30", unit: mg, source: "p.3"}
  - {name: Cmax 30 mg capsule, value: 865, unit: ng/mL, source: "Bioavailability, Induction, and Plasma Levels, p.3", note: "mean; range 666-982"}
  - {name: Tmax 30 mg, value: 1.5, unit: h, source: "p.3", note: "mean; about 1.2-1.6 h; measurable at 10-20 min"}
  - {name: C 9 h, 30 mg nightly days 2-7, value: "260 +/- 210", unit: ng/mL, source: "p.3", note: "mean +/- SD; 7-day study, dose 1 h before retiring"}
  - {name: C 24 h, 30 mg nightly days 2-7, value: "75 +/- 80", unit: ng/mL, source: "p.3", note: "steady state (max trough) by the third dose; slight decline after day 4"}
```
- No active metabolites; no enzyme induction at 30 mg/day for 8 weeks; cimetidine does not alter PK.
- Absolute bioavailability is not stated as such. (derived: F = 0.92 if absorption is complete and 8% is lost
  to first pass.)

## Pharmacodynamics and end points
No plasma concentration-effect function, EC50 or therapeutic concentration range. The label does give
dose-response sleep outcomes (Clinical Pharmacology, "Controlled Trials Supporting Efficacy", PDF p.4; significance
pattern only, no effect magnitudes, n or P values printed). Dose 30 min before bedtime; these are dose-response
observations, not an effect-site model:
- Chronic insomnia, 2-week placebo-controlled sleep-laboratory studies, 7.5, 15 and 30 mg: linear dose-response
  improvement in total sleep time and sleep latency; significant drug-placebo differences at 2 weeks only for total
  sleep time at 15 and 30 mg and for sleep latency at 30 mg. REM sleep essentially unchanged, slow-wave sleep
  decreased; no measurable effect on daytime alertness or performance; no tolerance over at least 2 weeks;
  transient sleep disturbance after withdrawal of the higher doses.
- Transient insomnia (normal subjects, first-night adaptation), 24-hour placebo-controlled sleep-laboratory studies,
  same doses: linear dose-response improvement in total sleep time, sleep latency and number of awakenings;
  significant drug-placebo differences for sleep latency at all three doses, total sleep time at 15 and 30 mg, and
  number of awakenings at 30 mg only.

Not an opioid; boxed warning on concomitant opioids.

## Covariates
Elderly/debilitated: lower starting dose (no PK numbers given).

## Caveats
Study sizes, populations and assay unstated; 7-day concentrations highly variable (SD about the mean or larger).

## Used by stanpumpR
- R/drugs_temazepam.R:14-15 - 865 ng/mL peak vs the one-compartment Ochs reduction (max 296 ng/mL); distribution t1/2 0.4-0.6 h.
- R/drugs_temazepam.R:65 - distribution half-life comparison (model 0.88 h vs label 0.4-0.6 h).
- R/drugs_temazepam.R:81-82, :195 - bioavailability 0.92 from "minimal (8%) first pass metabolism".
- R/drugs_temazepam.R:92, :94 - checks: 30 mg peak 818 vs label 865 (666-982) at 1.5 h; day-7 217/82 vs 260 +/- 210 / 75 +/- 80.
- R/drugs_temazepam.R:154, :174 - reference entry.
- docs/sedatives-verification.md:44-45, :114, :120-121; docs/temazepam.md:6, 30, 32, 191, 209, 223, 238-240, 289;
  inst/help/drugs/temazepam.md:14, 18, 20; tests/testthat/test-drugs-temazepam.R:148; inst/help/models/absorption.md:33 - the same values.

## Verification against stanpumpR
- **30 mg mean peak 865 ng/mL, range 666-982, at mean 1.5 h** (R/drugs_temazepam.R:14, :92-93;
  docs/sedatives-verification.md:45; docs/temazepam.md:31-32, :238; help :14, :20): **matches**.
- **Distribution half-life 0.4-0.6 h** (R/drugs_temazepam.R:15, :65; docs/temazepam.md:33, :191;
  docs/sedatives-verification.md:45): **matches**.
- **Terminal t1/2 3.5-18.4 h** (docs/sedatives-verification.md:45): **matches** (mean 8.8 h not quoted but not contradicted).
- **Bioavailability 0.92 from "minimal (8%) first pass metabolism"** (R/drugs_temazepam.R:81-82, :195;
  docs/temazepam.md:223-224; help :18; docs/sedatives-verification.md:114): quote exact; F = 0.92 is a derived
  value that assumes complete absorption (label: "well absorbed"). **matches** (derivation reasonable).
- **30 mg nightly, days 2-7: 260 +/- 210 ng/mL at 9 h, 75 +/- 80 at 24 h** (R/drugs_temazepam.R:94;
  docs/temazepam.md:239-240; docs/sedatives-verification.md:121; help :20; test :148 comment "label 260, 75"):
  **matches**. The label's dose was taken 1 h before retiring; the repo's checks simulate exact 24 h
  intervals, which is equivalent.
- **No lag** (R/drugs_temazepam.R:86): label says measurable at 10-20 min; consistent with no or minimal lag.
- **inst/help/models/absorption.md:33** - "the label's bioavailability": **matches**.
- Reference entry (R/drugs_temazepam.R:154; docs/temazepam.md:289) gives no NDA or date; the archived label is NDA 018163, 2023-01-17.
- No discrepancies found.
