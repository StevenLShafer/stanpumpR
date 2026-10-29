---
file: NJ_label-sensorcaine_018304_2018-11-05.pdf
pmid: null
doi: null
citation: "FDA label. Sensorcaine / Sensorcaine-MPF (bupivacaine HCl injection, USP), with and without epinephrine 1:200,000. Fresenius Kabi. NDA 018304, label 451106G revised November 2018 (FDA action 2018-11-05; Reference ID 4344087). https://www.accessdata.fda.gov/drugsatfda_docs/label/2018/018304s049lbl.pdf"
kind: label
drugs: [bupivacaine]
source_read: "text layer, whole label (22 pages; old-format label without numbered sections: Description, Clinical Pharmacology/Pharmacokinetics, Indications, Warnings, Precautions, Adverse Reactions, Overdosage, Dosage and Administration with Table 1, How Supplied); page images rendered and read for PDF pages 4, 16 and 18"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: matches
---

# Sensorcaine label 2018: bupivacaine HCl injection (NDA 018304)

## Study
Not a study: the US prescribing information for Sensorcaine (bupivacaine HCl) injection, with and without
epinephrine 1:200,000 (5 mcg/mL), multiple-dose (methylparaben) and MPF single-dose forms, in 0.25%, 0.5% and
0.75% strengths. Old-format (pre-PLR) label; printed page numbers equal PDF pages 1-21; PDF page 22 is the FDA
signature page. No population PK study is described; the PK statements are generic class text.

## Model and parameters
No compartmental parameter values are given. The label states only (PDF p3-4, printed p3-4):
- IV PK described as a three-compartment open model (qualitative, p3).
- After caudal, epidural or peripheral nerve block, peak blood levels at 30 to 45 minutes, declining to
  insignificant levels over the next 3 to 6 hours (p3-4).
- Half-life 2.7 h in adults, 8.1 h in neonates (p4; confirmed on page image).
- Protein binding 95%; fetal/maternal ratio 0.2 to 0.4 (p3).
- pKa 8.1 (p1). Only 6% excreted unchanged in urine; major metabolite pipecoloxylidine (p4).
- Elderly: faster maximal spread, higher peak plasma concentrations, decreased total plasma clearance (p4; no
  numbers).

```yaml
parameters:
  - {name: t_half_adult, value: 2.7, unit: h, source: "PDF p4 Clinical Pharmacology", note: "label statement; no study cited"}
  - {name: t_half_neonate, value: 8.1, unit: h, source: "PDF p4 Clinical Pharmacology", note: "label statement"}
  - {name: protein_binding, value: 95, unit: "%", source: "PDF p3", note: "high protein binding capacity"}
  - {name: fetal_maternal_ratio, value: "0.2 to 0.4", unit: ratio, source: "PDF p3"}
  - {name: fraction_unchanged_urine, value: 6, unit: "%", source: "PDF p4"}
  - {name: tmax_regional_block, value: "30 to 45", unit: min, source: "PDF p3-4", note: "caudal, epidural or peripheral nerve block; blood"}
  - {name: seizure_dose_rhesus, value: 4.4, unit: mg/kg, source: "PDF p16 Overdosage", note: "mean; rhesus monkeys; route not stated"}
  - {name: seizure_conc_rhesus, value: 4.5, unit: mcg/mL, source: "PDF p16 Overdosage", note: "mean arterial plasma concentration at seizure; rhesus monkeys"}
  - {name: LD50_iv_mouse, value: "6 to 8", unit: mg/kg, source: "PDF p16"}
  - {name: LD50_sc_mouse, value: "38 to 54", unit: mg/kg, source: "PDF p16"}
  - {name: max_single_dose_with_epi, value: 225, unit: mg, source: "PDF p17 Dosage and Administration", note: "most experience to date; individualize"}
  - {name: max_single_dose_plain, value: 175, unit: mg, source: "PDF p17"}
  - {name: max_daily_dose, value: 400, unit: mg/24 h, source: "PDF p17", note: "doses may be repeated up to once every 3 h"}
```

## Pharmacodynamics and end points
- No therapeutic plasma concentration range is stated anywhere in the label.
- Toxic concentration: the only number is from animals: mean seizure dose 4.4 mg/kg with mean **arterial plasma**
  concentration 4.5 mcg/mL in rhesus monkeys (Overdosage, PDF p16, confirmed on page image). End point:
  seizures. No human toxic threshold is given; CNS/CV toxicity is described qualitatively.
- Onset of dental anesthesia 2 to 10 minutes; dental duration up to 7 hours (p3, p18).
- Convulsions in about 0.1% of epidural local anesthetic administrations (survey; p14).
- Not an opioid; no MEAC, ke0 or potency ratio.

Dosing (Table 1, PDF p18-19, confirmed on page image p18): e.g. epidural 0.75% 10-20 mL (75-150 mg, complete motor
block), 0.5% 10-20 mL (50-100 mg), 0.25% 10-20 mL (25-50 mg); caudal 0.5% 15-30 mL (75-150 mg), 0.25% 15-30 mL
(37.5-75 mg); retrobulbar 0.75% 2-4 mL (15-30 mg); sympathetic 0.25% 20-50 mL (50-125 mg); dental 0.5% w/epi
1.8-3.6 mL (9-18 mg) per site, total per sitting normally not over 90 mg (p18); epidural test dose 0.5% w/epi
2-3 mL (10-15 mg) (p19, text layer). Obstetric epidural: 3-5 mL increments of 0.5% not exceeding 50-100 mg per
dosing interval (p17). Not recommended under 12 years.

## Covariates
Qualitative only: reduce doses in elderly, debilitated, cardiac or hepatic disease; neonatal half-life longer.

## Caveats
- The 4.5 mcg/mL toxic value is a monkey mean arterial plasma concentration, not a human venous threshold; the
  route of the 4.4 mg/kg seizure dosing is not stated.
- "Blood" and "plasma" are used loosely in the PK text.
- Label is the 2018 revision and states it may not be the latest.

## Used by stanpumpR
- `inst/extdata/therapeuticRanges.csv:167` — registry source for the bupivacaine row (no therapeutic range;
  label toxic statement).

## Verification against stanpumpR
- `inst/extdata/therapeuticRanges.csv:167`: bupivacaine, plasma, lower/upper/typical 0, units mg/L,
  band_status `none_established`; source text says toxic thresholds only, no therapeutic range, Sensorcaine
  label (2018) seizures in rhesus monkeys at a mean arterial 4.5 mcg/mL; label_statement repeats the 4.5 mcg/mL
  monkey value. Label: no therapeutic range stated (matches); 4.5 mcg/mL mean arterial plasma concentration at
  seizure in rhesus monkeys, PDF p16 (matches, confirmed on page image). Units: 4.5 mcg/mL = 4.5 mg/L, consistent
  with the row's mg/L. The value is not used as a band (lower/upper 0), so no numeric threshold depends on it.
  Verdict: **matches**.
