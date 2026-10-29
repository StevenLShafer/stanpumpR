---
file: NJ_dailymed-8dae76a2.pdf
pmid: null
doi: null
citation: "Metoprolol Tartrate Injection, USP, 5 mg/5 mL, for intravenous use. US prescribing information, Civica Inc. (manufactured by Hikma Farmaceutica, Portugal); ANDA077761; revised 4/2026. DailyMed setid 8dae76a2-32e7-40a2-bb70-9c6380b9b330; downloaded 2026-10-21."
kind: label
drugs: []
source_read: "text layer, all 15 PDF pages (form-feed chunks = pages). Read Highlights (p1), contents (p2), 1-5 Indications to Warnings incl. 2 Dosage and Administration (pp3-4), 6 Adverse Reactions (pp4-6), 7 Drug Interactions (p6), 8 Use in Specific Populations (pp7-9), 10 Overdosage and 11 Description (pp9-10), 12.1-12.3 and 12.5 Clinical Pharmacology (pp10-11), 14 Clinical Studies (pp12-13), 16 How Supplied and SPL data (pp13-15). Rendered page 1 to confirm identity."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Metoprolol tartrate injection (Civica/Hikma ANDA077761) label, 2026

## Study
This is the DailyMed SPL for generic metoprolol tartrate injection, 5 mg in 5 mL single-dose vials (1 mg/mL). It
is ANDA077761, initial US approval 1978, label revised 4/2026. Page 1 is that product, so there is no source file
problem.

The indication is definite or suspected acute MI in haemodynamically stable patients, with oral metoprolol
maintenance.

**Dosage (2, p3):** three 5 mg IV boluses about 2 minutes apart, given in a coronary care unit with BP, heart rate
and ECG monitoring. The patient is then moved to oral metoprolol.

**Contraindications (4):** heart rate below 45, second- or third-degree block, PR of 0.24 s or more, systolic BP
below 100 mmHg, and decompensated failure.

## Model and parameters
Section 12.3 (p11) gives descriptive values only, with no compartmental model.
```yaml
parameters:
  - {name: albumin binding, value: 12, unit: "%", source: "12.3, p11", note: "about 12%"}
  - {name: CSF/plasma ratio, value: 0.78, unit: ratio, source: "12.3, p11", note: "CSF 78% of simultaneous plasma"}
  - {name: plasma t1/2, value: "3-7", unit: h, source: "12.3, p11", note: "approximately; elimination mainly hepatic"}
  - {name: urinary unchanged after IV, value: 10, unit: "% of dose", source: "12.3, p11", note: "approximately; <5% after an oral dose"}
  - {name: total systemic CL (IV, chronic renal failure), value: 1, unit: L/min, source: "12.3, p11", note: "mean"}
  - {name: dialytic CL (high-flux haemodialysis, oral dosing), value: 87, unit: mL/min, source: "12.3, p11", note: "mean"}
  - {name: time to maximum beta blockade (10-min IV infusion), value: 20, unit: min, source: "12.2, p10", note: "normal volunteers, approximately"}
  - {name: oral:IV equieffective dose ratio, value: 2.5, unit: ratio, source: "12.2, p10", note: "for equivalent maximal beta blockade, approximately"}
```

## Pharmacodynamics and end points
- **Exercise heart rate (12.2, p10):** log plasma concentration is linearly related to the reduction in exercise
  heart rate.
- **Acute MI:** IV then oral dosing lowered heart rate, systolic pressure and cardiac output. Stroke volume,
  diastolic pressure and PA end-diastolic pressure were unchanged (p11).
- **Effect delay:** maximal beta blockade comes about 20 min after a 10-minute infusion, so effect lags
  concentration.
- **Resting pressure:** the label gives no concentration-BP coefficients.
- **CYP2D6 (7.3, 12.3):** strong inhibitors double concentrations, quinidine triples S-metoprolol, and propafenone
  raises steady state 2- to 5-fold. Poor metabolizers are about 8% of Caucasians.

## Covariates
- **CYP2D6 phenotype and inhibitors:** several-fold rise.
- **Hepatic impairment:** levels likely increase substantially; start low.
- **Geriatric:** caution.
- **Renal:** total CL is still about 1 L/min in chronic renal failure.

## Caveats
- The half-life range is a summary with no study.
- The time to maximal effect is one volunteer observation.
- The adverse-reaction table on p5 is scrambled in the text layer. It was not needed here, and the numbers are
  the same as the LOPRESSOR tablet label's.

## Used by stanpumpR
`inst/extdata/antihypertensiveRegistry.csv:86` (generated from `data-raw/antihypertensiveRegistry.R:460-464`),
record `metoprolol__tartrate_injection__iv__catalog__none__label`:
- descriptive only, no stanpumpR drug;
- limitations: "Label half-life 3-7 h and CYP2D6-dependent. Log concentration tracks EXERCISE heart-rate
  reduction; there is no validated intravenous BP effect model", and the record is distinct from the oral ones.

## Verification against stanpumpR
- **Half-life 3-7 h:** 12.3 says approximately 3 to 7 hours. **matches**
- **CYP2D6-dependent:** metoprolol is primarily metabolized by CYP2D6 (12.3, 12.5). **matches**
- **Log concentration tracks exercise heart-rate reduction:** 12.2 gives a linear relation between log plasma
  levels and reduction of exercise heart rate. **matches**
- **No validated IV BP effect model:** the label reports only qualitative haemodynamic changes in MI. **matches**
- **A record distinct from the oral ones:** this is an IV injection label and differs from the LOPRESSOR oral
  label (`NJ_dailymed-b5f4fed2`). **matches**
