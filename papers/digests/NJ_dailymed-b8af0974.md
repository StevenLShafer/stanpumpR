---
file: NJ_dailymed-b8af0974.pdf
pmid: null
doi: null
citation: "Nitroglycerin Injection, USP, 5 mg/mL (50 mg/10 mL vial), for intravenous infusion after dilution. US prescribing information, repackaged by RemedyRepack Inc. (source NDC 0517-4810); ANDA072034; SPL revised 9/2026. DailyMed setid b8af0974-33ea-4dd5-9ff7-3b61272aeb25; downloaded 2026-10-21."
kind: label
drugs: [nitroglycerin]
source_read: "text layer, all 12 PDF pages (form-feed chunks = pages). Old-format label with no numbered sections: Description, Clinical Pharmacology incl. Pharmacokinetics and Clinical Trials (pp1-3), Indications, Contraindications, Warnings (p3), Precautions incl. pregnancy, pediatric, nursing (pp4-5), Adverse Reactions, Overdosage, Methemoglobinemia (pp6-7), Dosage and Administration with Table 1 and dilution table (pp7-9), How Supplied and SPL data (pp9-12). Rendered page 1 to confirm identity. Table 1 (p9) is scrambled in the text layer; no number from it is recorded here."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Nitroglycerin injection 5 mg/mL (ANDA072034, RemedyRepack) label

## Study
This is the DailyMed SPL for generic nitroglycerin injection: 5 mg/mL in 30% alcohol and 30% propylene glycol, in
10 mL vials, under ANDA072034. It was repackaged by RemedyRepack, marketed from 11/14/2024, SPL revised 9/2026.
Page 1 is that product, so there is no source file problem.

**Indications:**
- peri-operative hypertension;
- CHF in acute MI;
- angina refractory to sublingual nitroglycerin and beta-blockers;
- induced intraoperative hypotension.

**Dosage (pp7-8):**
- The drug must be diluted, in D5W or saline, to 50-400 mcg/mL. It is not for direct IV injection.
- With a non-absorbing infusion set: start at 5 mcg/min and step by 5 mcg/min every 3-5 min. Above 20 mcg/min
  without response, use 10, then 20 mcg/min steps.
- The 25 mcg/min starting doses reported in older studies used PVC sets. With non-absorbing tubing they are too
  high.

**Warnings (p3):** PVC tubing delivered only 20-60% of the solution's nitroglycerin in published studies, varying
over time, and no correction factor applies.

## Model and parameters
Pharmacokinetics (p2) gives descriptive values only.
```yaml
parameters:
  - {name: Vd, value: 3, unit: L/kg, source: "Clinical Pharmacology, Pharmacokinetics, p2", note: "about 3 L/kg"}
  - {name: CL, value: 1, unit: L/kg/min, source: "p2", note: "'close to 1 L/kg/min'; far above hepatic blood flow; red cells and vessel walls metabolize it"}
  - {name: serum t1/2, value: 3, unit: min, source: "p2", note: "about 3 min"}
  - {name: drug-free interval to avoid tolerance, value: "10-12", unit: h, source: "p2", note: "known to be sufficient; shorter intervals not well studied"}
  - {name: dose for clinically significant methaemoglobinaemia, value: 1, unit: mg/kg, source: "Overdosage, p7", note: "about 1 mg/kg even with no cytochrome b5 reductase activity"}
```
**Half-life check (derived):** 0.693 x 3 L/kg / 1 L/kg/min = 2.1 min, against the label's "about 3 min". This
falls within the label's rounding of a single-compartment summary.

**Metabolites (p2):** inorganic nitrate and 1,2- and 1,3-dinitroglycerols. The dinitrates are weaker vasodilators,
longer-lived in serum, and of unknown net contribution. They are further metabolized to mononitrates, then to
glycerol and CO2.

## Pharmacodynamics and end points
- **Tolerance (pp2-3):** continuous nitrates were indistinguishable from placebo on exercise testing after 24 h
  or less. Dose escalation failed to restore efficacy. In one IV study, continuous infusion had lost almost all
  haemodynamic effect at 48 h, while 12-of-24-h dosing showed no attenuation. Rebound appeared during nitrate-free
  intervals.
- **Haemodynamics:** systolic and diastolic BP, CVP, RAP, PAP, PCWP, PVR and SVR all fall. Heart rate usually
  rises.
- **Concentrations:** the label gives no concentration-effect relation, and says serum levels have no established
  role in overdose management.

## Covariates
None quantified. Volume-depleted or hypotensive patients, and those with normal filling pressures, may respond to
as little as 5 mcg/min.

## Caveats
- The PK values are unsourced round numbers.
- The label does not give the matrix (serum or plasma, arterial or venous), and the fraction delivered depends
  heavily on tubing.
- The infusion-rate table on p9 is scrambled in the text layer and was not used.

## Used by stanpumpR
`inst/extdata/antihypertensiveRegistry.csv:98` (generated from `data-raw/antihypertensiveRegistry.R:445-449`),
record `nitroglycerin__injection__iv__catalog__none__label`:
- active moiety "nitroglycerin (dinitrate metabolites persist)", IV infusion, plasma, descriptive only;
- limitations: "Label Vd about 3 L/kg, clearance about 1 L/kg/min, serum half-life about 3 min; a human infusion
  study reports clearance 0.3-1 L/min/kg", plus dinitrate persistence, tolerance and no BP coefficients.

The vasoactive registry row for Armstrong 1983 (`inst/extdata/vasoactiveRegistry.csv:13`) links to this record.

## Verification against stanpumpR
- **Vd about 3 L/kg, CL about 1 L/kg/min and serum t1/2 about 3 min:** all three are in the label (p2). **matches**
- **Dinitrate metabolites persist:** the label says the dinitrates are longer-lived in the serum. **matches**
- **Tolerance during continuous delivery, and the antianginal tolerance is not a BP tolerance rate:** the label
  describes exercise-test tolerance within 24 h and haemodynamic loss by 48 h, with no rate constant. **matches**
- **"A human infusion study reports clearance 0.3-1 L/min/kg":** this is not from this label, which gives only
  "close to 1 L/kg/min". The repo attributes it to McNiff 1981 (`R/drugs_nitroglycerin.R:59-64`), which is not in
  this batch. **cannot verify here**
- **No fitted BP coefficients:** none are in the label. **matches**
