---
file: NJ_dailymed-2b29422e.pdf
pmid: null
doi: null
citation: "UCB, Inc. NAYZILAM (midazolam) nasal spray, CIV. US prescribing information, Revised 1/2023 (midazolam initial US approval 1985). DailyMed SPL setid 2b29422e-54d5-4a49-8522-e9cf752368c3; archived 2026-10-21."
kind: label
drugs: [midazolam]
source_read: "text layer, 36 PDF pages: Highlights and contents p1-3; 1 Indications and 2 Dosage and Administration p4-5; 8 Use in Specific Populations p12-14; 11 Description p17-18; 12.1-12.3 Clinical Pharmacology p18-20; 14 Clinical Studies p21. Warnings, adverse reactions, abuse and dependence skimmed; medication guide and instructions for use not read. Page 18 (12.2-12.3) checked on a rendered image; page 1 rendered to confirm identity."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Nayzilam (intranasal midazolam) US label, Revised 1/2023

## Study
Not a study: PLR-format US label of a single-dose nasal spray, 5 mg midazolam in 0.1 mL (3, p5; 11, p17-18).
Indication: acute treatment of seizure clusters (acute repetitive seizures) in epilepsy patients 12 years and older
(1, p4). Efficacy (14, p21): Study 1 (NCT01390220), test-dose phase in 292 patients (two 5 mg doses 10 min apart),
then 201 patients randomised 2:1 (134 Nayzilam 5 mg, 67 placebo); treatment success (seizure end within 10 min and
no recurrence for 6 h) 53.7% vs 34.3%, p = 0.011 (Table 4). PK sources are a healthy-adult single 5 mg study and an
unspecified population PK analysis in epilepsy patients (12.3, p18); no n, assay or model is given for either.

## Model and parameters
No model parameters. Label values (12.3, p18-20):
- Dose proportionality of Cmax and AUC from 5 to 15 mg in epilepsy patients (population PK analysis).
- Healthy adults, single 5 mg nasal: median Tmax 17.3 min (range 7.8-28.2); mean (SD) Cmax 54.7 (30.4) ng/mL;
  AUC0-inf 126.2 (59) ng·h/mL; mean absolute bioavailability about 44%.
- Protein binding about 97% (1-hydroxymidazolam 89%). Estimated total volume of distribution 226.5 L.
- Median elimination half-lives across trials 2.1-6.2 h (midazolam) and 2.7-7.2 h (1-hydroxymidazolam), dose-
  independent. CYP3A4 (liver and gut) to 1-hydroxymidazolam, at least as potent as the parent.

```yaml
parameters:
  - {name: F_nasal, value: 0.44, unit: fraction, source: "12.3 Absorption, PDF p18", note: "mean absolute bioavailability, approximately; healthy adults"}
  - {name: tmax_nasal, value: 17.3, unit: min, source: "12.3 Absorption, PDF p18", note: "median, range 7.8-28.2; single 5 mg, healthy adults"}
  - {name: Cmax_5mg, value: 54.7, unit: ng/mL, source: "12.3 Absorption, PDF p18", note: "mean, SD 30.4"}
  - {name: AUC0inf_5mg, value: 126.2, unit: ng*h/mL, source: "12.3 Absorption, PDF p18", note: "mean, SD 59"}
  - {name: V_total, value: 226.5, unit: L, source: "12.3 Distribution, PDF p18", note: "estimated; basis not stated"}
  - {name: t_half_midazolam, value: "2.1-6.2", unit: h, source: "12.3 Elimination, PDF p19", note: "range of medians in clinical trials"}
  - {name: t_half_1OH_midazolam, value: "2.7-7.2", unit: h, source: "12.3 Elimination, PDF p19", note: "range of medians"}
  - {name: protein_binding, value: 0.97, unit: fraction, source: "12.3 Distribution, PDF p18", note: approximate}
```

Derived (mine): apparent CL/F = 5 mg / 126.2 ng·h/mL = 39.6 L/h, so with F 0.44 systemic CL about 17 L/h; with V
226.5 L, t1/2 = ln 2 x 226.5 / 17.4 = 9.0 h, longer than the 2.1-6.2 h medians, so the 226.5 L is probably not a Vz
from the same study (its basis is not stated).

## Pharmacodynamics and end points
No concentration-effect model or ke0. Sedation and psychomotor impairment began within 10 min, peaked at 30 min to
2 h and were near baseline by 4 h (12.2, p18). Dosing (2.2, p5): one 5 mg spray, a second 5 mg in the other nostril
after 10 min if needed; at most 2 doses per episode, one episode every 3 days, 5 per month.

## Covariates
Text only (12.3, p19-20; 8.5-8.7, p14): geriatric (>65) Cmax and AUC 21-45% higher, half-life about 2 h longer; obesity
(parenteral, n = 20 + 20) half-life 5.9 vs 2.3 h from a larger weight-corrected Vd, clearance unchanged; acute renal
failure in ICU (n = 6) clearance 1.9 vs 2.8 mL/min/kg; cirrhosis clearance 50% lower in one study, unchanged in
another; heart failure half-life doubled, clearance 25% lower. CYP3A4 inducers among antiseizure drugs lower combined
midazolam + 1-OH exposure 16-26% (strong) or 8-15% (weak to moderate), judged not clinically significant (p20).

## Caveats
- The covariate studies are mostly IV or oral midazolam from older labels, not Nayzilam.
- Only an absolute F and a Tmax; no absorption rate constant, no population parameter table.
- In the renal-failure paragraph the label lists "half-life was prolonged (7.6 vs 13 hours)" after "clearance was
  reduced (1.9 vs 2.8)", so the order of patient and control values differs within one sentence (p19); as printed.

## Used by stanpumpR
- `inst/extdata/antiseizureRegistry.csv:25` (at 8baa733): moiety Midazolam, routes "intranasal (Nayzilam), IM
  (Seizalam); IV off-label for status", `US_SEIZURE_LABELED`, `published_population_fit`, analyte midazolam (plasma),
  model "see library model", `stanpumpr_drug` midazolam, this setid as source; notes "Already in the library (IV).
  Nayzilam F~0.44 (not the experimental 0.80); no nasal popPK ka retrievable." Built at
  `data-raw/antiseizureRegistry.R:169-173`.

## Verification against stanpumpR
- "Nayzilam F~0.44": label mean absolute bioavailability about 44% (p18). **matches**.
- "no nasal popPK ka retrievable": the label mentions a population PK analysis but prints no ka or parameters
  (p18). **matches**.
- "Already in the library (IV)": `inst/extdata/drugDefaults_global.csv:13` gives midazolam units "mg,mg/kg,mg/hr",
  IV only; `R/drugs_midazolam.R` has no nasal, IM or oral route terms at 8baa733. **matches**; the nasal product is
  therefore not simulated, and the registry does not claim it is.
- Status `US_SEIZURE_LABELED`, intranasal: label indication seizure clusters, nasal route only (1, 2.2). **matches**.
- "IM (Seizalam)" and "the experimental 0.80": not in this document. **cannot verify** here.
