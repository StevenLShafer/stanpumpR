---
file: NJ_dailymed-5d398da1.pdf
pmid: null
doi: null
citation: "Dr. Reddy's Laboratories Inc. Enalaprilat Injection USP, 1.25 mg/mL (1 mL and 2 mL single-dose vials). DailyMed SPL setid 5d398da1-020c-ccb1-a775-ce8e0b29d621; label text revised 04/2023, SPL revised 6/2024; archived 2026-10-21."
kind: label
drugs: [enalaprilat]
source_read: "text layer, all 18 PDF pages (old non-PLR format): pregnancy box, Description, Clinical Pharmacology incl. Pharmacokinetics and Metabolism and Pharmacodynamics p1-3; Indications, Contraindications, Warnings incl. Fetal/Neonatal Morbidity and Mortality p3-6; Precautions incl. Pregnancy, Nursing, Pediatric and Geriatric Use p6-8; Adverse Reactions p8-10; Overdosage p10-11; Dosage and Administration p11-12; How Supplied p12-13 (Revised 04/2023); carton images and SPL data p13-18 (Revised 6/2024). PDF p2 checked on a rendered image."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Enalaprilat Injection USP, Dr. Reddy's, US label (04/2023)

## Study
Not a study: a generic enalaprilat injection label (1.25 mg/mL anhydrous enalaprilat, benzyl alcohol 9 mg/mL;
enalaprilat dihydrate MW 384.43; PDF p1). Indication: hypertension when oral therapy is not practical (PDF p3).

## Model and parameters
No model. Pharmacokinetics and Metabolism (PDF p2):

```yaml
parameters:
  - {name: t_half_accumulation, value: 11, unit: h, source: "PDF p2", note: "approximately; 'as determined from oral administration of multiple doses of enalapril maleate', not from IV enalaprilat"}
  - {name: urinary_recovery_IV, value: ">0.90", unit: fraction of dose, source: "PDF p2", note: "unchanged enalaprilat within 24 h"}
  - {name: dialysis_clearance, value: 62, unit: mL/min, source: "PDF p2"}
```
After a single IV dose the serum profile is polyexponential with a prolonged terminal phase, attributed to a small
ACE-bound fraction whose amount does not rise with dose (saturable binding; PDF p2). Enalaprilat is poorly absorbed
orally. Renal impairment: unchanged disposition until GFR <= 30 mL/min, then higher peak and trough and prolonged
effective half-life (PDF p2). No volumes, clearances or exponents are given.

## Pharmacodynamics and end points
- IV onset usually within 15 min, maximum effect within 1-4 h, duration about 6 h at the recommended dose; duration
  dose-related (Pharmacodynamics, PDF p2). Peak after the first dose may take up to 4 h; later doses may exceed the
  first (D&A, PDF p11).
- Single doses as low as 0.2 mg caused excessive hypotension in at-risk normotensive patients (PDF p12).
- No concentration-effect model; not an opioid.

Dosing (PDF p11-12): 1.25 mg IV over 5 min every 6 h; doses to 5 mg every 6 h tolerated for 36 h, limited experience
above 20 mg/day; on diuretics or CrCl <= 30 mL/min start 0.625 mg, repeat after 1 h if needed; IV-to-oral conversion
5 mg enalapril once daily (2.5 mg if CrCl <= 30 mL/min or responding to 0.625 mg). Overdosage: 80 mg IV over 15 min
given in studies without added effects (PDF p10).

## Covariates
Renal function (above). Pregnancy categories C (first trimester) and D (second and third) with a "USE IN PREGNANCY"
box (PDF p1, p8); pediatric safety and effectiveness not established (PDF p8).

## Caveats
- The only half-life given is an accumulation half-life from ORAL enalapril; the label has no IV elimination half-life.
- Generic label; no study sizes for the PK statements.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:45`: record `enalaprilat__injection__iv__catalog__none__label`, IV,
  descriptive only, no stanpumpR drug; limitations quoted below.

## Verification against stanpumpR
- "polyexponential profile comes from saturable ACE binding": **matches** (PDF p2).
- "about 11 h effective accumulation half-life is derived from repeated ORAL enalapril dosing: it must not be used as an
  intravenous one-compartment elimination half-life": **matches** (PDF p2, rendered).
- "IV BP onset is within 15 min with a peak at 1-4 h": **matches** (PDF p2).
- "no fitted PK or BP": **matches**.
- Analyte and matrix (enalaprilat, plasma): consistent. Setid matches the registry URL.
