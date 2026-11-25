---
file: NJ_dailymed-0bef7cf2.pdf
pmid: null
doi: null
citation: "Felodipine extended-release tablets USP 2.5, 5 and 10 mg (Heritage Pharmaceuticals d/b/a Avet Pharmaceuticals; ANDA201964). US prescribing information (old non-PLR format), text revised 12/2022, SPL revised 8/2026. DailyMed setid 0bef7cf2-012b-4804-ac93-0a8b333035db; downloaded 2026-10-21."
kind: label
drugs: [felodipine]
source_read: "text layer, all 17 PDF pages (form-feed chunks = pages). Read Description and Clinical Pharmacology incl. Pharmacokinetics and Metabolism, Geriatric/Hepatic/Renal, Cardiovascular Effects and Clinical Studies (pp1-4), Indications, Contraindications, Precautions incl. Drug Interactions, Pregnancy, Pediatric/Geriatric Use (pp4-8), Overdosage and Dosage and Administration (pp10-11), How Supplied and product data (pp12-17). Rendered page 4 (the mean BP reduction table, scrambled in the text layer); confirmed from the image."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# Felodipine ER tablets (generic, Avet) US label, 2022

## Study
This is a generic felodipine extended-release tablet label (USV, India, for Avet; ANDA201964, marketed from
11/2013). Felodipine is a racemic dihydropyridine calcium channel blocker, MW 384.26 (p1). PDF page 1 is this
product as named, so there is no source file problem. The text (p1) notes "USP dissolution test pending".

**Dosage (D&A, p11):**
- Start at 5 mg once daily; range 2.5-10 mg once daily, adjusted at intervals of at least 2 weeks.
- Over 65 years or with impaired liver function: start at 2.5 mg.
- Take without food or with a light meal. Swallow whole.

## Model and parameters
No compartmental model is stated. Descriptors from Pharmacokinetics and Metabolism (pp2-3):
```yaml
parameters:
  - {name: F (systemic), value: 0.20, unit: fraction, source: "p2", note: "approximately; almost complete absorption, extensive first pass"}
  - {name: Tmax (ER tablet), value: "2.5-5", unit: h, source: "p2", note: "mean peak"}
  - {name: "IV disposition t1/2 (phase 1)", value: 4.8, unit: min, source: "p2", note: "triexponential after IV; 15% of AUC"}
  - {name: "IV disposition t1/2 (phase 2)", value: 1.5, unit: h, source: "p2", note: "40% of AUC"}
  - {name: "IV disposition t1/2 (phase 3)", value: 9.1, unit: h, source: "p2", note: "45% of AUC"}
  - {name: terminal t1/2 (oral IR), value: "11-16", unit: h, source: "p2", note: "mean"}
  - {name: CL (systemic, young healthy), value: 0.8, unit: L/min, source: "p2", note: "about"}
  - {name: V (apparent), value: 10, unit: L/kg, source: "p2", note: "about"}
  - {name: EC50 (BP), value: "4-6", unit: nmol/L, source: "p2", note: "concentration for half-maximal BP decline; derived 1.5-2.3 ng/mL at MW 384.26"}
  - {name: "Css peak/trough, ER 10 mg, young healthy", value: "7 / 2", unit: nmol/L, source: "p2", note: "IR 10 mg once daily: 20 / 0.5 nmol/L"}
  - {name: "Css peak/trough, ER 20 mg, hypertensives (mean age 64)", value: "23 / 7", unit: nmol/L, source: "p2", note: ""}
  - {name: protein binding, value: ">0.99", unit: fraction, source: "p2", note: ""}
```
- Cmax and AUC are linear up to 20 mg (p2).
- Steady-state peak is about 20% above the single-dose peak.
- Food: a high-fat or carbohydrate meal raises Cmax about 60% with AUC unchanged; a light meal has no effect;
  grapefruit juice about doubles bioavailability (p3).
- About 70% of radiolabel appears in urine and 10% in faeces, with less than 0.5% unchanged (p2).
- Derived 3-compartment reconstruction from the IV data: AUC_i = fraction × Dose/CL and C_i = AUC_i·λ_i.
  - It gives V1 of about 32 L, Vss of about 330 L and Vz = CL/λ3 of about 630 L.
  - That Vz is 9.0 L/kg at 70 kg, matching the label's "about 10 L/kg".
  - So the IV disposition numbers are internally consistent and determine a triexponential IV disposition.
    Release kinetics of the ER product are not given.

## Pharmacodynamics and end points
- **Concentration-effect (p2):** BP response correlates with plasma concentration, with EC50 4-6 nmol/L for a
  half-maximal BP decline. No Emax or slope is given.
- **ER onset and duration (p3):** BP falls within 2-5 h. Trough diastolic reduction is about 40-50% of peak.
  Reflex HR rises 5-10 bpm.
- **Placebo-subtracted BP reductions (table, p4, rendered).** Peak SBP/DBP, trough SBP/DBP, and trough/peak %
  (S/D):

  | Study | Dose | n | Peak | Trough | Trough/peak % |
  |---|---|---|---|---|---|
  | Study 1 (8 wk) | 2.5 mg | 68 | 9.4/4.7 | 2.7/2.5 | 29/53 |
  | Study 1 (8 wk) | 5 mg | 69 | 9.5/6.3 | 2.4/3.7 | 25/59 |
  | Study 1 (8 wk) | 10 mg | 67 | 18.0/10.8 | 10.0/6.0 | 56/56 |
  | Study 2 (4 wk) | 10 mg | 50 | 5.3/7.2 | 1.5/3.2 | 33/40 |
  | Study 2 (4 wk) | 20 mg | 50 | 11.3/10.2 | 4.5/3.2 | 43/34 |

- **Not an opioid:** no MEAC or ke0.

## Covariates
- Age: CL in elderly hypertensives (mean 74 y) is 45% of that in young volunteers (mean 26 y). The young
  patients' steady-state AUC is 39% of the elderly's (p3).
- Hepatic disease: CL about 60% of normal (p3).
- Renal impairment: no change in the parent; inactive metabolites accumulate.
- CYP3A4 interactions (pp5-6), as stated:
  - Inducing anticonvulsants (phenytoin, carbamazepine, phenobarbital): AUC about 6% of healthy volunteers'.
  - Itraconazole: AUC about 8-fold with another ER formulation.
  - Erythromycin: about 2.5-fold.
  - Grapefruit juice: more than 2-fold.
  - Cimetidine: about +50%.
- No equations.

## Caveats
- This is a generic label carrying the reference product's PK text. The IV, IR and ER data come from
  unidentified studies.
- The ER release profile (ka, lag, release fraction) is not described.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:54` (8baa733): `felodipine__er_tablet__oral__catalog__none__label`
  (felodipine, oral, extended-release tablet, `official_label_descriptive`, `descriptive_only`, no stanpumpR
  drug).
  - Limitations: "Label F about 20% and Tmax 2.5-5 h do not identify the release profile or the disposition;
    product-specific ER PK and any BP coefficient are missing."
- The row is generated by `data-raw/antihypertensiveRegistry.R:113-116`.
- `tests/testthat/test-antihypertensive-registry.R:520` lists felodipine among the calcium channel blockers.

## Verification against stanpumpR
- **F about 20% and Tmax 2.5-5 h**: match p2 exactly. **matches**
- **No release profile; product-specific ER PK missing**: correct. The label gives ER Cmax/trough concentrations
  and Tmax but no release kinetics. **matches**
- **"do not identify ... the disposition"**: true of F and Tmax alone. But the same label gives an IV
  triexponential disposition: half-lives 4.8 min, 1.5 h and 9.1 h, AUC shares 15/40/45%, and CL 0.8 L/min.
  Those determine the IV disposition (derived reconstruction above, consistent with the label's about 10 L/kg).
  The limitation text omits this. **discrepancy** (omission)
- **"any BP coefficient [is] missing"**: the label gives an EC50 of 4-6 nmol/L for the half-maximal BP decline
  (p2). It gives no Emax or slope, so no complete concentration-BP model, but a BP coefficient is printed.
  **discrepancy** (wording)
- **Extended-release tablet, oral, felodipine as the moiety**: matches p1. **matches**
