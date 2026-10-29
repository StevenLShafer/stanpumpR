---
file: NJ_dailymed-1a52bedc.pdf
pmid: none
doi: none
citation: "INSPRA (eplerenone) tablets 25 mg and 50 mg, Viatris Specialty LLC; NDA021437; prescribing information Revised 6/2025 (UPJ:INSPRA:R2); DailyMed setid 1a52bedc-8e2c-4116-a296-a87770676b4a, archived 2026-10-21. https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=1a52bedc-8e2c-4116-a296-a87770676b4a"
kind: label
drugs: [eplerenone]
source_read: "text layer, all 24 pages (one form-feed chunk per page). Read: Highlights and contents p.1-2; 1 Indications p.3; 2 Dosage and Administration p.3-4; 4 Contraindications and 5 Warnings p.5; 6 Adverse Reactions p.5-7; 7 Drug Interactions p.8; 8 Use in Specific Populations p.8-10; 10 Overdosage and 11 Description p.10-11; 12.1-12.3 Clinical Pharmacology p.11-13; 14 Clinical Studies p.14-19; 16-17 and carton/SPL data p.19-24. Page image: p.7 (Tables 2-4, scrambled in the text layer)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Inspra (eplerenone) prescribing information, DailyMed, revised 6/2025

## Study
Not a study: the US prescribing information for INSPRA (eplerenone) 25 and 50 mg film-coated tablets, Viatris,
NDA021437 (initial US approval 2002), with carton panels and SPL data (marketing end date 01/31/2027 listed for both
NDCs). The file is what its name says.

## Model and parameters
No model. Label facts (12.3, p.12-13 unless noted):
- Cleared predominantly by CYP3A4; **elimination half-life 3-6 h**; steady state within 2 days; food does not affect
  absorption.
- **Peak plasma concentration about 1.5-2 h**; **absolute bioavailability 69% after a 100 mg tablet**; Cmax and AUC
  dose-proportional 25-100 mg, less than proportional above 100 mg.
- Protein binding about 50%, mainly alpha-1-acid glycoprotein; **apparent Vss 42-90 L**; no preferential red-cell
  binding.
- No active metabolites identified in human plasma; < 5% excreted unchanged; radiolabel 67% urine, 32% faeces;
  **apparent plasma clearance about 10 L/h**.
- Elderly (>= 65): steady-state Cmax +22%, AUC +45%; no sex difference; Black subjects Cmax -19%, AUC -26%.
- Severe renal impairment AUC +38%, Cmax +24%; haemodialysis AUC -26%, Cmax -3%; no correlation of clearance with
  CrCl; not dialysable. Moderate hepatic impairment (400 mg) Cmax +3.6%, AUC +42%. Heart failure (n = 8 vs 8): AUC +38%,
  Cmax +30%.
- Ketoconazole: Cmax x1.7, AUC x5.4; moderate CYP3A inhibitors Cmax +40-60%, AUC +100-190%; grapefruit +25%; St
  John's wort AUC about -30%.
- Check (derived): 10 L/h x 4.5 h / 0.693 = 65 L, inside the 42-90 L range.
- Dosing (2): HFrEF post-MI 25 mg o.d. titrated to 50 mg o.d. within 4 weeks with a potassium table; hypertension 50 mg
  o.d., up to 50 mg b.i.d.; moderate CYP3A inhibitors 25 mg o.d. (max 25 mg b.i.d. in hypertension). Contraindicated
  K > 5.5 mEq/L, CrCl <= 30 mL/min, strong CYP3A inhibitors; for hypertension also CrCl < 50, SCr > 2.0 (M) / 1.8 (F)
  mg/dL, type 2 diabetes with microalbuminuria, potassium supplements or K-sparing diuretics.
- MW 414.50 (11).

```yaml
parameters:
  - {name: F, value: 0.69, unit: fraction, source: "12.3 p.12", note: "absolute, 100 mg tablet"}
  - {name: tmax, value: "1.5-2", unit: h, source: "12.3 p.12"}
  - {name: t_half, value: "3-6", unit: h, source: "12.3 p.12"}
  - {name: CL_apparent, value: 10, unit: L/h, source: "12.3 p.12", note: "approximately"}
  - {name: Vss_apparent, value: "42-90", unit: L, source: "12.3 p.12"}
  - {name: protein_binding, value: 0.50, unit: fraction, source: "12.3 p.12"}
  - {name: MW, value: 414.50, unit: g/mol, source: "11 p.11"}
```

## Pharmacodynamics and end points
Hypertension (14.2): two fixed-dose placebo-controlled 8-12 week studies (611 eplerenone, 140 placebo; 25-400 mg/day);
50-200 mg/day lowered trough sitting BP by 6-13 / 3-7 mmHg versus placebo; 50 mg b.i.d. beat 100 mg o.d. by 4/3 mmHg
(cuff); effect within 2 weeks, maximal by 4 weeks. Potassium rose with dose (Table 4, p.7, image-confirmed: +0.08,
+0.14, +0.09 mEq/L at 25, 50, 100 mg/day). EPHESUS (14.1; n = 6632): all-cause death 14.4% vs 16.7%, HR 0.85. No
concentration-effect relation and no therapeutic range are given, and the label does not say whether the trials
sampled PK. Not an opioid; no MEAC.

## Covariates
Age, race, renal and hepatic impairment, heart failure, CYP3A inhibitors and inducers (above). Paediatric
hypertension: not effective in a 10-week study of 304 children aged 4-16 at adult-like exposures.

## Caveats
Descriptive label values; the studies behind 12.3 are not identified.

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:47 (8baa733; built by data-raw/antihypertensiveRegistry.R:324):
  catalog record `eplerenone__tablet__oral__catalog__none__label`, descriptive_only; limitations "Label F about 69% at
  100 mg, Tmax 1.5-2 h, half-life 3-6 h", then that a parent/reversible-metabolite PK model exists but the FDA review
  says the pivotal hypertension trials took no PK samples, so no concentration-DBP fit exists to port.

## Verification against stanpumpR
- **F about 69% at 100 mg; Tmax 1.5-2 h; half-life 3-6 h**: 12.3. **matches**.
- **Parent/reversible-metabolite PK model; FDA review says pivotal trials took no PK samples**: not in this label
  (the label says no active metabolites were found in plasma and is silent on trial PK sampling). **cannot verify**
  here.
