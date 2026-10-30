---
file: NJ_dailymed-87768fbf.pdf
pmid: none
doi: none
citation: "PERINDOPRIL ERBUMINE tablets USP 2, 4 and 8 mg, Aurobindo Pharma Limited; ANDA079070 (marketing start 11/10/2009); prescribing information Revised 3/2024, DailyMed SPL setid 87768fbf-7c63-47da-8925-0316f343d6ef, archived 2026-10-21. https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=87768fbf-7c63-47da-8925-0316f343d6ef"
kind: label
drugs: [perindopril]
source_read: "text layer, all 22 pages (one form-feed chunk per page). Read: Highlights and contents p.1-3; 1 Indications and 2 Dosage and Administration p.3-4; 8 Use in Specific Populations p.10-11; 10 Overdosage p.11-12; 11 Description p.12; 12.1-12.3 Clinical Pharmacology p.12-14; 14.1 Hypertension p.15; package and SPL product data p.19-22. Page image: p.1 (identity)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# Perindopril erbumine tablets (Aurobindo) prescribing information, DailyMed, revised 3/2024

## Study
Not a study: the PLR-format US generic label for perindopril erbumine 2, 4 and 8 mg tablets (Aurobindo, ANDA079070);
initial US approval 1993; indications essential hypertension and stable coronary artery disease. The file is
what its name says (image p.1).

## Model and parameters
No model. Label facts (12.3, p.13-14 unless noted):
- Perindopril peak plasma at about 1 h; absolute oral bioavailability about 75%.
- 30-50% of systemically available perindopril is hydrolysed (hepatic esterase) to perindoprilat, whose mean
  bioavailability is about 25%; perindoprilat peaks at 3-7 h.
- Food: absorption unchanged but conversion to perindoprilat reduced about 43%, plasma ACE inhibition curve
  reduced about 20%.
- Dose-proportional Cmax and AUC over 4, 8, 16 mg, single and steady state.
- Protein binding: perindopril about 60%, perindoprilat 10-20%.
- Perindopril multicompartmental, half-life of most of its elimination about 0.8-1 h; 4-12% of the dose unchanged
  in urine; six metabolites.
- Perindoprilat apparent half-life 3-10 h for most of the elimination, prolonged terminal half-life 30-120 h
  (slow dissociation from ACE binding sites); accumulates 1.5-2 fold, steady state in 3-6 days; cleared almost
  exclusively renally.
- Elderly (>70 y): perindopril and perindoprilat concentrations about twice those of younger patients. Heart
  failure: perindoprilat dose-interval AUC 40% higher. Renal: perindoprilat AUC about double at CrCl 30-80 vs
  100 mL/min, rising more steeply below 30. Hepatic impairment: perindoprilat concentrations about 50% higher.
- Dialysis clearance: perindopril about 40-80 mL/min, perindoprilat about 40-90 mL/min (12.3); 10 Overdosage gives
  52 and 67 mL/min.
- Molecular weight 368.47 (free acid), 441.61 (erbumine salt) (11, p.12).
Dosage (2, p.3-4): hypertension start 4 mg once daily, usual 4-8 mg/day in one or two doses, maximum 16 mg/day;
elderly start 4 mg; stable CAD 4 mg for 2 weeks then 8 mg; CrCl < 30 mL/min not recommended, lesser impairment
start 2 mg/day, maximum 8 mg/day.

```yaml
parameters:
  - {name: F_perindopril, value: 0.75, unit: fraction, source: "12.3 p.13", note: about}
  - {name: F_perindoprilat, value: 0.25, unit: fraction, source: "12.3 p.13", note: "mean bioavailability of the metabolite, about"}
  - {name: tmax_perindopril, value: 1, unit: h, source: "12.3 p.13", note: approximately}
  - {name: tmax_perindoprilat, value: "3-7", unit: h, source: "12.3 p.13-14"}
  - {name: t_half_perindopril, value: "0.8-1", unit: h, source: "12.3 p.14"}
  - {name: t_half_perindoprilat_apparent, value: "3-10", unit: h, source: "12.3 p.14"}
  - {name: t_half_perindoprilat_terminal, value: "30-120", unit: h, source: "12.3 p.14", note: ACE-binding dissociation}
  - {name: accumulation_perindoprilat, value: "1.5-2", unit: ratio, source: "12.3 p.14"}
  - {name: MW_perindopril, value: 368.47, unit: g/mol, source: "11 p.12", note: free acid}
```

## Pharmacodynamics and end points
12.2 (p.13): ACE inhibition dose- and concentration-related, maximal 80-90% at 8 mg lasting 10-12 h, about 60%
at 24 h; ID50 rises over time; angiotensin I pressor response about 35% inhibited 24 h after 12 mg. 14.1
(p.15): 2 mg had little effect; 8 and 16 mg gave supine trough reductions of 9-15/5-6 mmHg; trough effects 75-100%
of peak. Black patients' diastolic response about half. No therapeutic plasma range. Not an opioid.

## Covariates
Age (>70 y), renal function (CrCl), hepatic impairment, heart failure, food (conversion), race (response).

## Caveats
Descriptive label values without n or assays. The two dialysis-clearance statements (12.3 ranges vs 10 point
values) differ in form but overlap.

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:102: catalog record `perindopril__tablet__oral__catalog__none__label`,
  active moiety perindoprilat, analyte and matrix NA, official_label, descriptive only; limitations "Prodrug
  identity only; parent and active-moiety PK and BP are missing." Built at data-raw/antihypertensiveRegistry.R:121-124.

## Verification against stanpumpR
- **Prodrug, active moiety perindoprilat, oral tablet**: 11 and 12.1. **matches**.
- **"Parent and active-moiety PK ... are missing"**: the label gives descriptive PK for both (F 75% and 25%,
  tmax 1 h and 3-7 h, half-lives 0.8-1 h, 3-10 h and 30-120 h, accumulation 1.5-2). No compartmental model or
  fitted parameter vector is given, so a model is missing, but descriptive PK is not, and the analyte field is
  NA although the label names both analytes. Other rows of this registry (candesartan, terazosin) record the
  label's descriptors. **discrepancy** (understates the source).
- **BP missing**: 14.1 gives only trough reductions and a trough/peak ratio, no time course or
  concentration-effect data. **matches**.
