---
file: NJ_dailymed-2abb091b.pdf
pmid: none
doi: none
citation: "LOTENSIN (benazepril hydrochloride) tablets, Validus Pharmaceuticals LLC; NDA019851; prescribing information Revised 1/2019 (60023-05 January 2019), DailyMed SPL setid 2abb091b-a53e-46b0-9b84-e5ee8f2bdd8e, archived 2026-10-21. https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=2abb091b-a53e-46b0-9b84-e5ee8f2bdd8e"
kind: label
drugs: [benazepril]
source_read: "text layer, all 21 pages (one form-feed chunk per page). Read: Highlights and contents p.1-3; 2 Dosage and Administration p.4; 8 Use in Specific Populations p.10-11; 10 Overdosage p.11-12; 12.1-12.3 Clinical Pharmacology p.12-14; 14 Clinical Studies p.15; 16 and SPL product data p.16-21. Page images: p.1, p.14 (pediatric PK table, scrambled in the text layer), p.15."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Lotensin (benazepril) prescribing information, DailyMed, revised 1/2019

## Study
Not a study: the US prescribing information for Lotensin tablets 10, 20 and 40 mg (Validus; NDA019851, marketed
since 06/21/1991), with carton panels and SPL product data. The file is what its name says (p.1 image).

## Model and parameters
No model. Label facts (12.3, p.13-14 unless noted):
- PK approximately dose-proportional over 10-80 mg.
- Peak plasma concentrations: benazepril 0.5-1.0 h, benazeprilat 1-2 h. Food does not change benazepril's
  bioavailability but delays benazeprilat's peak to 2-4 h. (The label does not use the word "fasting"; the 1-2 h
  figure is the one contrasted with food.)
- Protein binding (equilibrium dialysis): benazepril about 96.7%, benazeprilat about 95.3%.
- Almost complete ester cleavage to benazeprilat, mainly hepatic; both glucuronidated.
- Urine: about 37% of an oral dose, as benazeprilat 20%, benazeprilat glucuronide 8%, benazepril glucuronide 4%,
  trace benazepril. Biliary excretion about 11-12% of benazeprilat excretion.
- Effective half-life of benazeprilat with once-daily dosing 10-11 h; steady state after 2-3 doses; AUC
  accumulation ratio 1.19.
- Renal: PK similar to normal for CrCl >30 mL/min; at <=30 mL/min peak benazeprilat and the alpha half-life
  increase. About 6% of benazeprilat removed by 4 h of dialysis started 2 h after 10 mg.
- Hepatic cirrhosis: benazeprilat PK essentially unaltered.
- Pediatric single-dose benazeprilat (table, p.14, image-confirmed; Tmax is starred but the footnote is not
  printed; values with ranges in parentheses, statistic not named):

| Age group | n | Cmax ng/mL | Tmax h | AUC0-inf ng.h/mL | CL/F/wt L/h/kg | t1/2 h |
|---|---|---|---|---|---|---|
| >1 to <=24 months | 5 | 277 (192, 391) | 1 (0.6, 2) | 1328 (773, 2117) | 0.26 (0.18, 0.4) | 5.0 (4, 5.8) |
| >2 to <=6 y | 7 | 200 (168, 244) | 2 (1.4, 2.4) | 978 (842, 1152) | 0.36 (0.31, 0.42) | 5.5 (4.7, 6.5) |
| >6 to <=12 y | 7 | 221 (194, 258) | 2 (1.2, 2.2) | 1041 (855, 1313) | 0.25 (0.21, 0.31) | 5.5 (4.7, 6.5) |
| >12 to <=17 y | 8 | 287 (217, 420) | 2 (1.3, 2.3) | 1794 (1478, 2340) | 0.16 (0.13, 0.21) | 5.1 (4.2, 5.7) |

The dose given to the children is not stated in the table.

- Dosage (2, p.4): adults 10 mg o.d. start (5 mg on a diuretic), usual 20-40 mg/day as one or two doses, 80 mg
  limited experience; children >=6 y 0.2 mg/kg o.d. up to 0.6 mg/kg (not above 40 mg); GFR <30 mL/min/1.73 m2
  (SCr >3 mg/dL) start 5 mg, maximum 40 mg/day. Not for children <6 y or infants <1 y.
- Molecular weight of benazepril hydrochloride 460.96 (11, p.12).

```yaml
parameters:
  - {name: tmax_benazepril, value: "0.5-1.0", unit: h, source: "12.3 p.13"}
  - {name: tmax_benazeprilat, value: "1-2", unit: h, source: "12.3 p.13", note: "2-4 h with food"}
  - {name: t_half_eff_benazeprilat, value: "10-11", unit: h, source: "12.3 p.13", note: once-daily repeat dosing}
  - {name: accumulation_ratio_AUC, value: 1.19, unit: ratio, source: "12.3 p.13"}
  - {name: protein_binding_benazeprilat, value: 0.953, unit: fraction, source: "12.3 p.13"}
  - {name: urine_benazeprilat, value: 0.20, unit: fraction of dose, source: "12.3 p.13"}
  - {name: MW_benazepril_HCl, value: 460.96, unit: g/mol, source: "11 p.12"}
```

## Pharmacodynamics and end points
10 mg or more inhibits plasma ACE by at least 80-90% for at least 24 h; pressor response to angiotensin I
inhibited 60-90% up to 4 h at 10 mg (12.2). Blood pressure falls within 1 h, peaks at 2-4 h, lasts 24 h; trough
reduction about 50% of peak (14). No therapeutic plasma range. Not an opioid.

## Covariates
Renal function (dose adjustment at CrCl <=30 mL/min or GFR <30 mL/min/1.73 m2), age (children), race (less
effect as monotherapy in Black patients). Hepatic impairment does not alter benazeprilat PK.

## Caveats
Descriptive label values only (ranges, approximate); the source studies, assays and n behind 12.3 are not given.
Revised 1/2019; a newer Lotensin label may exist.

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:12: catalog record `benazepril__ir_tablet__oral__catalog__none__label`,
  source_type official_label, descriptive only; limitations "Label parent Tmax 0.5-1 h, active 1-2 h fasting,
  effective active half-life 10-11 h", plus a remark on a renin-angiotensin systems model.

## Verification against stanpumpR
- **Parent Tmax 0.5-1 h**: 12.3. **matches**.
- **Active (benazeprilat) Tmax 1-2 h fasting**: 12.3 gives 1-2 h, delayed to 2-4 h by food; "fasting" is a fair
  reading but the label does not say it. **matches**.
- **Effective active half-life 10-11 h**: 12.3. **matches**.
- **"The published renin-angiotensin systems model outputs angiotensin II, not a verified BP curve"**: not from
  this label. **cannot verify** here.
- The implemented benazepril model (R/drugs_benazepril.R) is from Kaiser 1989 and does not cite this label.
