---
file: NJ_label-lopressor_017963_2024-01-03.pdf
pmid: none
doi: none
citation: "LOPRESSOR (metoprolol tartrate) tablets, for oral use, NDA 017963 (Validus Pharmaceuticals LLC), prescribing information revised 09/2023, Reference ID 5302971, posted on Drugs@FDA 2024-01-03. https://www.accessdata.fda.gov/drugsatfda_docs/label/2023/017963Orig1s076lbl.pdf"
kind: label
drugs: [metoprolol]
source_read: "text layer, all 15 pages: highlights and contents (p1), sections 1-2 Indications, Dosage and Administration (pp2-3), 3-7 (pp3-7), 8 Use in Specific Populations (pp7-9), 10-11 (pp9-10), 12.1-12.5 Clinical Pharmacology (pp10-12), 13-17 (pp12-15); page image of p11 (12.2-12.3)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Lopressor tablets label (2023): metoprolol tartrate immediate release

## Study
US prescribing information for Lopressor (metoprolol tartrate) 50 and 100 mg tablets; Validus, product of Spain;
"Initial U.S. Approval: 1992" as printed (p1). Not a study.

## Model and parameters
No model. Label facts:
```yaml
parameters:
  - {name: F_oral, value: 0.50, unit: fraction, source: "12.3 p11", note: "about 50%, presystemic metabolism saturable, exposure rises more than dose"}
  - {name: Vd, value: "3.2-5.6", unit: L/kg, source: "12.3 p11"}
  - {name: albumin_binding, value: 10, unit: "%", source: "12.3 p11", note: "about"}
  - {name: t_half, value: "3-4", unit: h, source: "12.3 p12", note: "7-9 h in CYP2D6 poor metabolisers"}
  - {name: time_to_max_beta_blockade, value: 20, unit: min, source: "12.2 p11", note: "after a 10-min IV infusion in normal volunteers"}
  - {name: oral_iv_equipotent_ratio, value: 2.5, unit: ratio, source: "12.2 p11", note: "oral:IV dose for equal maximal beta-blockade"}
  - {name: conc_30_80pct_beta1, value: "30-540", unit: nmol/L, source: "12.2 p11", note: "8-23% reduction in exercise heart rate"}
  - {name: beta1_selectivity_lost_above, value: 300, unit: nmol/L, source: "12.2 p11"}
```
Dosing (section 2, pp2-3): hypertension 100 mg/day in single or divided doses, range 100-450 mg/day; angina
100 mg/day in two divided doses, 100-400 mg/day; MI: 50 mg every 6 h starting 15 min after the last IV dose for
48 h, then 100 mg twice daily. Exercise heart-rate effect half-gone at 3.3, 5.0 and 6.4 h after 20, 50 and 100 mg
orally (12.2). Urine: under 5% of an oral and under 10% of an IV dose unchanged in extensive metabolisers, up to
30-40% in poor (12.3). CYP2D6 absent in about 8% of Caucasians (12.5). Quinidine tripled S-metoprolol (12.3).
Renal failure: availability and half-life not clinically different (8.7, 12.3). Hepatic impairment not studied
(8.6). Metoprolol tartrate Mw 684.82 (2:1 salt) (11).

## Pharmacodynamics and end points
Maximum beta-blockade about 20 min after a 10-min IV infusion (end point: beta-blockade; the measure is not specified). Log plasma concentration linear with reduction in exercise heart rate;
antihypertensive effect not related to concentration (12.2). Not an opioid.

## Covariates
CYP2D6 phenotype and inhibitors; elderly slightly higher concentrations; no renal adjustment.

## Caveats
Highlights (p1) say once daily with food, while section 2.1 allows single or divided doses. Section 3 describes
GEIGY-imprinted tablets, section 16 LOPRESSOR-imprinted ones. No population-PK model.

## Used by stanpumpR
- R/drugs_metoprolol.R:58-60 and inst/help/drugs/metoprolol.md:11: maximal beta blockade about 20 min after a
  10-min infusion in volunteers (context for the tPeak choice).
- inst/extdata/vasoactiveRegistry.csv:72 (Ritchie row) repeats it as an argument against 3.3 min.
- inst/extdata/antihypertensiveRegistry.csv:87 (metoprolol__tartrate_ir_tablet__oral__catalog__none__label) and
  tests/testthat/test-antihypertensive-registry.R:178-182 (record IDs only).

## Verification against stanpumpR
- "maximal beta blockade about 20 minutes after a 10-minute infusion in volunteers" (drugs_metoprolol.R:58-60;
  help :11; registry 72): **matches** 12.2 (p11, page image). The repo names only "the label (Lopressor)": no
  holder or version, and the same sentence is in the Lopressor injection label (NDA 018704, p8).
- Registry :87 "F about 50%, terminal half-life 3-4 h", CYP2D6- and enantiomer-dependent: **matches** 12.3. The
  record's URL is a DailyMed setid, not this FDA file; I cannot confirm they are the same version.
- Test :178-182 asserts only that the record IDs exist: **not applicable**.
- Doses entered as metoprolol tartrate (help :11): consistent with this product, labelled as the tartrate.
