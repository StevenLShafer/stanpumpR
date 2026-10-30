---
file: NJ_label-roxicodone_021011_2026-01-06.pdf
pmid: none
doi: none
citation: SpecGx LLC (Mallinckrodt). ROXICODONE (oxycodone HCl) tablets, for oral use, CII (15 and 30 mg). US prescribing information, NDA 021011, revised 12/2025 (FDA Reference ID 5715819; accessdata.fda.gov 2025/021011s016lbl.pdf, posted 2026-01-06).
kind: label
drugs: [oxycodone]
source_read: text layer; read Highlights, section 2 (2.1-2.4), 12.2-12.3 in full; Table 2 re-extracted from page 25 with pypdf layout mode because the pdftotext layer scrambled its columns; skimmed the rest
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# ROXICODONE label 2025: immediate-release oral oxycodone

## Study
Prescribing information. PK from unnamed studies in "normal volunteers"; n, assay and fasting state of the single-dose rows not stated.

Dosing (2.3): opioid-naive start 5 to 15 mg every 4 to 6 h as needed. Conversion: factor in the prior opioid's potency relative to oxycodone; no ratio given. Relative bioavailability versus extended-release oxycodone stated as unknown.

## Model and parameters
Table 2 (p. 25), mean +/- SD; doses as the hydrochloride:

| Regimen | AUC (ng.h/mL) | Cmax (ng/mL) | Tmax (h) | t1/2 (h) |
|---|---|---|---|---|
| 5 mg tabs x 3, SD | 133.2 +/- 33 | 22.3 +/- 8.2 | 1.8 +/- 1.8 | 3.73 +/- 0.9 |
| 15 mg tab, SD | 128.2 +/- 35.1 | 22.2 +/- 7.6 | 1.4 +/- 0.7 | 3.55 +/- 1.0 |
| Liquid Concentrate 15 mg oral solution, SD | 130.6 +/- 34.7 | 21.1 +/- 6.1 | 1.9 +/- 1.5 | 3.71 +/- 0.8 |
| 30 mg tab, SD | 268.2 +/- 60.7 | 39.3 +/- 14.0 | 2.6 +/- 3.0 | 3.85 +/- 1.3 |
| 10 mg/10 mL solution, fasted | 105 +/- 6.2 | 19.0 +/- 3.7 | 1.25 +/- 0.5 | 2.9 +/- 0.4 |
| 10 mg/10 mL solution, fed | 133 +/- 25.2 | 17.7 +/- 3.0 | 2.54 +/- 1.2 | 3.3 +/- 0.5 |
| 5 mg tabs q6h x 14 (AUC 72-84 h) | 113.3 +/- 24.0 | 15.7 +/- 3.2 | 1.3 +/- 0.3 | n/a (Cmin 7.4, Cavg 9.4) |
| 3.33 mg solution q4h x 21 | 99.0 +/- 24.8 | 12.9 +/- 3.1 | 1.0 +/- 0.3 | n/a (Cmin 7.2, Cavg 9.7) |

```yaml
parameters:
  - {name: F_oral, value: "0.60-0.87", unit: fraction, source: "12.3 Absorption", note: "relative to a parenteral dose"}
  - {name: F_rel_15mg_tab, value: 0.96, unit: fraction, source: "12.3", note: "vs 5 mg tablets; 30 mg is 1.01; bioequivalent"}
  - {name: Vss_IV, value: 2.6, unit: L/kg, source: "12.3 Distribution"}
  - {name: CL_IV, value: 0.8, unit: L/min, source: "12.3 Excretion", note: "total plasma clearance, adults"}
  - {name: t_half_apparent, value: "3.5-4", unit: h, source: "12.3 Excretion"}
  - {name: protein_binding, value: 45, unit: "%", source: "12.3", note: "37 C, pH 7.4"}
  - {name: noroxycodone_AUC_ratio, value: 0.6, unit: ratio, source: "12.3 Metabolism", note: "major circulating metabolite; oxymorphone low"}
```
Food (5 mg/5 mL solution, high-fat meal): AUC up 27%, rate unchanged, Tmax 1.25 to 2.54 h. Steady state in 18-24 h. Metabolism: N-dealkylation to noroxycodone (CYP3A4), O-demethylation to oxymorphone (CYP2D6). Urine: free oxycodone up to 19%, conjugated up to 50%, conjugated oxymorphone up to 14%.

## Pharmacodynamics and end points
- MEAC: no value; 12.2 says the minimum effective analgesic concentration varies widely and may rise with tolerance.
- No ke0, t1/2ke0, tPeak or hysteresis.
- No equianalgesic or potency ratio.

## Covariates
Geriatric: popPK found no increase in concentrations over 65 y. Hepatic and renal: not studied quantitatively; clearance "may decrease".

## Caveats
Single-dose rows do not state fasting (only the 10 mg food-effect pair does). Study sizes absent. The pdftotext layer misaligns Table 2's half-life column; the layout extraction above is unambiguous.

## Used by stanpumpR
- R/drugs_oxycodone.R:291-302 (context :294, :297): "mg PO liquid" takes the tablet's oral parameters; ROXICODONE label 15 mg fasted single dose, solution Cmax 21.1, AUC 130.6 ng.h/mL, tmax 1.9 h; tablet 22.2, 128.2, 1.4 h.
- inst/help/drugs/oxycodone.md:9: label and FDA review found solution and IR tablet equivalent, solution absorbed no faster.
- tests/testthat/test-drugs-oxycodone.R:138: liquid-equals-tablet test, citing the label.

## Verification against stanpumpR
- Solution 21.1 / 130.6 / 1.9 h and tablet 22.2 / 128.2 / 1.4 h (drugs_oxycodone.R:294-296): **matches** Table 2 exactly. The solution row is the 20 mg/mL "Liquid Concentrate". "Fasted" is not stated in the label for these rows (minor).
- "Solution absorbed no faster" (oxycodone.md:9): **matches** (Tmax 1.9 versus 1.4 h; Cmax 21.1 versus 22.2).
- Test (test-drugs-oxycodone.R:138): **matches** the equivalence claim.
- Observations (derived, not citation errors): the model's oral F 0.67 lies inside the label's 60-87%. But the label's means imply apparent oral clearance 15 mg / 128.2 ng.h/mL = 1.95 L/min (as HCl), whereas the model, CL 0.924 L/min at 70 kg (0.0132 L/min/kg; age and renal factors taken as 1) with F 0.67, gives 1.38 L/min, i.e. AUC about 181 ng.h/mL for 15 mg, about 40% above the label (about 27% if the dose is converted to base). The model's peak (30 ng/mL at 35 min, per the help page) is also higher and earlier than the label's Cmax 22.2 ng/mL at Tmax 1.4 h. The label's own IV CL is 0.8 L/min. These reflect the Lalovic calibration, which the help page already says peaks earlier than observed.
