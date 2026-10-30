---
file: NJ_fda-019152orig1s045lbl.pdf
pmid: null
doi: null
citation: "CALAN SR (verapamil hydrochloride) Extended-Release Tablets, G.D. Searle LLC, Division of Pfizer Inc. Prescribing information LAB-0268-13.0, Revised May 2020, with container and carton labels; NDA 019152, supplement S-045 (Drugs@FDA file label/2020/019152Orig1s045lbl.pdf, FDA signature dated 2020-05-22)."
kind: label
drugs: [verapamil]
source_read: "text layer of PDF p1-13 (one-column old-format label, no TOC): Description p1, Clinical Pharmacology p1-4 (mechanism p2, pharmacokinetics and metabolism p3-4, hemodynamics p4), Indications p4-5, Contraindications p5, Warnings p5-7, Precautions p7-11 (Pregnancy, Labor, Nursing, Pediatric p11), Adverse Reactions p11-12, Overdosage p13, Dosage and Administration p13-14; page images of p14 (end of Dosage, How Supplied), p15-19 (container and carton labels) and p20 (FDA signature page)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# CALAN SR (extended-release verapamil) label, May 2020

## Study
US prescribing information for CALAN SR, distributed by G.D. Searle LLC (Pfizer). PDF p1-14 are the package insert
(LAB-0268-13.0, "Revised May 2020", p14), p15-19 bottle and shipper labels for 120, 180 and 240 mg (NDC
0025-1901/2712, 1911/2818, 1891/2924; the older bottle art says "sustained-release caplets", the newer
"extended-release tablets"), p20 the FDA reviewer's digital signature dated 2020-05-22. Tablets: 240 mg HCl (222.16
mg base), 180 mg (166.62 mg base), 120 mg (111.08 mg base); scored tablets may be halved without changing release.
The text layer of p14-19 is empty (image pages); the text file stops at p13, so p14 was read from the render. No
Geriatric Use section; ageing is mentioned in Clinical Pharmacology.

## Model and parameters
Pharmacokinetics and metabolism (PDF p3-4); values are for the **immediate-release** product unless stated:
- More than 90% absorbed; bioavailability 20-35% (first-pass); Tmax 1-2 h.
- 120 mg every 6 h chronically: plasma verapamil 125-400 ng/mL. Nonlinear dose-level relation. PR prolongation
  correlates with concentration early in titration; the relation may disappear in chronic dosing. No relation
  established between concentration and fall in blood pressure.
- Mean elimination half-life 2.8-7.4 h in single-dose studies, 4.5-12.0 h after repetitive dosing (fewer than 10
  doses 6 h apart); may increase during titration; may be prolonged in the elderly.
- CALAN SR: multiple-dose fasting AUC similar to IR; absorption rates differ. Single 240 mg SR dose in healthy
  volunteers (randomised crossover): with food Cmax 79 ng/mL, Tmax 7.71 h, AUC0-24 841 ng·h/mL; fasting Cmax 164
  ng/mL, Tmax 5.21 h, AUC0-24 1478 ng·h/mL; similar for norverapamil.
- Norverapamil: steady-state concentrations about equal to verapamil's; cardiovascular activity about 20% of
  verapamil's. About 70% of a dose in urine as metabolites, 16% or more in faeces in 5 days, 3-4% unchanged; about
  90% protein bound. Hepatic insufficiency: IR half-life up to 14-16 h, volume increased, clearance about 30% of
  normal. CSF:plasma partition 0.06 (verapamil), 0.04 (norverapamil) after 4 weeks of 120 mg q.i.d.
- Metabolised by CYP3A4, 1A2, 2C8, 2C9 and 2C18 (Precautions, p7).

```yaml
parameters:
  - {name: F_IR, value: "0.20-0.35", unit: fraction, source: "Clinical Pharmacology, PDF p3", note: "immediate-release, range"}
  - {name: t_half_single_dose, value: "2.8-7.4", unit: h, source: "PDF p3", note: "mean elimination half-life, single-dose studies, IR"}
  - {name: t_half_repeated_dose, value: "4.5-12.0", unit: h, source: "PDF p3", note: "after < 10 doses q6h, IR"}
  - {name: Cmax_SR240_fed, value: 79, unit: ng/mL, source: "PDF p3", note: "single 240 mg CALAN SR with food, healthy volunteers"}
  - {name: Tmax_SR240_fed, value: 7.71, unit: h, source: "PDF p3", note: "with food"}
  - {name: AUC0_24_SR240_fed, value: 841, unit: ng*h/mL, source: "PDF p3", note: "with food"}
  - {name: Cmax_SR240_fasted, value: 164, unit: ng/mL, source: "PDF p3", note: fasting}
  - {name: Tmax_SR240_fasted, value: 5.21, unit: h, source: "PDF p3", note: fasting}
  - {name: AUC0_24_SR240_fasted, value: 1478, unit: ng*h/mL, source: "PDF p3", note: fasting}
  - {name: protein_binding, value: 0.90, unit: fraction, source: "PDF p3", note: approximately}
  - {name: Css_range_IR_120q6h, value: "125-400", unit: ng/mL, source: "PDF p3", note: "chronic 120 mg every 6 h"}
```

## Pharmacodynamics and end points
No concentration-effect model. Antihypertensive effect evident within the first week (Dosage, p13). Bradycardia
< 50/min 1.4%; CHF or pulmonary oedema 1.8% and hypotension 2.5% of 4954 trial patients; higher-degree AV block 0.8%
(Warnings p5-6). The label states no therapeutic or toxic concentration range beyond the 125-400 ng/mL observed on
120 mg q6h IR. Food lowers SR AUC but narrows the peak-to-trough ratio.

## Dosing (Dosage and Administration, PDF p13-14)
With food. Start 180 mg each morning (120 mg/day in the elderly or small people); titrate weekly, assessed about 24
h after a dose: 240 mg each morning; then 180 mg morning plus 180 mg evening, or 240 mg plus 120 mg; then 240 mg every
12 h. Switching from IR CALAN keeps the same total daily dose. Severe hepatic dysfunction: about 30% of the normal
dose (Precautions, p7). Overdose: observe at least 48 h; SR concretions reported.

## Covariates
Ageing (half-life may be prolonged), hepatic insufficiency (CL about 30% of normal), food (SR AUC 841 vs 1478),
CYP3A4 inhibitors and inducers; renal impairment: cautious use, not removed by haemodialysis. Paediatric safety
not established below 18 years.

## Caveats
Old-format label; numbers are summary ranges from unnamed studies, mostly for the IR product. The SR food study
does not give n. The file's last pages are artwork and a signature page, not label text.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:136` (from `data-raw/antihypertensiveRegistry.R:230`): record
  `verapamil__er_tablet__oral__catalog__none__label`, racemic verapamil, oral extended-release tablet (CALAN SR),
  plasma, official_label_descriptive, descriptive_only, BP simulation FALSE; limitations "IR absolute F 20-35%,
  multiple-dose half-life 4.5-12 h, and the ER input differs from IR", and the controlled-onset record (a different
  product and analyte) must not be used here.

## Verification against stanpumpR
- **Product and route** (csv `:136`): CALAN SR extended-release verapamil HCl tablets, oral. **Matches.**
- **F 20-35%**: the label gives 20-35% for the immediate-release formulation, as the row says; the label does not
  call it "absolute", the row's word. **Matches** (wording only).
- **Multiple-dose half-life 4.5-12 h**: PDF p3. **Matches.**
- **ER input differs from IR**: the label says absorption rates differ while multiple-dose AUC is similar, and gives
  SR Tmax 5.2-7.7 h. **Matches.**
- **"Racemic verapamil"**: the label does not mention stereochemistry; not contradicted, not verified from it.
- Descriptive-only status with no fitted parameters is consistent with a label that has none.
