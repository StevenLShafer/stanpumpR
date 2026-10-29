---
file: NJ_label-tramadol_020281_2023-02-14.pdf
pmid: none
doi: none
citation: Janssen Pharmaceuticals. ULTRAM (tramadol hydrochloride) tablets, for oral use, C-IV (50 mg, immediate release). US prescribing information, NDA 020281, revised 02/2023 (FDA Reference ID 5125834; accessdata.fda.gov 2023/020281s040lbl.pdf).
kind: label
drugs: [tramadol, desmetramadol]
source_read: text layer; read Highlights, section 2.3, 8.5-8.6, 12.1-12.3 (incl. Table 3) in full; skimmed the rest
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
---

# ULTRAM label 2023: immediate-release tramadol tablets

## Study
Prescribing information. PK from unnamed healthy-adult and special-population studies; n and assay not given. Racemic tramadol and racemic M1 (O-desmethyltramadol) reported.

Dosing (2.3): titrate from 25 mg/day by 25 mg every 3 days to 100 mg/day, then by 50 mg every 3 days to 200 mg/day; then 50-100 mg every 4-6 h as needed, maximum 400 mg/day (or 50-100 mg every 4-6 h from the start when rapid onset is needed). CLcr < 30 mL/min: every 12 h, maximum 200 mg/day. Severe hepatic impairment: 50 mg every 12 h. Over 75 y: maximum 300 mg/day. Relative bioavailability versus extended-release tramadol stated as unknown.

## Model and parameters
No compartmental model. Section 12.3 and Table 3 (mean, %CV):
```yaml
parameters:
  - {name: F_oral_100mg, value: 0.75, unit: fraction, source: "12.3 Absorption", note: "mean absolute bioavailability of a 100 mg oral dose, approximately"}
  - {name: Tmax_tramadol_text, value: 2, unit: h, source: "12.3 Absorption", note: "mean peak of racemic tramadol; M1 at 3 h"}
  - {name: Cmax_tramadol_100mg_SD, value: 308, unit: ng/mL, source: "Table 3", note: "CV 25%"}
  - {name: Tmax_tramadol_100mg_SD, value: 1.6, unit: h, source: "Table 3", note: "CV 63%"}
  - {name: CL_over_F_100mg_SD, value: 8.50, unit: mL/min/kg, source: "Table 3; 12.3 Elimination", note: "CV 31%, apparent oral clearance"}
  - {name: t_half_tramadol_100mg_SD, value: 5.6, unit: h, source: "Table 3", note: "CV 20%"}
  - {name: Cmax_M1_100mg_SD, value: 55.0, unit: ng/mL, source: "Table 3", note: "CV 36%; Tmax 3.0 h (51%); t1/2 6.7 h (16%)"}
  - {name: Cmax_tramadol_100mg_qid, value: 592, unit: ng/mL, source: "Table 3", note: "CV 30%; Tmax 2.3 h; CL/F 5.90 mL/min/kg; t1/2 6.7 h"}
  - {name: Cmax_M1_100mg_qid, value: 110, unit: ng/mL, source: "Table 3", note: "CV 29%; Tmax 2.4 h; t1/2 7.0 h"}
  - {name: t_half_terminal, value: 6.3, unit: h, source: "12.3 Elimination", note: "+/- 1.4 tramadol; M1 7.4 +/- 1.4 h"}
  - {name: Vd_IV, value: "2.6 (male), 2.9 (female)", unit: L/kg, source: "12.3 Distribution", note: "after 100 mg IV"}
  - {name: protein_binding, value: 20, unit: "%", source: "12.3 Distribution"}
  - {name: urine_unchanged, value: 30, unit: "% of dose", source: "12.3 Metabolism"}
```
Other Table 3 rows: geriatric > 75 y, 50 mg, tramadol Cmax 208 ng/mL, t1/2 7.0 h, CL/F 6.89 mL/min/kg (M1 not measured); hepatic impaired 50 mg, tramadol t1/2 13.3 h, CL/F 4.23, M1 t1/2 18.5 h; renal impaired 100 mg IV, CL 4.23 (CLcr 10-30) and 3.73 mL/min/kg (CLcr < 5), tramadol t1/2 10.6 and 11.0 h, M1 11.5 and 16.9 h. Food: no significant effect. Steady state within 2 days on qid dosing; linear PK 50-100 mg.

## Pharmacodynamics and end points
- MEAC: no value; 12.2 states the minimum effective analgesic concentration varies widely and may rise with tolerance.
- No ke0, t1/2ke0 or tPeak. Clinical time course (12.1): analgesia begins within about 1 h and peaks at about 2-3 h (end point: analgesia, method not given).
- Potency (12.1, animal data): M1 up to 6 times as potent as tramadol for analgesia and 200 times in mu-opioid binding. No equianalgesic ratio to morphine.
- CYP2D6 (12.3): about 7% of the population are poor metabolisers; popPK in healthy subjects, tramadol about 20% higher and M1 40% lower in poor versus extensive metabolisers.

## Covariates
Age > 75 y (higher Cmax 208 versus 162 ng/mL, t1/2 7 versus 6 h, versus 65-75 y), hepatic cirrhosis, renal impairment, CYP2D6 phenotype, as above. Dialysis removes < 7% in 4 h.

## Caveats
Table 3 footnote markers are misplaced in the text layer ("c", not applicable, attached to tramadol clearances). The label does not separate enantiomers. The capsule and extended-release products the repo mentions are not in this label.

## Used by stanpumpR
- R/drugs_tramadol.R:55-60 (context :55): F 0.70 "the regulatory figure for capsules"; 100 mg IR tablet quoted at 0.75; 24 h ER product at 85-90% of its IR comparator; only an IR oral unit offered.
- (Also inst/help/drugs/tramadol.md:3: F 0.70 for capsules, oral peak near 1 h; not listed in the context.)

## Verification against stanpumpR
- 100 mg IR tablet F 0.75 (drugs_tramadol.R:56): **matches** (12.3, "approximately 75%").
- F 0.70 for capsules (drugs_tramadol.R:55, :176; tramadol.md:3): **cannot verify**; ULTRAM is a tablet and no capsule figure appears. The source for 0.70 is not named in the repo and is not this label.
- ER at 85-90% of IR (drugs_tramadol.R:56-57): **cannot verify** here (ULTRAM ER or similar label needed).
- Cross-checks (derived): model Vss 169 L = 2.4 L/kg at 70 kg versus label 2.6-2.9 L/kg; model CL/F = 28.9 / 0.70 = 41.3 L/h = 9.8 mL/min/kg versus label 8.50 (CV 31%), i.e. model single-dose oral AUC about 13% below the label mean; model oral peak at 0.99 h versus label Tmax 1.6 h (Table 3, CV 63%) and "two hours" (12.3 text). All within label variability except that the model's peak is earlier than either label figure.
