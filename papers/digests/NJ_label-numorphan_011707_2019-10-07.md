---
file: NJ_label-numorphan_011707_2019-10-07.pdf
pmid: none
doi: none
citation: Endo Pharmaceuticals. OPANA (oxymorphone hydrochloride) Injection, CII. US prescribing information, NDA 011707 (the former NUMORPHAN application), revised 10/2019 (FDA Reference ID 4500828; accessdata.fda.gov 011707s037lbl.pdf). Stand-in for the cited NUMORPHAN entry, Physicians' Desk Reference 54th ed., 2000, which is not archived.
kind: label
drugs: [oxymorphone]
source_read: text layer; read Highlights, section 2 (2.1-2.5), 12.2-12.3 and 8.5-8.7 in full; skimmed the rest
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
---

# OPANA Injection label 2019 (NDA 011707, ex-NUMORPHAN): parenteral oxymorphone

## Study
Prescribing information, not a study. Product: oxymorphone HCl injection 1 mg/mL for IV, IM or SC use. No PK study design, n or assay given for the injection.

Dosing (2.2): SC or IM, initially 1 to 1.5 mg every 4 to 6 h as needed; IV, 0.5 mg initially, cautiously increased in non-debilitated patients; labor analgesia 0.5 to 1 mg IM. Conversion from oral OPANA: "Given the absolute oral bioavailability of approximately 10%", give one-tenth of the total daily oral dose as injection in 4 or 6 divided doses (example: 40 mg/day oral is about 1 mg IM every 6 h). Renal: use reduced doses when CLcr < 50 mL/min (2.4). Hepatic: contraindicated in moderate or severe impairment (2.3).

## Model and parameters
No compartmental model. Section 12.3:
```yaml
parameters:
  - {name: Vss_IV, value: 3.08, unit: L/kg, source: "12.3 Distribution", note: "mean +/- SD 1.14, healthy male and female subjects"}
  - {name: F_oral, value: 0.10, unit: fraction, source: "2.2 conversion paragraph", note: "approximately 10% absolute"}
  - {name: onset, value: "5-10", unit: min, source: "12.3", note: "initial effects perceived after parenteral dose"}
  - {name: duration, value: "3-6", unit: h, source: "12.3", note: "duration of action"}
```
The Elimination heading in 12.3 is followed only by Metabolism and Excretion: **no clearance and no half-life are given** for the injection. Metabolism: oxymorphone-3-glucuronide (AUC about 90-fold the parent; activity not evaluated) and 6-OH-oxymorphone (animal analgesic activity; AUC about 70% of parent after single oral doses, about equal at steady state). < 1% excreted unchanged; 33-38% in urine as the 3-glucuronide.

## Pharmacodynamics and end points
- MEAC: no value; 12.2 states the minimum effective analgesic concentration varies widely and may rise with tolerance.
- No ke0, t1/2ke0, tPeak or hysteresis. Clinical onset 5-10 min and duration 3-6 h after parenteral dosing (12.3) are the only time-course statements; end point not defined (clinical analgesia).
- Potency ratio stated only as route conversion: parenteral = oral / 10 (2.2), justified by F of about 10%.

## Covariates
All from oral extended-release oxymorphone studies (12.3): hepatic, oral bioavailability 1.6-fold (mild, n = 6), 3.7-fold (moderate, n = 5), 12.2-fold (one severe patient); renal, bioavailability up 26% (CLcr 51-80), 57% (30-50) and 65% (< 30 mL/min), n = 8 each; elderly about 40% higher plasma levels; sex differences disappear when adjusted for weight. Effects on the injection itself not studied.

## Caveats
This is the 2019 label of the same application, not the PDR 2000 NUMORPHAN entry the repo cites; the older text may have carried the clearance and half-life that this one lacks. Vss given per kg without body weights. Special-population data are oral-ER only.

## Used by stanpumpR
- R/drugs_oxymorphone.R:6-12 (context :9): header, manufacturer summary CL 2.0 +/- 0.5 L/min, Vss 3.08 +/- 1.14 L/kg, terminal t1/2 1.3 +/- 0.7 h; one compartment because no central volume exists.
- R/drugs_oxymorphone.R:62-63 and 176-178 (context :61-62, :176-177): reference list and reference string.
- R/drugs_oxymorphone.R:153-154 (context :152): VSS_PER_KG 3.08; CL_PER_KG 120/70 L/h/kg.
- inst/help/drugs/oxymorphone.md:3: CL 2.0 L/min, Vss 3.08 L/kg, t1/2 1.2 h modelled against 1.3 reported.
- tests/testthat/test-drugs-oxymorphone.R:5, :44: pins CL 2.0, Vss 3.08 L/kg, t1/2 1.3 h.
- inst/help/investigators.md:65 (context :54): "Endo (manufacturer summary)".

## Verification against stanpumpR
- Vss 3.08 +/- 1.14 L/kg (drugs_oxymorphone.R:10, :153; oxymorphone.md:3; test :44): **matches** (12.3 Distribution), units L/kg.
- CL 2.0 +/- 0.5 L/min and t1/2 1.3 +/- 0.7 h (drugs_oxymorphone.R:9-11; oxymorphone.md:3; test-drugs-oxymorphone.R:44): **cannot verify**: absent from this label. They need the PDR 2000 NUMORPHAN text itself.
- Oral F 0.10 (drugs_oxymorphone.R:37, 173): **matches** this label too (2.2), as well as the OPANA tablet label.
- One compartment at Vss: no support or contradiction here beyond the absence of any central volume, which matches the code's statement that none is published in the label.
- Observation: onset 5-10 min and duration 3-6 h are compatible with the provisional tPeak 20 min only loosely; the label gives no tPeak.
