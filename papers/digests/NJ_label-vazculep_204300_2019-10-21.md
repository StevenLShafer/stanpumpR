---
file: NJ_label-vazculep_204300_2019-10-21.pdf
pmid: null
doi: null
citation: "VAZCULEP (phenylephrine hydrochloride) Injection for intravenous use, full prescribing information. Avadel Legacy Pharmaceuticals. NDA 204300, revised 10/2019 (FDA label 204300s008lbl.pdf, 2019-10-21; Reference ID 4507680). https://www.accessdata.fda.gov/drugsatfda_docs/label/2019/204300s008lbl.pdf"
kind: label
drugs: [phenylephrine]
source_read: "text layer (all 10 PDF pages: Highlights, sections 1-17); page images rendered and read for PDF pages 2 (2.1-2.3 dosing) and 9 (12.3 Pharmacokinetics); pages 1 and 8 rendered but not needed beyond the text layer"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: matches
---

# Vazculep label (NDA 204300), rev. 10/2019: phenylephrine hydrochloride injection

## Study
Not a study: US prescribing information for phenylephrine hydrochloride 10 mg/mL injection (Avadel), Initial US approval 1954, revised 10/2019. Indication: clinically important hypotension, mainly from vasodilation, in the setting of anesthesia (section 1, PDF p2). Efficacy rests on 16 published studies (section 14, PDF p10): 9 in ASA 1-2 parturients under neuraxial anesthesia for Cesarean delivery, 6 in non-obstetric surgery under general anesthesia, and 1 under combined general/neuraxial.

Dosing (section 2.2, PDF p2, confirmed on page image). All doses are in mcg of **phenylephrine hydrochloride** (the product strength is 10 mg/mL of the hydrochloride, section 3/11):
- Bolus: 40 to 100 mcg IV, repeated every 1-2 min as needed, not to exceed a total of 200 mcg.
- Infusion: 10 to 35 mcg/min if BP stays below target, not to exceed 200 mcg/min. Titrate to the BP goal.
- Preparation: 100 mcg/mL for boluses (10 mg in 99 mL); 20 mcg/mL for infusion (10 mg in 500 mL) (2.3-2.4). Diluted solution keeps 4 h at room temperature or 24 h refrigerated.
- Doses are absolute, not per kg.

Description (section 11, PDF p8): C9H14ClNO2, MW 203.66 g/mol (the hydrochloride).

## Model and parameters
No compartmental model, study size or variability. Section 12.3 (PDF p9, confirmed on page image):
```yaml
parameters:
  - {name: t_half_effective, value: 5, unit: min, source: "12.3, PDF p9", note: "approximately; after IV infusion"}
  - {name: Vss, value: 340, unit: L, source: "12.3, PDF p9", note: "approximately; absolute, not per kg"}
  - {name: CL, value: 2100, unit: mL/min, source: "12.3, PDF p9", note: "average total serum clearance, approximately; absolute"}
  - {name: t_half_terminal, value: 2.5, unit: h, source: "12.3, PDF p9", note: "plasma terminal elimination half-life"}
  - {name: urinary_recovery_48h, value: 0.86, unit: fraction, source: "12.3, PDF p9", note: "radiolabel; about 80% eliminated in first 12 h"}
  - {name: fraction_unchanged_urine_48h, value: 0.16, unit: fraction, source: "12.3, PDF p9"}
  - {name: m_hydroxymandelic_acid, value: 0.57, unit: fraction of dose, source: "12.3, PDF p9", note: "inactive metabolite"}
  - {name: sulfate_conjugates, value: 0.08, unit: fraction of dose, source: "12.3, PDF p9", note: "inactive metabolite"}
```
Metabolism: mainly monoamine oxidase and sulfotransferase. The label does not say whether the PK values are referred to the base or the hydrochloride.

## Pharmacodynamics and end points
End point: arterial blood pressure (12.2, PDF p8-9).
- Rises in systolic, diastolic and mean pressure and in total peripheral resistance, with reflex bradycardia.
- After a bolus, BP begins to rise rapidly, typically within minutes. There is no numeric onset, peak time, ke0 or EC50.
- **No therapeutic or toxic plasma concentration range** is given. Dosing is titrated to the BP goal (2.2).
- Interactions: MAOIs, oxytocics, tricyclics, angiotensin/aldosterone, atropine, steroids, NET inhibitors and ergots augment the pressor effect. α-blockers, PDE5 inhibitors, mixed α/β-blockers, calcium-channel blockers, benzodiazepines, ACE inhibitors and central sympatholytics reduce it (7.1-7.2).
(Not an opioid: the opioid audit does not apply.)

## Covariates
- Hepatic (8.6): cirrhosis (Child-Pugh B/C) reduces responsiveness, so more drug may be needed.
- Renal (8.7): ESRD increases responsiveness, so start low.
- Autonomic dysfunction (5.7) augments the pressor response. Acidosis may reduce effectiveness (2.1).
- Pediatrics not established; geriatrics start low.
- No PK covariate equations.

## Caveats
- A label: no study n, assay, population or variability for the PK figures.
- PK values are approximate and absolute (not weight-normalised).
- The effective half-life (5 min) and terminal half-life (2.5 h) differ by about 30-fold. A one-compartment reading of either would mislead.

## Used by stanpumpR
- inst/extdata/therapeuticRanges.csv:194 (the phenylephrine row is at line 195 in the snapshot) — registry source: no therapeutic concentration range; titrated to effect.

## Verification against stanpumpR
Checked against `C:\dev\stanpumpRPapers\_build\master_snapshot` (master 50bb4219).

- **inst/extdata/therapeuticRanges.csv:195** (context said :194): lower/upper/typical 0 ng/mL, band_status none_established, "no therapeutic concentration range; titrated to effect (label read: VAZCULEP, rev. 10/2019)". The label gives no plasma concentration range and says to adjust the dose to the BP goal (2.2, PDF p2, image). Its revision date is 10/2019 (Highlights, PDF p1; "Rev. 10/2019" PDF p10). **matches**.
- Cross-check outside the cited line (the label is not cited there):
  - R/drugs_phenylephrine.R:39 and :129-131 use Hengstmann 1982: CL 2095 mL/min, Vdss 340 L, "Doses as the hydrochloride". The label's approximately 2100 mL/min and 340 L (PDF p9, image) agree.
  - The model's 164-min slow half-life is close to the label's 2.5 h terminal half-life (150 min).
  - The label doses in mcg of the hydrochloride, which supports entering doses as the hydrochloride. The label does not settle the salt basis of the PK values.
