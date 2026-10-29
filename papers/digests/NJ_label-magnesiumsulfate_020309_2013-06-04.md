---
file: NJ_label-magnesiumsulfate_020309_2013-06-04.pdf
pmid: null
doi: null
citation: "Hospira, Inc. Magnesium Sulfate in Water for Injection (flexible plastic container), prescribing information. NDA 020309, label revised 05/2013 (FDA posting 2013-06-04, Reference ID 3315657). https://www.accessdata.fda.gov/drugsatfda_docs/label/2013/020309s028lbl.pdf"
kind: label
drugs: [magnesiumSulfate]
source_read: "text layer (all 5 pages) and page images of PDF pages 1, 2 and 4 (Clinical Pharmacology, Pharmacokinetics, Special Populations, Precautions, Dosage and Administration, How Supplied); the whole label is 5 pages"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: matches
---

# Hospira 2013: Magnesium Sulfate in Water for Injection label (NDA 020309)

## Study
Not a study: an older-format (non-PLR) FDA label for premixed magnesium sulfate heptahydrate (MgSO4·7H2O) in
water, 4% (40 mg/mL) and 8% (80 mg/mL), IV only (PDF p1). Indication: prevention and control of seizures in
pre-eclampsia and eclampsia (p2). No clinical trial, PK study or population is described; there are no PK
tables.

## Model and parameters
No compartmental model, clearance, volume or half-life is given. Qualitative PK (PDF p1, "Pharmacokinetics"):
- Absorption: IV magnesium is immediately available.
- Distribution: about 1-2% of total body magnesium is in the extracellular fluid; magnesium is 30% bound to
  albumin.
- Metabolism: none.
- Excretion: renal only, at a rate proportional to serum concentration and glomerular filtration.

Onset/duration (p1): IV anticonvulsant action is immediate and lasts about 30 minutes; IM onset about 1 h,
lasting 3 to 4 h.

```yaml
parameters: []  # the label gives no PK parameters; serum concentration thresholds are listed below
concentrations:
  - {name: normal serum Mg, lower: 1.3, upper: 2.1, unit: mEq/L, source: "PDF p1, Clinical Pharmacology"}
  - {name: effective anticonvulsant serum Mg, lower: 2.5, upper: 7.5, unit: mEq/L, source: "PDF p1, Clinical Pharmacology"}
  - {name: serum Mg usually sufficient to control convulsions, lower: 2.5, upper: 5, unit: mEq/L, source: "PDF p2, Precautions", note: "stated as 3 to 6 mg/100 mL (2.5 to 5 mEq/L)"}
  - {name: optimal serum Mg for seizure control, value: 6, unit: mg/100 mL, source: "PDF p4, Dosage and Administration", note: "about 4.9 mEq/L (derived: 60 mg/L / 24.305 g/mol x 2 mEq/mmol)"}
  - {name: deep tendon reflexes begin to diminish, value: 4, unit: mEq/L, source: "PDF p1 and p2"}
  - {name: reflexes absent / respiratory paralysis possible, value: 10, unit: mEq/L, source: "PDF p1 and p2"}
```

## Pharmacodynamics and end points
- End points are clinical: patellar (deep tendon) reflex, respiratory rate (about 16 breaths/min or more as a
  sign of a safe regimen, p2), seizure control, blood pressure (flushing/sweating at low doses, hypotension at
  larger ones, p1). Heart block may occur at about 10 mEq/L or lower (p1).
- Concentration thresholds as in the YAML block above (serum, total magnesium; assay not stated).
- No ke0, EC50 or concentration-effect model. Not an opioid; no MEAC or potency ratio applies.
- Note that the label states two different "effective" upper limits: 7.5 mEq/L (Clinical Pharmacology, p1) and
  5 mEq/L (Precautions, p2).

## Covariates
- Renal insufficiency: lower dose, frequent serum levels; maximum 20 g per 48 h in severe renal insufficiency
  (p2, p4). Urine output to be kept at 100 mL every 4 hours (p2).
- Hepatic insufficiency: no adjustment (p2).
- Drugs causing renal magnesium loss (p2): aminoglycosides, amphotericin B, cyclosporine, diuretics, digitalis,
  cisplatin, alcohol.

## Dosing (PDF p4, Dosage and Administration)
- Severe pre-eclampsia/eclampsia: total initial dose 10 to 14 g MgSO4; 4 g IV to start; IV rate generally not
  above 150 mg/min (3.75 mL/min of 4%), except in severe eclampsia with seizures; simultaneously 4 to 5 g
  (32.5 to 40.6 mEq) IM into each buttock (50% injection); then some clinicians give 1 to 2 g/h IV infusion.
- Subsequent IM 4 to 5 g every 4 h guided by patellar reflex, respiration and absence of toxicity.
- Maximum 30 to 40 g per 24 h.
- How Supplied (p4 table): 4 g/100 mL = 32.5 mEq Mg2+; 20 g/500 mL = 162.3 mEq; 40 g/1000 mL = 325 mEq;
  2 g/50 mL = 16.25 mEq; 4 g/50 mL (8%) = 32.5 mEq; all doses as the heptahydrate. Osmolarity 325 mOsmol/L (4%),
  649 mOsmol/L (8%).
- Overdose (p3): IV calcium 10 to 20 mL of a 5% solution; subcutaneous physostigmine 0.5 to 1 mg may help.

## Caveats
- Older non-PLR label; thresholds are stated without supporting data or assay description.
- The 2.5 to 7.5 vs 2.5 to 5 mEq/L inconsistency within the label (p1 vs p2).
- Contraindicated (IV) in toxemia during the 2 hours before delivery (p2); fetal harm warning beyond 5-7 days
  of continuous use (p2).

## Used by stanpumpR
- `inst/extdata/therapeuticRanges.csv:184` — registry source for the magnesiumSulfate therapeutic-range row
  (with Okusanya 2016).

## Verification against stanpumpR
Checked in `C:\dev\stanpumpRPapers\_build\master_snapshot` (stanpumpR master 50bb4219).

- `inst/extdata/therapeuticRanges.csv:184`: analyte "serum magnesium above the patient's baseline", units
  mmol/L, lower/upper/typical NA, band_status `not_applicable`; label_statement "effective anticonvulsant serum
  levels 2.5 to 7.5 mEq/L"; notes convert this to 1.25 to 3.75 mmol/L. The label (PDF p1, confirmed on the
  rendered page) gives exactly 2.5 to 7.5 mEq/L; the conversion is right for a divalent ion
  (derived: 1 mmol Mg2+ = 2 mEq). No band is drawn, so no number is used for plotting. **matches**.
- The row quotes only the Clinical Pharmacology range; the label's Precautions (p2) gives 2.5 to 5 mEq/L
  (3 to 6 mg/100 mL) as usually sufficient, and Dosage (p4) calls 6 mg/100 mL (about 4.9 mEq/L, derived)
  optimal. Not a transcription error, but if a band is ever drawn the upper limit is ambiguous within this
  label. `docs/therapeutic-ranges.md:210` repeats 2.5–7.5 mEq/L (label): consistent.
- The source field's "Hospira, rev. 05/2013" matches the label's "Revised: 05/2013" (p5).
