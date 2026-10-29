---
file: NJ_label-nitroglycerin-d5w_019970_2016-08-23.pdf
pmid: null
doi: null
citation: "Baxter Healthcare Corporation. Nitroglycerin in 5% Dextrose Injection, prescribing information (NDA 019970; label Rev. August 2016, FDA Reference ID 3975287; FDA posting 2016-08-23)."
kind: label
drugs: [nitroglycerin]
source_read: "text layer, whole label (12 PDF pages: Description, Clinical Pharmacology incl. Pharmacokinetics, Indications, Contraindications, Warnings, Precautions, Adverse Reactions, Overdosage, Dosage and Administration, How Supplied); page images rendered and read for PDF pages 3, 10, 11"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: matches
open_questions:
  - "[source] R/drugs_nitroglycerin.R:89-93 (not a location this card is cited for) calls McNiff 1981 'the source the US label quotes'; this label (PDF p3) gives Vd about 3 L/kg, t1/2 about 3 min and clearance 'close to 1 L/kg/min' but names no source. Confirm the attribution from McNiff 1981 or the NDA review, or drop it."
---

# Baxter label 2016: Nitroglycerin in 5% Dextrose Injection (NDA 019970)

## Study
Not a study: US prescribing information for premixed IV nitroglycerin in 5% dextrose (Baxter glass containers,
250 mL holding 25, 50 or 100 mg). Rev. August 2016 (PDF p12). No study population, assay or model; the
PK statements are given without citations or a study description.

Formulation (PDF p1-2, Table 1): each 100 mL holds 10, 20 or 40 mg nitroglycerin, i.e. 100, 200, 400 mcg/mL;
dextrose hydrous 50 g/L; osmolarity (calc) 428, 440, 465 mOsmol/L; pH 4.0 (3.0 to 5.0); 0.84 mL alcohol and
105 mg citric acid per 100 mL; propylene glycol from the diluted nitroglycerin. MW 227.09.

## Model and parameters
No model. The label's PK paragraph (PDF p3, "Pharmacokinetics"; confirmed on the rendered page):
- Volume of distribution about 3 L/kg.
- Serum half-life about 3 minutes.
- Clearance "close to 1 L/kg/min", exceeding hepatic blood flow; extrahepatic metabolism in red cells and vessel walls.
- Metabolites: inorganic nitrate and 1,2- and 1,3-dinitroglycerols (less potent vasodilators, longer-lived),
  then mononitrates (non-vasoactive), glycerol and CO2.

```yaml
parameters:
  - {name: Vd, value: 3, unit: L/kg, source: "PDF p3, Pharmacokinetics", note: "'about'; no source, population or SD given"}
  - {name: t_half, value: 3, unit: min, source: "PDF p3, Pharmacokinetics", note: "'about'; serum half-life"}
  - {name: CL, value: 1, unit: L/kg/min, source: "PDF p3, Pharmacokinetics", note: "'close to'; exceeds hepatic blood flow"}
```

## Pharmacodynamics and end points
- No therapeutic or minimum effective plasma concentration. The label says that dosing to keep plasma
  levels above a minimally effective concentration is inappropriate for organic nitrates (tolerance) (PDF p2).
- Overdosage: serum levels of nitroglycerin and metabolites have no established role in management (PDF p9).
- Dosing is titrated to haemodynamic effect (BP, HR, often PCWP). Using non-adsorptive tubing: start
  5 mcg/min, increase by 5 mcg/min every 3 to 5 min; if no response at 20 mcg/min, steps of 10 and 20 mcg/min;
  smaller, less frequent steps once a response is seen (PDF p11, rendered). Some patients respond fully to
  5 mcg/min, others need one or two orders of magnitude more (PDF p10). Published starting doses of 25 mcg/min
  or more were through PVC tubing, which delivers 20-60% of the nominal dose (PDF p4, p10).
- Paediatric literature doses 0.5 to 5 mcg/kg/min (PDF p7-8); infusion-rate formula mL/h = dose x 60 / concentration.
- Tolerance: drug-free intervals of 10-12 h suffice; continuous IV infusion lost almost all haemodynamic
  effect by 48 h in one study (PDF p3).
- Methaemoglobinaemia: about 1 mg/kg would be needed for clinically significant (>= 10%) MetHb even without
  cytochrome b5 reductase; 36 patients on 3.1-4.4 mg/h for 2-4 weeks averaged 0.2% MetHb; treat with
  methylene blue 1-2 mg/kg IV (PDF p10, rendered).
- No ke0, EC50 or exposure-response model.

## Covariates
None quantified. Geriatric: start at the low end of the range. Fluid load matters in cardiac, hepatic or renal
impairment.

## Caveats
- PK values are rounded figures without source, population, or variability.
- Typo on PDF p11 (rendered): Example 2 states a dose of 240 mcg/min with 400 mcg/mL, but the worked formula
  shows "5 (mcg/min)"; the stated answer 36 mL/h equals 240 x 60 / 400 (derived), so the "5" is the error.
- PDF p11 says flow rates are "shown in the following table", but no such table appears in the label.
- PVC adsorption makes literature doses overestimates of delivered dose.

## Used by stanpumpR
- `inst/extdata/therapeuticRanges.csv:192`: registry source for the nitroglycerin row (no therapeutic range).

## Verification against stanpumpR
- **therapeuticRanges.csv:192**: row `nitroglycerin`, plasma, lower/upper/typical 0/0/0 ng/mL,
  `band_status: none_established`, source "no therapeutic concentration range; titrated to effect (label read:
  nitroglycerin in 5% dextrose, Baxter, rev. 08/2016)". The label gives no therapeutic concentration, says
  plasma-level targeting is inappropriate for nitrates (PDF p2) and that serum levels have no established role
  (PDF p9), and doses by titration to haemodynamic response (PDF p10-11). Manufacturer (Baxter) and revision
  (Rev. August 2016, PDF p12) match. The 0/0/0 values are placeholders for "none", not a range. **matches**.
- Outside this card's locations: `R/drugs_nitroglycerin.R:89-93` quotes Vd about 3 L/kg and t1/2 about 3 min,
  as does the label, and gives McNiff's clearance range 0.3-1 L/min/kg against the label's "close to 1 L/kg/min"
  (same unit, upper end). The label does not name McNiff as its source; see open question.
