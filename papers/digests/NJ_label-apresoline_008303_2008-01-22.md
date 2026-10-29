---
file: NJ_label-apresoline_008303_2008-01-22.pdf
pmid: null
doi: null
citation: "Apresoline hydrochloride (hydralazine hydrochloride USP) tablets. Prescribing information, NDA 008303 (supplement s068), Ciba-Geigy Corporation, C95-14 (Rev. 5/95). FDA label PDF https://www.accessdata.fda.gov/drugsatfda_docs/label/1996/008303s068lbl.pdf (archive file dated 2008-01-22)."
kind: label
drugs: [hydralazine]
source_read: "text layer, all 8 pages (scanned label with an imperfect text layer); page images rendered and read for PDF pages 2 (Clinical Pharmacology) and 7 (Dosage and Administration, How Supplied, revision code)"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: matches
---

# Apresoline label (Rev. 5/95): oral hydralazine prescribing information

## Study
Not a study: prescribing information for oral Apresoline (hydralazine HCl, MW 196.64) tablets 10, 25, 50 and
100 mg (Description, PDF p1). Indication: essential hypertension, alone or as an adjunct. No intravenous
formulation is covered by this label. Revision code C95-14 (Rev. 5/95), Ciba-Geigy (PDF p7, rendered); the
FDA URL path is 1996 and the archive file name carries 2008-01-22.

Dosing (Dosage and Administration, PDF p6-7; p7 rendered): start 10 mg four times daily for 2-4 days, 25 mg
four times daily for the rest of week 1, then 50 mg four times daily; maintenance at the lowest effective dose;
up to 300 mg/day in a few resistant patients. Pediatric (Precautions, PDF p5, text layer): 0.75 mg/kg/day in four
divided doses, increased over 3-4 weeks to a maximum of 7.5 mg/kg or 200 mg daily.

## Model and parameters
No compartmental model. Clinical Pharmacology (PDF p2, rendered page read):
- Rapid oral absorption; peak plasma levels at 1-2 h.
- Plasma levels of apparent hydralazine decline with half-life 3-7 h.
- Plasma protein binding 87%.
- Plasma levels vary widely between individuals; polymorphic acetylation — slow acetylators have higher levels
  and need lower doses.
- Extensive hepatic metabolism; excreted mainly as urinary metabolites.
- Food increases plasma levels (Drug/Food Interactions, PDF p4).
No clearance, volume, bioavailability, Cmax or AUC values are given.

```yaml
parameters:
  - {name: tmax, value: "1-2", unit: h, source: "Clinical Pharmacology, PDF p2", note: oral; range}
  - {name: thalf, value: "3-7", unit: h, source: "Clinical Pharmacology, PDF p2", note: "apparent hydralazine; range"}
  - {name: protein_binding, value: 0.87, unit: fraction, source: "Clinical Pharmacology, PDF p2"}
```

## Pharmacodynamics and end points
End point is blood pressure (direct arteriolar vasodilator; diastolic falls more than systolic; reflex
tachycardia, raised cardiac output and renin). Dose is titrated to individual response. No therapeutic or toxic
plasma concentration range, no concentration-effect relation, no ke0. Overdose (PDF p6, text layer): highest
survived adult dose 10 g orally; rat oral LD50 173 and 187 mg/kg. Not an opioid: no MEAC or potency ratio.

## Covariates
Acetylator phenotype (qualitative only). Food. Renal disease (caution). No equations.

## Caveats
Old (1995) oral-tablet label; no quantitative PK beyond ranges; assay of "apparent hydralazine" unspecified.
The text layer is from a scan and has OCR errors (e.g. "colilrölolölöoa"); numbers above were confirmed on the
rendered pages 2 and 7, while pediatric dosing and overdose numbers are from the text layer only.

## Used by stanpumpR
- `inst/extdata/therapeuticRanges.csv:193` — registry row for hydralazine: no therapeutic concentration range
  (band_status none_established), titrated to effect, "label read: APRESOLINE, rev. 05/1995".

## Verification against stanpumpR
- **therapeuticRanges.csv:193**: the row gives lower/upper/typical 0 with `none_established`, and says the
  label gives no therapeutic concentration range and that hydralazine is titrated to effect. The label contains
  no plasma concentration target anywhere in its 8 pages, says plasma levels vary widely, and directs titration
  to individual response (PDF p2, p7, rendered). The revision cited, "rev. 05/1995", matches "C95-14 (Rev. 5/95)"
  on PDF p7 (rendered). Unit column (ng/mL) is a placeholder with no value and is not addressed by the label.
  **matches**.
