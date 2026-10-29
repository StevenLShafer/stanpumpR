---
file: NJ_label-depakote_018723_2026-04-02.pdf
pmid: none
doi: none
citation: "DEPAKOTE (divalproex sodium) delayed-release tablets, US prescribing information, AbbVie; NDA 018723 (s073), revised 3/2026 (FDA Reference ID 5770126; posted 2026-04-02). https://www.accessdata.fda.gov/drugsatfda_docs/label/2026/018723s073lbl.pdf"
kind: label
drugs: [valproate]
source_read: "text layer, 54 pages (one form-feed chunk per PDF page): Highlights and contents (pp. 1-2), 2.1 Mania, 2.2 Epilepsy, 2.4 General Dosing Advice (pp. 5-8), 7.2 ethosuximide paragraph (p. 25), 8.1 Pregnancy (from p. 27, scanned for PK statements), 12.2-12.3 (pp. 35-36); PDF p. 6 (2.2, the range and thrombocytopenia statements) rendered and read"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master ca8ce42
verification: matches
---

# Depakote label 2026: divalproex delayed-release tablets, the valproate range

## Study
Regulatory label (PLR format), revised 3/2026. Divalproex sodium delayed-release tablets; doses expressed as
valproic acid equivalents. Indications: mania, epilepsy (complex partial and absence seizures), migraine
prophylaxis.

## Model and parameters
No compartmental model. Section 12.3 (PDF pp. 35-36) repeats the class values given in the Depakene label:

```yaml
parameters:
  - {name: CL_total, value: 0.56, unit: "L/h/1.73 m2", source: "12.3 Elimination, PDF p36", note: "total valproate; patients not on enzyme inducers"}
  - {name: V_total, value: 11, unit: "L/1.73 m2", source: "12.3 Elimination, PDF p36"}
  - {name: CL_free, value: 4.6, unit: "L/h/1.73 m2", source: "12.3 Elimination, PDF p36"}
  - {name: V_free, value: 92, unit: "L/1.73 m2", source: "12.3 Elimination, PDF p36"}
  - {name: t_half, value: "9 to 16", unit: h, source: "12.3 Elimination, PDF p36", note: "monotherapy, oral 250-1000 mg"}
  - {name: free_fraction, value: "about 10% at 40 mcg/mL to 18.5% at 130 mcg/mL", source: "12.2, PDF p35"}
  - {name: tmax_tablet, value: "4 fasting, 8 fed", unit: h, source: "12.3 Absorption, PDF p35", note: "single dose; sprinkle capsule 3.3 to 4.8 h"}
```

Equivalent oral doses of Depakote products and valproic acid capsules deliver equivalent valproate (12.3).
Total concentration rises less than proportionally with dose (saturable binding); unbound kinetics linear.

## Pharmacodynamics and end points
- Epilepsy: "usually accepted therapeutic range (50 to 100 mcg/mL)" total valproate (2.2, PDF p6, rendered,
  three times in the monotherapy, conversion and adjunctive paragraphs); absence seizures 50 to 100 mcg/mL
  (2.2, PDF p7); 12.2 (PDF p35) gives the epilepsy range as commonly 50 to 100 mcg/mL of total valproate.
- Thrombocytopenia: probability rises at total trough above 110 mcg/mL (women) or 135 mcg/mL (men) (2.2, PDF p6;
  2.4 and 5.x repeat it as at or above 110/135).
- Mania: trials dosed to response with trough 50 to 125 mcg/mL (2.1, PDF p5; 12.2, PDF p35).
- Maximum 60 mg/kg/day. Valproate on ethosuximide (7.2, PDF p25): the same paragraph as the Depakene label
  (single 500 mg, n = 6, half-life +25%, clearance -15%).
- Not an opioid; no MEAC, ke0 or potency ratio.

## Covariates
Age, hepatic and renal disease, enzyme inducers (12.3); no equations. No pregnancy PK statement in 8.1.

## Caveats
The range is a consensus statement ("usually accepted"), not tied to a sampling time beyond "total trough" for
the thrombocytopenia thresholds. The mania trough range (50-125) differs from the epilepsy range.

## Used by stanpumpR
- docs/therapeutic-ranges.md:41 (same line at ca8ce42) - valproate row: "Depakote label: 50-100 mcg/mL".
- docs/therapeutic-ranges.md:240 (context gave :175; found by content at ca8ce42) - Depakote among the labels that
  state a range, "read through search excerpts".
- inst/extdata/therapeuticRanges.csv:4 (label_statement; not in the context file) - "usually accepted therapeutic
  range 50 to 100 mcg/mL total; thrombocytopenia more likely above 110 (women) and 135 (men) mcg/mL (label read
  through a search excerpt)".

## Verification against stanpumpR
- therapeutic-ranges.md:41 and :240, 50-100 mcg/mL: **matches** (2.2, PDF p6; 12.2, PDF p35).
- therapeuticRanges.csv:4 label_statement, 50-100 total and thrombocytopenia above 110/135: **matches** (2.2,
  PDF p6). The CSV's band (50-100 mg/L, Hart 2026) agrees with the label.
- The repo says the label was read "through a search excerpt"; the full label is now archived here and confirms
  the excerpt. The label's mania trough range 50-125 mcg/mL is not mentioned in the repo (its band is the
  epilepsy/AGNP range); not a discrepancy.
