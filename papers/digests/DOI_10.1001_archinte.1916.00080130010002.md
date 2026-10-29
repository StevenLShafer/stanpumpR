---
file: DOI_10.1001_archinte.1916.00080130010002.pdf
pmid: none (1916, predates PubMed)
doi: 10.1001/archinte.1916.00080130010002
citation: "Du Bois D, Du Bois EF. Clinical calorimetry, tenth paper: A formula to estimate the approximate surface area if height and weight be known. Arch Intern Med 1916;17:863-871."
kind: methods
drugs: [gentamicin, pregabalin]
source_read: text layer
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# Du Bois 1916: the height-weight body surface area formula

## Study
Body surface area from height and weight, fitted against areas measured by moulds or by the authors' "linear
formula" (nineteen body measurements) in a small set of subjects (text says nine subjects measured in the laboratory
and "ten cases"; Table 3 lists further subjects, adults and children, by the linear formula). Also corrects the
linear formula's arm factor and gives an alternative thigh measurement (Table 1).

## Model and parameters
General form A = W^(1/a) × H^(1/b) × C kept dimensionally consistent (surface ∝ length²); best values found
graphically a = 2.35, b = 1.38, giving (Summary):

A = W^0.425 × H^0.725 × 71.84, A in cm², W in kg, H in cm; log A = 0.425 log W + 0.725 log H + 1.8564.

Worked example (text): 150 cm, 60 kg → about 1.55 m² (checked: 1.548 m², derived).
Errors claimed: height-weight and linear formulas maximum ±5%, average ±1.5%; Meeh's weight-only formula maximum
+30%, average +15% (Meeh's constant 12.312 should average about 10.5).

```yaml
parameters:
  - {name: C, value: 71.84, unit: "cm2 per kg^0.425 cm^0.725", source: "Summary", note: "= 0.007184 for m2"}
  - {name: weight_exponent, value: 0.425, unit: none, source: "Summary"}
  - {name: height_exponent, value: 0.725, unit: none, source: "Summary"}
```

## Pharmacodynamics and end points
Not applicable (anthropometric formula). No drug data.

## Covariates
Weight and height only.

## Caveats
Very small calibration set; adhesive-plaster measurements about 3.3% high (Table 3 footnote); maximum errors apply
to unusual body shapes (authors), so obesity and infants are poorly represented; height vs supine length differs by
1-2 cm.

## Used by stanpumpR
R/renalFunction.R bsaDuBois() (de-indexing CKD-EPI eGFR; normalising Cockcroft-Gault for pregabalin);
R/drugs_gentamicin.R, R/drugs_pregabalin.R headers; inst/help/drugs/gentamicin.md; inst/help/references.md (full
citation); tests/testthat/test-renalFunction.R.

## Verification against stanpumpR
- **R/renalFunction.R:222-224** `0.007184 * weight^0.425 * height^0.725` (kg, cm → m²): 71.84 cm² = 0.007184 m².
  **Matches**.
- **tests/testthat/test-renalFunction.R:122-124** bsaDuBois(70, 170) = 1.809708 m²: recomputed 1.8097 (derived).
  **Matches**. Line 116 bsaDuBois(20, 110) ≈ 0.78: recomputed 0.775 (derived). **Matches**.
- **R/renalFunction.R:259** de-indexing eGFR × BSA/1.73; **R/drugs_gentamicin.R:19, 33**; **R/drugs_pregabalin.R:35,
  205** (CG × 1.73/BSA). Uses of the formula; consistent with it (the indexing convention itself is not from this paper).
- **inst/help/references.md:101**: Du Bois D, Du Bois EF … Arch Intern Med 1916;17:863-871. Title **matches**;
  page 863 start is consistent with the paper's internal cross-reference to p. 864.
- Applying the formula to small children (gentamicin) extrapolates beyond the calibration set (few children).
