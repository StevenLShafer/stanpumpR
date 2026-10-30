---
file: NJ_label-mannitol_013684_2019-08-05.pdf
pmid: none
doi: none
citation: OSMITROL (mannitol injection), for intravenous use (Baxter). Prescribing information, NDA 013684 (013684s106lbl), FDA label dated 2019-08-05 (Reference ID 4471169). https://www.accessdata.fda.gov/drugsatfda_docs/label/2019/013684s106lbl.pdf
kind: label
drugs: [mannitol]
source_read: text layer (Highlights, 2 Dosage and Administration, 5 Warnings, 8.4-8.6, 10, 12 Clinical Pharmacology incl. 12.3)
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: discrepancy
---

# Osmitrol label 2019: mannitol injection

## Study
Not a study: US prescribing information for Osmitrol 5%, 10%, 15%, 20% mannitol for reduction of
intracranial and intraocular pressure. Section 12.3 gives summary PK without n, population or assay,
except the renal-impairment half-life attributed to "a published study" (not cited).
The repo does not name the product; Osmitrol (the reference listed drug) is the archived edition.

## Model and parameters
No compartmental model. Section 12.3 (label pp. 8-9):

```yaml
parameters:
  - {name: Vd, value: 17, unit: L, source: "12.3 Distribution", note: "approximately, adults; distributes into extracellular space in 20-40 min"}
  - {name: CL, value: "87-109", unit: mL/min, source: "12.3 Elimination", note: "total clearance, normal renal function"}
  - {name: t_half, value: "0.5-2.5", unit: h, source: "12.3 Elimination", note: normal renal function}
  - {name: urine_3h, value: 80, unit: "% dose", source: "12.3 Excretion", note: "approximately, unchanged"}
  - {name: tubular_reabsorption, value: "<10", unit: "%", source: "12.3 Excretion", note: "filtered, not secreted"}
  - {name: t_half_renal_impairment, value: 36, unit: h, source: "12.3 Specific Populations", note: "'about 36 hours', based on serum osmolarity; acute and end-stage renal failure; one published study"}
  - {name: t_half_hemodialysis, value: 6, unit: h, source: "12.3 Specific Populations"}
  - {name: t_half_peritoneal_dialysis, value: 21, unit: h, source: "12.3 Specific Populations"}
```

Dosing (2.2): ICP reduction 0.25 g/kg IV over 30 min, repeatable every 6-8 h; intraocular pressure
1.5-2 g/kg of 20% solution (7.5-10 mL/kg) over 30 min. Normal osmolarity 280-310 mOsmol/L (Table 1
footnote). Only a small amount is metabolised; eliminated unchanged by the kidneys.

## Pharmacodynamics and end points
No concentration-effect data, no ke0. Mechanism (12.1): extracellular osmotic agent drawing intracellular
water out; osmotic diuresis. Rebound ICP rise may occur several hours after infusion (5.3). Monitoring of
serum osmolarity and osmol gap advised (5.4, 5.7). No target osmolality given.

## Covariates
Renal function (qualitative, plus the 36 h half-life); paediatric patients under 2 y at higher risk of
fluid/electrolyte problems. No equations.

## Caveats
- The 36 h figure is a single estimate from an uncited study, derived from serum osmolarity rather than
  mannitol assay, pooled across acute and end-stage renal failure. It is "about", not an upper bound.
- CL, Vd and t½ are ranges without populations.

## Used by stanpumpR
- `R/drugs_mannitol.R:72-75`: assumption that CL1 scales with Cockcroft-Gault creatinine clearance; the
  label's renal-failure half-life is cited as a check ("up to 36 hours").
- `docs/mannitol.md:156-157`: same check; a 70 kg 50 y man with creatinine 10 mg/dL gets CL1 0.006 L/min
  and a terminal half-life of 46 h, "the same order".

## Verification against stanpumpR
- Wording, `R/drugs_mannitol.R:73-74` and `docs/mannitol.md:156`: both say the label gives a half-life
  "of up to 36 hours" in renal impairment. The label says "about 36 hours" (an estimate, not a ceiling),
  based on serum osmolarity. **discrepancy** (wording; makes the model's 46 h look like it exceeds a
  maximum rather than being 28% above a point estimate).
- Order-of-magnitude check: model terminal half-life at CL1 0.006 L/min is 46.0 h (derived here from the
  three-compartment micro-constants, agreeing with docs/mannitol.md). 46 vs about 36 h: same order.
  **matches** as an order-of-magnitude check.
- Not a cited location, noted for audit: at normal renal function the model's CL1 (70 mL/min, Kaneda
  2010) is below the label's 87-109 mL/min, its terminal half-life 4.4 h (derived) is above the label's
  0.5-2.5 h, and its Vss 23.7 L (2.80 + 8.86 + 12.0) is above the label's ~17 L. The code (lines 37-44)
  already explains the choice of 0.07 over 0.04 L/min; the label would favour an even higher clearance.
