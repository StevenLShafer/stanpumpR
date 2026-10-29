---
file: NJ_label-glycopyrrolate_017558_2026-02-25.pdf
pmid: none
doi: none
citation: Glycopyrrolate Injection, USP 0.2 mg/mL single-dose prefilled syringes (Hikma Pharmaceuticals USA), prescribing information, NDA 017558 (S-066), revised 02/2026; FDA label file dated 2026-02-25 (https://www.accessdata.fda.gov/drugsatfda_docs/label/2026/017558s066lbl.pdf). Reference ID 5751011.
kind: label
drugs: [glycopyrrolate]
source_read: text layer, partial (Highlights/TOC; sections 2.1-2.5, 3, 5.6, 8.4-8.6, 10, 11, 12.1-12.3); the 12.3 table re-extracted with pypdf to fix column order
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# Glycopyrrolate injection label 2026 (NDA 017558)

## Study
US prescribing information for a prefilled-syringe product (0.2, 0.4, 0.6, 1 mg at 0.2 mg/mL). Not usable
for doses <0.1 mg (2.1). Glycopyrrolate is the quaternary ammonium **bromide** salt, C19H28BrNO3, MW 398.33;
each mL contains 0.2 mg of glycopyrrolate (11). Completely ionised at physiological pH; log P -1.52.

Dosing (2.2-2.5): premedication 0.004 mg/kg IM 30-60 min before induction (<2 y may need up to 0.009 mg/kg);
intraoperative 0.1 mg IV repeated every 2-3 min (children 0.004 mg/kg, ≤0.1 mg); with neostigmine 0.2 mg per
1 mg neostigmine or 5 mg pyridostigmine; peptic ulcer 0.1 mg q4h 3-4 times daily IV/IM.

## Model and parameters
No compartmental model. Section 12.3, from published studies that used **nonspecific assay methods**:

| Group | t½ (h) | Vss (L/kg) | CL (L/kg/h) | Tmax (min) | Cmax (mcg/L) | AUC 0-8 h (mcg/L·h) |
|---|---|---|---|---|---|---|
| 6 mcg/kg IV | 0.83 ± 0.27 | 0.42 ± 0.22 | 0.54 ± 0.14 | – | – | 8.64 ± 1.49 |
| 8 mcg/kg IM | – | – | – | 27.48 ± 6.12 | 3.47 ± 1.48 | 6.64 ± 2.33 |

(Mean ± SD; the text layer misplaces the IV AUC; column order confirmed with pypdf extraction.)
Other 12.3 values: IM t½ 0.55-1.25 h; after 0.2 mg IV radiolabel, 85% of recovered dose in urine by 48 h,
some in bile; >80% of IM dose recovered unchanged in urine and bile, half within 3 h. Paediatric IV
(5 mcg/kg) t½ 21.6-130.0 min (infants), 19.2-99.2 min (children). Uremic patients (renal transplant): t½
46.8 vs 18.6 min, AUC 10.6 vs 3.73 h·mcg/L, CL 0.43 vs 1.14 L/h/kg, 3-h urine excretion 0.7% vs 50%.
Metabolism in humans not studied.

```yaml
parameters:
  - {name: t_half_IV, value: 0.83, unit: h, source: "12.3 table", note: "mean ± 0.27, nonspecific assay"}
  - {name: Vss, value: 0.42, unit: L/kg, source: "12.3 table", note: "mean ± 0.22"}
  - {name: CL, value: 0.54, unit: L/kg/h, source: "12.3 table", note: "mean ± 0.14"}
  - {name: Tmax_IM, value: 27.48, unit: min, source: "12.3 table", note: 8 mcg/kg IM}
  - {name: Cmax_IM, value: 3.47, unit: mcg/L, source: "12.3 table", note: 8 mcg/kg IM}
```

## Pharmacodynamics and end points
Section 12.2: onset of action within 1 minute after IV; after IM, onset 15-30 min and peak effect about
30-45 min; vagal blocking effect lasts 2-3 h, antisialagogue effect up to 7 h. End point not specified
(clinical antimuscarinic effects). No concentration-effect data, ke0 or EC50. Limited blood-brain barrier
passage (quaternary). Not an opioid; no MEAC/potency ratio.

## Covariates
Renal failure markedly reduces elimination (5.6, 8.6, 12.3); age (paediatric t½ ranges). No equations.

## Caveats
PK from old studies with nonspecific assays; mixed time units (h and min) between sections; healthy
half-life quoted both as 0.83 h (table) and 18.6 min (renal study control group).

## Used by stanpumpR
- `R/drugs_glycopyrrolate.R:23`, `inst/help/drugs/glycopyrrolate.md:7` — dose labelled as glycopyrrolate
  bromide (MW 398.33) versus the cation in the model.
- `R/drugs_glycopyrrolate.R:33`, `inst/help/drugs/glycopyrrolate.md:15` — label onset judged insufficient to
  identify a ke0; plasma only.

## Verification against stanpumpR
- `R/drugs_glycopyrrolate.R:23-25`, `glycopyrrolate.md:7`: injection labelled as the bromide, MW 398.33.
  Label section 11: MW 398.33, formula with Br; dose stated as mg glycopyrrolate (the salt). **matches**.
  The cation MW 318.43 is not in the label; derived 398.33 − 79.90 (Br) = 318.43, ratio 0.7994. **matches**.
- `R/drugs_glycopyrrolate.R:33`, `glycopyrrolate.md:15`: label onset cannot identify an equilibration
  constant. Label gives only onset (<1 min IV) and IM peak/duration on unspecified clinical end points, with no
  concentrations. The judgement is **supported**.
- Context (not a listed location): the label's PK (Vss 0.42 L/kg ≈ 29 L, t½ 0.83 h at 70 kg, derived)
  is far from the Bartels model the code uses (Vss 101.8 L, terminal 7.2 h); `glycopyrrolate.md:19` already
  notes an older study with a different assay reported a much shorter half-life. Consistent.
