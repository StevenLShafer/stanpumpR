---
file: NJ_web-novo-pi-f96b44.pdf
pmid: ""
doi: ""
citation: "NOVOLOG MIX 70/30 (insulin aspart protamine and insulin aspart) injectable suspension, for subcutaneous use. Prescribing information, Novo Nordisk Inc. Revised 02/2023. https://www.novo-pi.com/novologmix7030.pdf; retrieved 2026-10-21."
kind: label
drugs: [insulin aspart]   # NovoLog Mix 70/30 is a registry row only, not a stanpumpR drug model
source_read: "text layer, all 10 PDF pages: Highlights and contents (p1), 1-2 Indications and Dosage and Administration (p2), 3-7 and 8.1-8.5 (p2-3), 8.6-8.7 Renal and Hepatic Impairment, 10-11, 12.1-12.3 Clinical Pharmacology and 14.1-14.2 (p4), 14.2-17 (p5), patient information and instructions for use (p6-10). PDF p4 (12.2, 12.3, Figures 2-3) confirmed from the rendered page image"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# NovoLog Mix 70/30 prescribing information (Novo Nordisk, revised 02/2023)

## Study
US label for a premixed suspension, 100 units/mL: 70% insulin aspart protamine (crystalline, prolonged absorption)
and 30% soluble insulin aspart; 10 mL vial and 3 mL FlexPen. Indicated to improve glycaemic control in adults with
diabetes; not for diabetic ketoacidosis; fixed ratio, so no separate basal and prandial adjustment (1, PDF p2).
Dosing (2.1-2.2, PDF p2): subcutaneous only (abdomen, buttocks, thigh, upper arm), within 15 min before a meal (after
meal initiation also allowed in type 2 diabetes), typically twice daily; resuspend before use. **Not for intravenous
use and not for insulin infusion pumps**; do not mix with other insulins.

## Model and parameters
12.3 (PDF p4), healthy-subject euglycaemic clamp, n = 24, single 0.3 units/kg SC dose:
- mean Cmax 61.3 ± 20.1 milliunits/L, reached after 85 min (mean); serum insulin back to baseline 16 to 20 h after
  the dose (Figure 3 shows mean ± SEM serum insulin over 24 h).
- relative bioavailability against NovoLog: absorbed to a similar extent (no number given).
- plasma protein binding of insulin aspart below 10%.
- no clearance, volume, half-life or absorption-rate constants are given.

```yaml
parameters:
  - {name: Cmax, value: "61.3 (SD 20.1)", unit: mU/L, source: "12.3, PDF p4", note: "healthy, n = 24, 0.3 units/kg SC, mean"}
  - {name: tmax, value: 85, unit: min, source: "12.3, PDF p4", note: "mean, same study"}
  - {name: time_to_baseline_insulin, value: "16-20", unit: h, source: "12.3, PDF p4"}
  - {name: protein_binding, value: "<10", unit: "%", source: "12.3, PDF p4", note: insulin aspart}
```

## Pharmacodynamics and end points
12.2 (PDF p4), same clamp (glucose infusion rate end point), n = 24, 0.3 units/kg SC: onset of action 10-20 min, mean
± SD time to peak activity 2.7 ± 0.9 h, duration up to 24 h (Figure 2: mean GIR peaks near 9 mg/kg/min). 14.1 (PDF p4):
3-month trials in type 1 (n = 104) and type 2 (n = 187) diabetes against Novolin 70/30 with comparable small HbA1c
changes (Table 4); 14.2 Trials 1-2 add-on to oral agents (PDF p4-5). No concentration-effect parameters.

## Covariates
Renal and hepatic impairment (8.6, 8.7, PDF p4): effect on PK not studied; more frequent monitoring and dose
adjustment advised. Paediatric safety not established (8.4); geriatric experience insufficient (8.5).

## Caveats
Mean descriptors from one small healthy-volunteer clamp study; the curve is the sum of a rapid and a protamine-
retarded component, and the label does not split them.

## Used by stanpumpR
- `inst/extdata/antidiabeticRegistry.csv:45` (generator `data-raw/antidiabeticRegistry.R:196-198`): row
  `insulin_aspart_mix_70_30`, "NovoLog Mix 70/30", SC, PK-TEMPLATE / PK-TEMPLATE, PD-CONSTRAINT / PD-CONSTRAINT, end
  point "clamp GIR", csii FALSE, no stanpumpR drug, note "Aspart protamine release fit required."

## Verification against stanpumpR
- csii FALSE: the label forbids insulin infusion pumps (2.1): **matches**.
- PD end point "clamp GIR" with PD-CONSTRAINT: the label gives onset, time to peak and duration from a GIR clamp but
  no model: **matches**.
- PK-TEMPLATE and "aspart protamine release fit required": the label gives only Cmax, tmax and return to baseline,
  not a two-component absorption model: **matches**.
