---
file: NJ_label-zonisamides_214273_2022-07-18.pdf
pmid: none
doi: none
citation: "ZONISADE (zonisamide oral suspension) 100 mg/5 mL, US prescribing information, Azurity Pharmaceuticals; NDA 214273 (original approval, s000), approved 07/2022 (FDA label dated 2022-07-18). https://www.accessdata.fda.gov/drugsatfda_docs/label/2022/214273s000lbl.pdf"
kind: label
drugs: [zonisamide]
source_read: text layer; read Highlights, contents, 2.1-2.4 Dosage and Administration, 3, 12.2-12.3 Pharmacokinetics, 14 Clinical Studies (opening)
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
---

# Zonisade label 2022: zonisamide oral suspension prescribing information

## Study
Regulatory label. Product: zonisamide oral suspension 100 mg/5 mL, adjunctive therapy for partial-onset
seizures (adults and paediatric patients 16 y and older). Section 14: efficacy rests on a bioavailability
study comparing the suspension with zonisamide capsules in healthy subjects; clinical-trial data are those of
the capsule. The bioavailability study's results (ratios, n) are not given.

## Model and parameters
No compartmental model. Values (12.3); disposition values are the same text as the Zonegran capsule label:

```yaml
parameters:
  - {name: tmax_suspension, value: "0.5 to 5", unit: h, source: "12.3 Absorption", note: "after 100 mg suspension, normal volunteers"}
  - {name: tmax_fed, value: "3.5 to 7.5", unit: h, source: "12.3 Effect of Food", note: "food has no effect on bioavailability"}
  - {name: linearity, value: "dose-proportional 200-400 mg", source: "12.3"}
  - {name: time_to_steady_state, value: 14, unit: days, source: "12.3"}
  - {name: V_F, value: 1.45, unit: L/kg, source: "12.3 Distribution", note: "after 400 mg"}
  - {name: protein_binding, value: "approximately 40", unit: "% bound", source: "12.3", note: "1.0-7.0 mcg/mL; RBC 8-fold plasma"}
  - {name: CL_F_no_inducer, value: "0.30 to 0.35", unit: mL/min/kg, source: "12.3 Elimination"}
  - {name: CL_F_inducer, value: 0.5, unit: mL/min/kg, source: "12.3 Elimination"}
  - {name: CL_renal, value: 3.5, unit: mL/min, source: "12.3"}
  - {name: t_half_plasma, value: 63, unit: h, source: "12.3 Excretion"}
  - {name: t_half_RBC, value: 105, unit: h, source: "12.3 Excretion"}
  - {name: t_half_with_inducers, value: "27 to 38", unit: h, source: "12.3 CYP3A4 Inducers", note: "46 h on valproate"}
```

Dosing (2.2): 100 mg daily, increase by 100 mg/day every two weeks to 400 mg/day, maximum 600 mg/day; once or
twice daily, with or without food; measure with an accurate device.

## Pharmacodynamics and end points
No therapeutic range; 12.2 notes carbonic anhydrase inhibition (metabolic acidosis). Not an opioid: no MEAC,
ke0 or potency ratio.

## Covariates
Inducers, renal function (as the capsule label).

## Caveats
The bridging study's numerical results are not in the label. The suspension's tmax range (0.5-5 h) starts
earlier than the capsule's (2-6 h, at 200-400 mg), though doses differ (100 mg vs 200-400 mg).

## Used by stanpumpR
- R/drugs_zonisamide.R:33-34 and inst/help/drugs/zonisamide.md:7: capsule and oral suspension are
  label-bridged and share one parameter set. CSV row 113 offers "mg PO" and "mg PO liquid"; the drug function
  declares no oralFormulations, so the liquid takes the default (capsule) input.

## Verification against stanpumpR
- **"label-bridged" (zonisamide.R:33-34; zonisamide.md:7)**: this label bases the suspension's efficacy on a
  bioavailability comparison with capsules and carries the capsule's disposition data. **Matches** in
  substance. Caveat: the label does not state bioequivalence numbers, and its tmax (0.5-5 h) suggests the
  suspension may be absorbed somewhat faster; one shared ka (0.671/h) is an approximation for the rate, while
  the extent (bioavailability) bridge is supported.
- Context: as for the capsule label, label t½ 63 h, CL/F 0.30-0.35 mL/min/kg and V/F 1.45 L/kg differ from the
  model (t½ 44 h, CL/F 0.761 L/h, V/F 48.1 L); the header acknowledges the half-life gap.
