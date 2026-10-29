---
file: NJ_label-zonisamides_020789_2020-04-14.pdf
pmid: none
doi: none
citation: "ZONEGRAN (zonisamide) capsules, US prescribing information (old format); NDA 020789 (s036), revised 04/2020 (FDA label dated 2020-04-14); distributed by Amdipharm. https://www.accessdata.fda.gov/drugsatfda_docs/label/2020/020789s036lbl.pdf"
kind: label
drugs: [zonisamide]
source_read: "text layer; read Description, Clinical Pharmacology (Pharmacokinetics: absorption, distribution, metabolism and elimination, specific populations, interactions), Dosage and Administration"
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
---

# Zonegran label 2020: zonisamide capsules prescribing information

## Study
Regulatory label (pre-PLR format). Product: zonisamide 25 and 100 mg capsules, adjunctive therapy for partial
seizures in adults over 16. This label covers the capsule only; it says nothing about an oral suspension.

## Model and parameters
No compartmental model. Values (Clinical Pharmacology, Pharmacokinetics):

```yaml
parameters:
  - {name: Cmax_200_400mg, value: "2 to 5", unit: ug/mL, source: "Absorption", note: "range, normal volunteers"}
  - {name: tmax, value: "2 to 6", unit: h, source: "Absorption", note: "fasting; with food 4-6 h, bioavailability unchanged"}
  - {name: linearity, value: "dose-proportional 200-400 mg; Cmax and AUC rise disproportionately at 800 mg", source: "Absorption", note: "attributed to saturable red-cell binding"}
  - {name: time_to_steady_state, value: 14, unit: days, source: "Absorption"}
  - {name: V_F, value: 1.45, unit: L/kg, source: "Distribution", note: "after 400 mg oral (derived: ~102 L at 70 kg)"}
  - {name: protein_binding, value: "approximately 40", unit: "% bound", source: "Distribution", note: "at 1.0-7.0 ug/mL"}
  - {name: RBC_plasma_ratio, value: 8, unit: fold, source: "Distribution"}
  - {name: CL_F_no_inducer, value: "0.30 to 0.35", unit: mL/min/kg, source: "Metabolism and Elimination", note: "(derived: 1.26-1.47 L/h at 70 kg)"}
  - {name: CL_F_inducer, value: 0.5, unit: mL/min/kg, source: "Metabolism and Elimination", note: "on enzyme-inducing AEDs (derived: 2.1 L/h at 70 kg)"}
  - {name: CL_renal, value: 3.5, unit: mL/min, source: "Metabolism and Elimination", note: "single dose"}
  - {name: CL_RBC, value: 2, unit: mL/min, source: "Metabolism and Elimination"}
  - {name: t_half_plasma, value: 63, unit: h, source: "Metabolism and Elimination", note: approximately}
  - {name: t_half_RBC, value: 105, unit: h, source: "Metabolism and Elimination"}
  - {name: t_half_with_inducers, value: "27 to 38", unit: h, source: "CYP3A4 Induction", note: "400 mg, on phenytoin/carbamazepine/phenobarbital; 46 h on valproate"}
```

Excretion: 62% of radiolabel in urine, 3% faeces by day 10; of excreted dose 35% zonisamide, 15% N-acetyl
zonisamide, 50% SMAP glucuronide (CYP3A4). Renal: renal clearance 3.42, 2.50, 2.23 mL/min across CrCl 70-152,
14.5-59, 10-20 mL/min; AUC +35% at CrCl <20. Age: 300 mg PK similar in young (28 y) and elderly (69 y). No
gender or race information.

Dosing: start 100 mg daily, increase by 100 mg/day steps every ≥2 weeks to 400 mg/day; 100-600 mg/day
effective, no added response above 400 mg/day; once or twice daily.

## Pharmacodynamics and end points
No therapeutic concentration range in the label. Not an opioid: no MEAC, ke0 or potency ratio.

## Covariates
Enzyme-inducing AEDs, renal function (qualitative and summary values).

## Caveats
Single summary values without population detail; non-linearity at 800 mg.

## Used by stanpumpR
- R/drugs_zonisamide.R:33-34 and inst/help/drugs/zonisamide.md:7: capsule and oral suspension are
  label-bridged and share one parameter set.

## Verification against stanpumpR
- **Capsule/suspension bridging (zonisamide.R:33-34; zonisamide.md:7)**: this capsule label does not mention a
  suspension; the bridge is in the Zonisade label (NDA 214273, digest NJ_label-zonisamides_214273_2022-07-18).
  For this document: **not-applicable / cannot verify**. Note the two labels differ in tmax (capsule 2-6 h,
  suspension 0.5-5 h), so sharing one ka (0.671/h) is an approximation for the suspension.
- **Saturable red-cell binding, mild non-linearity (zonisamide.R:31-33)**: label: disproportionate Cmax/AUC at
  800 mg, possibly saturable RBC binding. **Matches.**
- Context: label t½ 63 h and CL/F 0.30-0.35 mL/min/kg (1.26-1.47 L/h at 70 kg) without inducers vs the model's
  CL/F 0.761 L/h and t½ 44 h, and V/F 1.45 L/kg (~102 L) vs 48.1 L. The model's clearance is about half the
  label's and its volume about half; the half-life is shorter than the label's 63 h, as the code header itself
  acknowledges (it attributes this to inducer exposure in Silva's cohort, though the label's inducer t½ is
  27-38 h). Not a transcription issue; flagged as a plausibility gap.
