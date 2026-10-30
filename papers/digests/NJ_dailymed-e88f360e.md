---
file: NJ_dailymed-e88f360e.pdf
pmid: null
doi: null
citation: "FINTEPLA (fenfluramine) oral solution. US prescribing information, UCB, Inc. DailyMed setid e88f360e-33ad-4cd6-b2de-5ef885857c5d; revised 10/2025 (Medication Guide 4/2025); downloaded 2026-10-21."
kind: label
drugs: []
source_read: "text layer, 47 PDF pages (form-feed chunks = pages). Read Highlights (pp1-2), 2 Dosage and Administration (pp4-6), 3, 8 Use in Specific Populations (pp15-18), 10 Overdosage, 11 Description, 12.1-12.3 Clinical Pharmacology including drug-interaction studies (pp19-22). Rendered pages 20 and 22 to confirm the CL/F, Vz/F and paroxetine values."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# FINTEPLA (fenfluramine) US label, 10/2025

## Study
This is the current US prescribing information for fenfluramine oral solution, 2.2 mg/mL fenfluramine, which
equals 2.5 mg/mL of the hydrochloride (section 11). Indications: seizures of Dravet syndrome (DS) and
Lennox-Gastaut syndrome (LGS) in patients 2 years and older. The label carries a boxed warning for valvular heart
disease and pulmonary arterial hypertension and is distributed under a REMS. The file is the Fintepla label as
named.

**Dosage (2.2-2.5, pp4-6):**
- Start at 0.1 mg/kg twice daily (Table 1).
  - DS: increase as needed to at most 0.35 mg/kg BID, 26 mg/day.
  - LGS: titrate to 0.35 mg/kg BID (26 mg/day) by Day 14.
  - With stiripentol plus clobazam: at most 0.2 mg/kg BID, 17 mg/day.
- Maximum total daily dose is 20 mg (17 mg with stiripentol) when the patient takes a strong CYP1A2 or CYP2D6
  inhibitor or has severe renal impairment (eGFR 15-29 mL/min/1.73 m2).
- Hepatic impairment (Table 2): Child-Pugh A 20 mg (13 mg with stiripentol), B 20 mg, C 17 mg. With stiripentol,
  use in B and C is not recommended.

## Model and parameters
Section 12.3 (PDF pp19-22) gives noncompartmental or summary values. The label presents no population model.
```yaml
parameters:
  - {name: CL/F, value: 24.8, unit: L/h, source: "12.3 Elimination, p20", note: "geometric mean (CV 29%), healthy subjects, oral"}
  - {name: Vz/F, value: 11.9, unit: L/kg, source: "12.3 Distribution, p20", note: "geometric mean (CV 16.5%), healthy subjects"}
  - {name: t1/2, value: 20, unit: h, source: "12.3 Elimination, p20", note: "fenfluramine, healthy subjects"}
  - {name: F (absolute), value: "0.68-0.74", unit: fraction, source: "12.3 Absorption, p20", note: approximately}
  - {name: Tmax (steady state), value: "3-5", unit: h, source: "12.3 Absorption, p20", note: "no food effect on fenfluramine or norfenfluramine"}
  - {name: protein binding, value: 0.50, unit: fraction, source: "12.3 Distribution, p20", note: "in vitro, concentration-independent"}
  - {name: Cmax,ss (DS children), value: 68.0, unit: ng/mL, source: "12.3, p19", note: "geometric mean (CV 41%), 0.7 mg/kg/day up to 26 mg/day"}
  - {name: AUC0-24,ss (DS children), value: 1390, unit: ng*h/mL, source: "12.3, p19", note: "geometric mean (CV 44%)"}
```
**Other PK statements in 12.3:**
- Steady-state Cmax and AUC are slightly more than dose-proportional from 13 to 51.8 mg BID.
- Over 75% is metabolised to norfenfluramine before elimination, mainly by CYP1A2, CYP2B6 and CYP2D6, with
  CYP2C9, CYP2C19 and CYP3A4/5 minor. Both compounds are active.
- Over 90% of the dose is excreted in urine, with fenfluramine plus norfenfluramine under 25% of the total, and
  under 5% in faeces.
- Age (2-50 years), sex and race have no clinically meaningful effect.
- The label gives no norfenfluramine CL, V or t1/2, and no ka.

**Arithmetic check (derived):** t1/2 = 0.693 × Vz/F / (CL/F). With 11.9 L/kg and 24.8 L/h, a 20 h half-life
implies a body weight of about 60 kg; at 70 kg the half-life would be 23 h. The label does not give the healthy
subjects' weight, so the two statements are consistent only for adults of about 60 kg.

**Drug-interaction studies** (12.3, pp21-22; single 0.35 mg/kg doses in healthy volunteers unless stated):

| Co-administered drug | Fenfluramine AUC | Fenfluramine Cmax | Norfenfluramine AUC | Norfenfluramine Cmax |
|---|---|---|---|---|
| Paroxetine 30 mg/day, strong CYP2D6 inhibitor | +81% | +13% | -13% | -29% |
| Fluvoxamine 50 mg/day, strong CYP1A2 inhibitor | +102% | +22% | -22% | -44% |
| Rifampin 600 mg/day | -58% | -40% | -50% | +13% |
| Cannabidiol | +59% | +10% | -22% | -33% |
| Stiripentol + clobazam + valproate, single doses, 0.7 mg/kg | +69% | +18% | -41% (AUC0-72h) | -42% |
| Stiripentol + clobazam at steady state (population PK simulation) | +166% (AUC0-24) | not given | -38% | not given |

**Renal and hepatic impairment:**
- Severe renal impairment (eGFR <30 by MDRD): fenfluramine Cmax +20% and AUC +88%; norfenfluramine +13% and +21%.
- Hepatic impairment, Child-Pugh A, B and C: fenfluramine AUC0-t +95%, +113% and +185%. The combined molar AUC of
  fenfluramine and norfenfluramine rose by +55%, +56% and +82%.

## Pharmacodynamics and end points
- No concentration-effect relation, therapeutic range or MEAC is given.
- Mechanism (12.1): 5-HT2 receptor agonism by fenfluramine and norfenfluramine. 5-HT2B agonism is linked to
  valvulopathy and PAH.
- QT (12.2): no QT prolongation at 4 times the maximum dose in adults.
- Efficacy end points (section 14, not used here) are seizure-frequency reductions.

## Covariates
The label gives no covariate model. It gives categorical dose caps for strong CYP1A2 or CYP2D6 inhibition,
stiripentol plus clobazam, severe renal impairment and hepatic impairment (above).

## Caveats
- The adult CL/F and Vz/F come from healthy subjects. Their study, dose and weight are not given.
- The pediatric exposure is summarised only as steady-state Cmax and AUC.
- The CYP2D6 effect is given as an inhibitor study, not by genotype.
- The structural PK of norfenfluramine is absent.

## Used by stanpumpR
- `inst/extdata/antiseizureRegistry.csv:16` (line confirmed at 8baa733): moiety Fenfluramine, "oral solution
  (Fintepla)", `US_SEIZURE_LABELED`, tier `proposed_unestimated`, dose basis "fenfluramine + norfenfluramine",
  structural model "joint 2C+2C with presystemic metabolite", `pk_only`, no stanpumpR drug.
- The row's notes read: "Label CL/F 24.8 L/h, Vz/F 11.9 L/kg; joint model fixed effects not published. Paroxetine
  DDI parent AUC x1.81 is the CYP2D6 validation scenario, not a genotype coefficient."
- The row is generated by `data-raw/antiseizureRegistry.R:123-127`.

## Verification against stanpumpR
- CL/F 24.8 L/h and Vz/F 11.9 L/kg: both appear as printed on p20, as geometric means in healthy subjects. Note
  that CL/F is absolute (L/h) and Vz/F is per kg (L/kg); the registry note keeps both units correctly. **matches**
- Paroxetine fenfluramine AUC × 1.81: the label (p22) gives +81% AUC0-inf with paroxetine 30 mg/day, which is
  × 1.81. It is an inhibitor study, not a genotype result, as the note says. **matches**
- `US_SEIZURE_LABELED` and "oral solution (Fintepla)": correct. The label covers seizures of DS and LGS from
  2 years and the product is an oral solution. **matches**
- "joint 2C+2C with presystemic metabolite" and "joint model fixed effects not published": the label describes no
  compartmental model. It says over 75% of fenfluramine becomes norfenfluramine, but does not say where.
  The structure cannot be verified from this source, and the label's silence is consistent with the statement
  that fixed effects are unpublished here. **cannot verify**, structure only.
- Dose basis: the label doses as fenfluramine base (2.2 mg/mL, equal to 2.5 mg/mL HCl). The registry's
  "fenfluramine + norfenfluramine" names the two analytes, the same pattern as the clobazam row, and does not
  contradict this. No discrepancy.
