---
file: DOI_10.2133_dmpk.13.376.pdf
pmid: null
doi: 10.2133/dmpk.13.376
citation: "Hirota N, Hirayama H, Yonemaru M, Yoshikawa O, Naruto H. Pharmacokinetics of nicardipine following intravenous and oral administration to the same human subject. Xenobiotic Metabolism and Disposition (Yakubutsu Dotai) 1998;13(4):376-381."
kind: pk-model
drugs: [nicardipine]
source_read: "text layer (all 6 pages); page images of PDF pages 1, 4 and 5 (Table I, Table II, Figs 4-6) rendered and read"
digest_by: Claude (unattended digest_run.py), 2026-10-30
checked_against: stanpumpR master 7be358cf
verification: matches
open_questions:
  - "[source] The registry (inst/extdata/vasoactiveRegistry.csv:39) says Ayub 2025's Table 1 lists 'Hirota 1998' among intravenous nicardipine studies; that this is the paper meant is assumed from author, year and content, and only Ayub 2025's reference list would confirm it."
  - "[check] The oral-phase rate constants (0.264 and 0.0221 /h single dose; 0.868 and 0.0613 /h from the cited Nakamura data; 0.138 and 0.0414 /h after the last multiple dose; peaks 39.2, 31.6, 25.9 ng/mL; trough 4-9 ng/mL) were read from the text layer only (PDF pages 3-4); confirm on rendered pages 3 and 4 before any use."
---

# Hirota 1998: nicardipine IV bolus, IV infusion and oral SR in one subject

## Study
- One healthy subject: a 32-year-old woman, normal renal and hepatic function, no other medication (PDF p1 / printed p376). **Weight and height not reported.**
- Drug: nicardipine hydrochloride (Yamanouchi). IV solution (Perdipine, 1 mg/mL) and Perdipine LA sustained-release capsule 40 mg "as hydrochloride salt". The IV dose is stated as "1.05 mg nicardipine" without saying salt or base (p1-2).
- Arms, same subject:
  1. IV bolus 1.05 mg in 9 mL saline over 2 min; venous samples 0.12-4.0 h.
  2. Single oral 40 mg LA capsule fasting; samples 0.5-24 h (one week later).
  3. Oral 40 mg LA twice daily for 10.5 days (21 doses); full profiles in the 8th and 20th intervals; samples to 36 h after last dose (two weeks later).
  4. IV infusion 1.27 mg/h for 3 h by syringe pump; samples during and to 28.1 h after stopping.
- Assay: capillary GC-ECD, plasma, sensitivity 1 ng/mL. Free fraction not measured.
- Analysis: MULTI, Damping-Gauss-Newton, weight 1/Cp²; bolus fitted biexponentially (eq. 1), infusion triexponentially (eqs. 2-3); oral data by deconvolution/convolution with SPLINE interpolation. Bioavailability = (AUCpo/AUCiv)·(Dose_iv/Dose_po), trapezoidal AUC (eq. 4).

## Model and parameters
Table I (PDF p4, printed p379), fitting value ± SD; A, B, C in ng/mL, α, β, γ in h⁻¹ (as printed).

| | A (A1) | B (A2) | C | α | β | γ |
|---|---|---|---|---|---|---|
| Bolus | 39.9 ± 4.59 | 2.14 ± 1.05 | n.d. | 2.92 ± 0.386 | 0.148 ± 0.183 | n.d. |
| Infusion | 0.0731 ± 0.00603 | 0.00286 ± 0.00233 | 0.00108 ± 0.000131 | 3.61 ± 0.524 | 0.652 ± 0.331 | 0.0463 ± 0.00672 |

n.d.: γ-phase not detected (assay sensitivity). Text (p5) gives a C of 0.313 ng/mL for the bolus, estimated by least squares with the infusion γ, for the convolution reconstruction in Fig. 6.

No volumes or clearances are reported. Derived values (mine):
- Half-lives: bolus 0.24 h (14 min) and 4.7 h; infusion 0.19 h, 1.06 h and 15.0 h (derived: ln2/rate constant).
- Bolus AUC = A/α + B/β = 28.1 ng·h/mL → CL ≈ 1.05 mg / 28.1 ≈ 37 L/h (derived; dose basis as printed).
- Infusion: the printed infusion coefficients only reproduce Fig. 5 if K0 is in µg/h (1270): predicted concentration at 3 h = 34 ng/mL against the plotted ≈35 ng/mL (derived). On that reading the unit-impulse AUC is 0.048 ng·h/mL per µg, CL ≈ 21 L/h (derived). The printed "ng/mL" unit for the infusion A, B, C is therefore per µg/h of infusion rate, not absolute.
- The bolus/infusion AUC-per-dose ratio, 1.8 (derived), is the twofold bioavailability difference of Table II.

Table II (PDF p5, printed p380), absolute oral bioavailability of Perdipine LA 40 mg:

| IV reference | Single dose | 8th interval | 20th interval |
|---|---|---|---|
| Bolus | 20.9% | 21.7% | 17.4% |
| Infusion | 11.8% | 12.2% | 9.81% |

Oral (text layer, p3-4): two absorption peaks after a single dose (0.5 h and 4 h); terminal rate constants 0.264 and 0.0221 h⁻¹; plateau by the 4th dose; steady-state troughs about 4-9 ng/mL; 8th-interval peak 39.2 ng/mL; 20th-interval peaks 31.6 and 25.9 ng/mL (attributed to food); after the last dose 0.138 and 0.0414 h⁻¹.

```yaml
parameters:
  - {name: A_bolus, value: 39.9, unit: ng/mL, source: "Table I, PDF p4", note: "fit ± 4.59 SD; 1.05 mg bolus"}
  - {name: B_bolus, value: 2.14, unit: ng/mL, source: "Table I, PDF p4", note: "fit ± 1.05 SD"}
  - {name: alpha_bolus, value: 2.92, unit: 1/h, source: "Table I, PDF p4", note: "fit ± 0.386 SD"}
  - {name: beta_bolus, value: 0.148, unit: 1/h, source: "Table I, PDF p4", note: "fit ± 0.183 SD"}
  - {name: A_infusion, value: 0.0731, unit: "ng/mL (printed); per ug/h of infusion rate", source: "Table I, PDF p4", note: "unit reading derived from Fig. 5"}
  - {name: B_infusion, value: 0.00286, unit: "ng/mL (printed)", source: "Table I, PDF p4", note: "as A_infusion"}
  - {name: C_infusion, value: 0.00108, unit: "ng/mL (printed)", source: "Table I, PDF p4", note: "as A_infusion"}
  - {name: alpha_infusion, value: 3.61, unit: 1/h, source: "Table I, PDF p4", note: "1.27 mg/h for 3 h"}
  - {name: beta_infusion, value: 0.652, unit: 1/h, source: "Table I, PDF p4"}
  - {name: gamma_infusion, value: 0.0463, unit: 1/h, source: "Table I, PDF p4", note: "t1/2 15.0 h (derived)"}
  - {name: CL_bolus, value: 37, unit: L/h, source: "derived from Table I", note: "derived: 1.05 mg / (A/alpha + B/beta); weight unknown"}
  - {name: CL_infusion, value: 21, unit: L/h, source: "derived from Table I", note: "derived: K0 read as ug/h; weight unknown"}
  - {name: F_oral_LA_single, value: 0.209, unit: fraction, source: "Table II, PDF p5", note: "vs bolus; 0.118 vs infusion"}
```

## Pharmacodynamics and end points
None measured. No ke0, tPeak, effective concentration or hemodynamic data. Not an opioid; no MEAC or potency ratio. The authors suggest, without data, that slow brain/CSF exchange may explain the γ-phase.

## Covariates
None (one subject).

## Caveats
- n = 1, weight unknown, so no per-kg parameter can be formed.
- The bolus β (0.148 ± 0.183 h⁻¹) has an SD larger than its value. Bolus samples stopped at 4 h (below 1 ng/mL), so the bolus could not show the γ-phase.
- The text says α and β were "roughly equal" for bolus and infusion, but Table I's β differ about fourfold (0.148 vs 0.652 h⁻¹). The abstract's "γ is 1/5 of β" fits neither β alone (γ/β = 0.31 bolus, 0.071 infusion; derived).
- Units of the infusion coefficients are printed as ng/mL but only fit the data per µg/h (derived above).
- The IV dose's salt/base basis is not stated.
- The bioavailability depends twofold on which IV reference is used, which is the paper's main point.

## Used by stanpumpR
- `inst/extdata/vasoactiveRegistry.csv:39` (row `nicardipine__pbpk__ayub2025`, column blocking_issue): names "Hirota 1998" among the intravenous studies in Ayub 2025's Table 1 that a compartmental model would come from.

## Verification against stanpumpR
- **vasoactiveRegistry.csv:39**: the row is about Ayub 2025's PBPK model and cites Hirota 1998 only as an intravenous nicardipine study. This paper is one (1.05 mg bolus and a 1.27 mg/h × 3 h infusion with full profiles), so the claim **matches**. It adds that Hirota could not supply a per-kg compartmental model: one subject of unknown weight, hybrid rate constants and coefficients only, no volumes. The row's `to_obtain` column rightly does not list it. Whether Ayub's "Hirota 1998" is this paper is left as an open question.
- **Comparison with the library model (`R/drugs_nicardipine.R`, Modi 1993, not citing this paper)**: Modi's means are CL 0.920 L/h/kg (lines 79-82), terminal t½ 1.45 h and distribution t½ 6.4 min (header lines 41-44). Hirota's derived clearance, 21-37 L/h, is lower than the model's for any plausible adult weight (e.g. 0.92 × 55 kg = 51 L/h). This lies within the range the header already flags: other studies give 0.31-0.91 L/h/kg, and the model "sits at the fast end" (lines 49-51). Hirota's infusion data also show a slow γ-phase (t½ ≈ 15 h) that the two-compartment library model does not have. That phase is at low concentrations (Fig. 5: ≈1-3 ng/mL beyond 10 h, from about 35 ng/mL at the end of infusion), so the model would under-predict the tail after long infusions. This is evidence only, not a transcription error; one subject cannot settle it.
- No discrepancy in any value the repo takes from this paper; the repo takes none.
