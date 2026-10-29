---
file: NJ_label-coumadin_009218_2017-05-24.pdf
pmid: null
doi: null
citation: "COUMADIN (warfarin sodium) tablets, for oral use; COUMADIN (warfarin sodium) for injection, for intravenous use. FDA prescribing information, NDA 009218 (s017), Bristol-Myers Squibb; revised 5/2017 (Reference ID 4101432). https://www.accessdata.fda.gov/drugsatfda_docs/label/2017/009218s017lbl.pdf"
kind: label
drugs: [warfarin]
source_read: "text layer: Highlights, contents, sections 1-3, 8.2-8.7, 10-13, start of 14 (Medication Guide and remaining clinical-trial tables skimmed by keyword search only); rendered page images of PDF p4-5 (section 2.2-2.3) and p22-23 (12.2-12.5) read to confirm numbers"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: matches
---

# Coumadin label 2017: warfarin prescribing information (NDA 009218)

## Study
FDA label, not a study. Revised 5/2017; initial US approval 1954. Racemic warfarin sodium (R- and S-enantiomers),
vitamin K antagonist. Boxed warning: bleeding risk; monitor INR in all patients. Forms: scored tablets 1, 2, 2-1/2,
3, 4, 5, 6, 7-1/2, 10 mg; 5 mg lyophilized vial for IV use (reconstituted with 2.7 mL to 2.5 mL of 2 mg/mL; IV dose
equals oral dose, slow bolus over 1-2 min; section 2.7).

### Target INR (section 2.2, PDF p4-5; confirmed on rendered pages)
- INR >4.0 gives no added benefit in most patients and more bleeding.
- VTE (DVT/PE): target 2.5 (range 2.0-3.0), all durations; 3 months if transient risk factor; ≥3 months if
  unprovoked; long-term after two unprovoked episodes.
- Non-valvular AF: target 2.5 (2.0-3.0).
- Bileaflet mechanical or Medtronic Hall tilting disk valve, aortic, sinus rhythm, no LA enlargement: 2.5 (2.0-3.0).
- Tilting disk and bileaflet mechanical valves, mitral: 3.0 (2.5-3.5).
- Caged ball or caged disk valves: 3.0 (2.5-3.5).
- Bioprosthetic mitral valve: 2.5 (2.0-3.0) for first 3 months; 2.5 (2.0-3.0) if additional risk factors.
- High-risk post-MI: INR 2.0-3.0 plus aspirin ≤100 mg/day for ≥3 months.
- Other (valvular AF, mitral stenosis, recurrent embolism): 2.0-3.0 may be used.

### Dosing (2.3-2.4, PDF p5-6; Table 1 from text layer)
- Genotype unknown: initial 2-5 mg once daily; typical maintenance 2-10 mg once daily. Loading doses not recommended.
- Table 1 (expected maintenance daily dose by VKORC1 −1639G>A × CYP2C9): GG: *1/*1 5-7, *1/*2 5-7, *1/*3 3-4,
  *2/*2 3-4, *2/*3 3-4, *3/*3 0.5-2 mg; AG: 5-7, 3-4, 3-4, 3-4, 0.5-2, 0.5-2 mg; AA: 3-4, 3-4, 0.5-2, 0.5-2,
  0.5-2, 0.5-2 mg (text layer only).
- INR daily after the first dose until stable, then every 1-4 weeks. Label calls warfarin a narrow therapeutic
  range (index) drug. Whole-blood clotting and bleeding times are not effective monitors.
- Heparin overlap 4-5 days and until INR therapeutic (2.9).

## Model and parameters
No compartmental model. Label PK statements (section 12.3, PDF p22-23, confirmed on rendered pages):
```yaml
parameters:
  - {name: F_oral, value: "essentially complete", unit: "", source: "12.3, PDF p22", note: "qualitative"}
  - {name: tmax, value: "within 4", unit: h, source: "12.3, PDF p22", note: "peak concentration generally within first 4 h"}
  - {name: Vd, value: 0.14, unit: L/kg, source: "12.3, PDF p22", note: "apparent volume, 'about'"}
  - {name: distribution_phase, value: "6-12", unit: h, source: "12.3, PDF p22", note: "after rapid IV or oral aqueous solution"}
  - {name: protein_binding, value: 99, unit: "%", source: "12.3, PDF p22", note: "approximately; plasma proteins"}
  - {name: t_half_terminal_single_dose, value: 1, unit: week, source: "12.3, PDF p22", note: approximate}
  - {name: t_half_effective, value: 40, unit: h, source: "12.3, PDF p22", note: "mean; range 20-60 h"}
  - {name: t_half_R, value: "37-89", unit: h, source: "12.3, PDF p23", note: range}
  - {name: t_half_S, value: "21-43", unit: h, source: "12.3, PDF p23", note: range}
  - {name: CL_R_over_CL_S, value: 0.5, unit: ratio, source: "12.3, PDF p23", note: "'generally half'; volumes similar"}
  - {name: urinary_recovery, value: "up to 92", unit: "%", source: "12.3, PDF p23", note: "radiolabel, oral dose, as metabolites"}
```
Metabolism by CYP2C9 (principal; S-warfarin 7-hydroxylation), 2C19, 2C8, 2C18, 1A2, 3A4 and by reductases. No
absolute clearance value is given. No plasma warfarin concentration range is given; therapy is defined by INR only.

## Pharmacodynamics and end points
End point: INR (prothrombin time). Section 12.2 (PDF p21-22): effect usually within 24 h; peak anticoagulant effect
may be delayed 72-96 h; duration of a single dose 2-5 days. Clotting-factor half-lives (PDF p22, confirmed): factor
II 60 h, VII 4-6 h, IX 24 h, X 48-72 h; protein C ~8 h, protein S ~30 h. S-enantiomer has 2-5 times the
anticoagulant activity of R (12.3). No EC50, ke0 or concentration-effect model. Not an opioid; no MEAC or potency
ratio applies.

## Covariates
Qualitative only: age (≥60 y greater INR response; possibly slightly lower R-warfarin clearance), Asian ancestry
(151 Chinese outpatients needed 3.3 ± 1.4 mg/day, mean ± SD, for INR 2-2.5; PDF p23), CYP2C9 and VKORC1 genotype
(Table 1; CYP2C9*2 and *3 frequencies in Caucasians ~11% and 7%), hepatic impairment potentiates, renal impairment
needs no dose change but more frequent INR. Many drug, food and herbal interactions (section 7).

## Caveats
- The label states INR targets, not drug concentrations; a "therapeutic range" for warfarin in this label is an INR range.
- Genotype dose ranges (Table 1) are derived from multiple published studies, not from a single model.
- Sections 6, 7, 14 tables and the Medication Guide were not read in full (keyword search only); nothing stanpumpR
  cites depends on them.

## Used by stanpumpR
- `inst/extdata/therapeuticRanges.csv:156` — registry source ("Coumadin label (rev. 5/2017), section 2.2") for the
  warfarin INR row.

## Verification against stanpumpR
- `inst/extdata/therapeuticRanges.csv:156`: analyte international normalized ratio, matrix plasma, lower 2, upper 3,
  typical 2.5, units INR, `band_status` drawn; `label_statement` "target INR 2.5 (range 2.0-3.0); caged-ball or
  caged-disk valves 3.0 (2.5-3.5)"; notes say 2.5-3.5 for some mechanical valves. Label section 2.2 (PDF p4-5,
  rendered): target 2.5 (2.0-3.0) for VTE, non-valvular AF, aortic bileaflet/tilting-disk valves and bioprosthetic
  mitral; 3.0 (2.5-3.5) for mitral tilting-disk/bileaflet and caged ball/disk valves. Values, units and section
  number transcribed correctly; revision date 5/2017 matches. **matches**.
- Matrix "plasma": the label does not name a specimen matrix for the INR; citrated plasma is the standard specimen,
  so this is a reasonable registry label, not a label fact.
- The row's Holbrook 2012 co-citation is outside this card's scope.
