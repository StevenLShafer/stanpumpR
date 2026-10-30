---
file: NJ_label-lidocaine-d5w_018461_2017-02-08.pdf
pmid: null
doi: null
citation: "Baxter Healthcare. Lidocaine Hydrochloride and 5% Dextrose Injection, USP in VIAFLEX Plus plastic container (0.4% and 0.8%) prescribing information, Rev. February 2017. NDA 018461, FDA label 2017-02-08 (Reference ID 4052672; accessdata.fda.gov 018461s058lbl.pdf)."
kind: label
drugs: [lidocaine]
source_read: "text layer (all 10 PDF pages, whole label); page images rendered and read for PDF pp2-4 (Clinical Pharmacology, Warnings) and p8 (Dosage and Administration)"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: matches
---

# FDA label 2017: Lidocaine HCl and 5% Dextrose Injection (Baxter, premixed antiarrhythmic infusion)

## Study
Not a study: old-format (pre-PLR) US label for premixed lidocaine hydrochloride in 5% dextrose, 0.4% (4 mg/mL) and
0.8% (8 mg/mL), for IV use as a ventricular antiarrhythmic. Sections: Description (Table 1 composition), Clinical
Pharmacology (mechanism, pharmacokinetics, specific populations), Indications (ventricular arrhythmias during cardiac
manipulation; life-threatening ventricular arrhythmias, e.g. acute MI), Contraindications, Warnings, Precautions
(drug interactions, pregnancy, lactation, pediatric, geriatric), Adverse Reactions, Overdosage, Dosage and
Administration, How Supplied. No PK study, population or assay is described.

## Model and parameters
No compartmental model. Pharmacokinetic statements (PDF pp2-3, image-confirmed):
- Protein binding is concentration-dependent and falls as concentration rises: 60 to 80% bound at 1 to 4 mcg free
  base/mL; also depends on alpha-1-acid glycoprotein.
- Crosses blood-brain and placental barriers.
- About 90% excreted as metabolites, under 10% unchanged. CYP1A2 and CYP3A4 N-dealkylation gives MEGX and GX (similar
  but less potent actions); main urinary metabolite a conjugate of 4-hydroxy-2,6-dimethylaniline.
- Elimination half-life after IV bolus typically 1.5 to 2.0 h.
- Hepatic dysfunction: half-life may be prolonged two-fold or more. Severe renal dysfunction: clearance halved, GX
  accumulation increased 1.5-fold; mild/moderate renal impairment no effect.
- Drug interactions (PDF p5, text layer): fluvoxamine raises lidocaine AUC 71% and Cmax 22%, lowers MEGX AUC 54% and
  Cmax 65%, cuts clearance 41-60% and prolongs mean half-life by 1 h; cimetidine (weak CYP1A2/3A4 inhibitor) raises
  levels 24-75%; propofol (CYP3A4 inhibitor) and beta-blockers (reduced hepatic blood flow) may raise levels.
- Milk:plasma ratio 0.4-1.1 (PDF p6, text layer).

```yaml
parameters:
  - {name: t_half_elim, value: "1.5 to 2.0", unit: h, source: "Clinical Pharmacology, PDF p3", note: "after IV bolus, typical"}
  - {name: protein_binding, value: "60 to 80", unit: "%", source: "PDF p2", note: "at 1 to 4 mcg free base/mL; falls with concentration"}
  - {name: fraction_excreted_unchanged, value: "< 10", unit: "%", source: "PDF p2"}
  - {name: CNS_toxicity_threshold, value: 6.0, unit: "mcg free base/mL", source: "Clinical Pharmacology PDF p2; Warnings PDF p4", note: "venous plasma; total (not unbound) concentration expressed as base"}
  - {name: bolus_dose, value: "1.0 to 1.5", unit: mg/kg, source: "Dosage and Administration, PDF p8", note: "at 25 to 50 mg/min"}
  - {name: infusion_rate, value: "1 to 4", unit: mg/min, source: "PDF p8", note: "label equates this to 0.020-0.050 mg/kg/min in a 70 kg adult"}
  - {name: infusion_rate_pediatric, value: "20 to 50", unit: mcg/kg/min, source: "PDF p8", note: "after 1 mg/kg bolus; no pediatric studies"}
```

## Pharmacodynamics and end points
- Mechanism: raises the ventricular electrical stimulation threshold in diastole; at usual doses no change in
  contractility, arterial pressure or absolute refractory period.
- Toxic threshold: CNS adverse reactions with venous plasma levels above 6.0 mcg free base/mL (PDF p2 and p4,
  image-confirmed). "Free base" here is the mass basis (lidocaine base, not the hydrochloride), not unbound drug.
- No therapeutic (antiarrhythmic or analgesic) concentration range is stated. Dosing is titrated to response under
  continuous ECG; stop at PR prolongation, QRS widening or new arrhythmia.
- Not an opioid: no MEAC, ke0 or potency ratio.

## Covariates
Hepatic impairment (half-life two-fold or more; lower maintenance rate), severe renal impairment (eGFR < 30
mL/min/1.73 m2: lower maintenance rate), prolonged infusion (reduce rate by about one-half after 24 h), CYP1A2/3A4
inhibitors and inducers, beta-blockers, alpha-1-acid glycoprotein (binding), age 65+ (start low).

Dosing (PDF p8, image-confirmed): bolus 1.0-1.5 mg/kg at 25-50 mg/min (separate lidocaine injection); then infusion
1-4 mg/min. 0.4% solution 15-60 mL/h (0.25-1 mL/min); 0.8% solution 7.5-30 mL/h (0.12-0.5 mL/min). Rarely needed
beyond 24 h. Pediatric: 1 mg/kg bolus then 20-50 mcg/kg/min; repeat the bolus if the infusion is not started within
15 min.

## Caveats
- The label's per-kg equivalent of 1-4 mg/min (0.020 to 0.050 mg/kg/min for 70 kg) does not match the arithmetic
  (derived: 1/70 = 0.014, 4/70 = 0.057 mg/kg/min); the mg/min figures are the primary ones.
- No PK source, population or assay is given; the 6.0 mcg/mL figure is an unreferenced label statement.
- Indication is antiarrhythmic only; the label says nothing about analgesic or perioperative infusions.

## Used by stanpumpR
From the context file: registry source for `inst/extdata/therapeuticRanges.csv:201` (lidocaine row; the row is at
physical line 202 in master 50bb4219).

## Verification against stanpumpR
Checked in `C:\dev\stanpumpRPapers\_build\master_snapshot` (master 50bb4219).

- `inst/extdata/therapeuticRanges.csv:202` (context says 201; line 201 is the dobutamineNeonatal row, so the context's
  line number is one short): source "no therapeutic range: lidocaine in 5% dextrose label (Baxter, rev. 02/2017) and
  Xylocaine label (rev. 10/2024) give toxic thresholds only"; matrix plasma; units mg/L; label_statement "CNS adverse
  reactions above 6.0 mcg free base/mL (venous plasma)". The label (Rev. February 2017) states no therapeutic range and
  gives 6.0 mcg free base/mL in venous plasma for CNS reactions (PDF p2 and p4, image-confirmed); mcg/mL = mg/L.
  The notes' 0.5 to 1.5 mcg/mL band (enhanced-recovery infusions) is not from this label and is not attributed to it;
  the notes correctly say the antiarrhythmic label states no therapeutic range. The Xylocaine label and Foo 2021 are
  other sources, not checked here. **matches**.

Overall: matches.
